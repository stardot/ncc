/*
 * cse.c: Common sub-expression elimination
 * Copyright (C) Acorn Computers Ltd., 1988.
 * Copyright 1991-1997 Advanced Risc Machines Limited. All rights reserved
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$ Codemist 133
 * Checkin $Date$
 * Revising $Author$
 */

#ifdef __STDC__
#  include <string.h>
#  include <time.h>
#  include <stddef.h>
#else
#  include <strings.h>
#  include "time.h"
#  include "stddef.h"
#endif

#include "globals.h"
#include "cse.h"
#include "jopcode.h"
#include "store.h"
#include "regalloc.h"
#include "cg.h"
#include "flowgraf.h"
#include "mcdep.h"     /* usrdbg, DBG_xxx */
#include "builtin.h"   /* for te_xxx */
#include "errors.h"
#include "aeops.h"
#include "cseguts.h"

typedef struct CSE CSE;
typedef struct CSERef CSERef;

struct CSERef {
    CSERef *cdr;
    union {
        BlockHead *b;
        ExprnUse *ex;
    } ref;
};

#define refuse_(p) ((p)->ref.ex)
#define refblock_(p) ((p)->ref.b)
#define mk_CSERef(a,b) ((CSERef *)CSEList2((a), (b)))
#define CSERef_DiscardOne(p) ((CSERef *)discard2((List *)(p)))

struct CSEDef {
    CSEDef *cdr;
    BlockHead *block;
    Exprn *exprn;
    CSERef *refs;
    Icode *icode;
    VRegSetP uses;
    CSEDef *subdefs, *nextsub;
    CSEDef *super;
    Binder *binders[1];
    int32 mask;           /* only for compares */
};

#define defbinder_(x,n) ((x)->binders[n])
#define defmask_(x) ((x)->mask)
#define defblock_(x) ((x)->block)
#define defex_(x) ((x)->exprn)
#define defrefs_(x) ((x)->refs)
#define defic_(x) ((x)->icode)
#define defuses_(x) ((x)->uses)
#define defsub_(x) ((x)->subdefs)
#define defsuper_(x) ((x)->super)
#define defnextsub_(x) ((x)->nextsub)
#define CSEDef_New(n) ((CSEDef *)CSEAlloc(offsetof(CSEDef, binders)+(n)*sizeof(Binder *)))

struct CSE {
    CSE *cdr;
    Exprn *exprn;
    CSEDef *def;
};
#define cseex_(x) ((x)->exprn)
#define csedef_(x) ((x)->def)

static CSE *cselist;
static CSEDef *localcsedefs;

static BindList *csespillbinders;

typedef struct LoopList LoopList;
struct LoopList
{   LoopList *cdr;
    BlockList *members;             /* list of basic blocks inside       */
    BlockHead *preheader;           /* where to put found invariants     */
    BlockHead *header;
    BlockHead *tail;
};
#define ll_mem_(p) ((p)->members)
#define ll_prehdr_(p) ((p)->preheader)
#define ll_hdr_(p) ((p)->header)
#define ll_tail_(p) ((p)->tail)

static LoopList *all_loops;
static VRegSetP loopmembers;

#define mk_CSEBlockList(a, b) ((BlockList *)CSEList2((BlockList *)a, (BlockHead *)b))

struct CSEUseList {
    CSEUseList *cdr;
    CSEDef *def;
    ExprnUse *use;
};
#define ul_def_(p) ((p)->def)
#define ul_use_(p) ((p)->use)
#define mk_CSEUseList(a,b,c) ((CSEUseList *)CSEList3((a),(b),(c)))

static unsigned32 nsets, newsets, setbytes;
static unsigned32 maxsets, maxbytes;
static unsigned32 cse_count, cse_refs;

VRegSetAllocRec cseallocrec; /* = {CSEAllocType, &nsets, &newsets, &setbytes};*/

SetSPList *setsplist;

static bool BlockList_Member(BlockHead *b, BlockList *list) {
    return generic_member((IPtr)b, (List *)list);
}

#ifdef ENABLE_CSE

#define printset(s) cseset_map(s, ps, NULL)

static void ps(int32 n, VoidStar arg)
{
    IGNORE(arg);
    cc_msg(" %ld", (long)n);
}

void cse_printset(VRegSetP s) { cseset_map(s, ps, NULL); }

#else

#define cse_printset(s) /* nothing */

#endif

static J_OPCODE CSEaccessOp(VRegnum r, J_OPCODE model)
{   /* 'model' is either J_LDRV or J_STRV.                              */
    /* @@@ This routine needs to exploit J_ALIGN4V in cg.c...           */
    J_OPCODE op;
    switch (vregsort(r))
    {   case FLTREG: op = J_LDRFV|J_ALIGN4; break;
/* See jopcode.h comment about J_ALIGN8 with alignof_double=4.          */
        case DBLREG: op = J_LDRDV|J_ALIGN8; break;
#ifdef never
        missing:     op = J_LDRLV|J_ALIGN8; break;
#endif
        default:     op = J_LDRV|J_ALIGN4; break;
    }
    return model == J_LDRV ? op : J_LDtoST(op);
}

static RegSort DestRegSort(J_OPCODE op)
{   /* Given 'op' return the 'regsort' suitable for the 'r1'            */
    /* field of 'op'.  If 'op' is a j_ischeck() (or J_CMP?) then        */
    /* 'GAP' is returned.                                               */
    /* This routine is the only user of deprecated 'floatiness_()'.     */
    /* It suggests that a new 'J_destregsort() could be in jopcode.h    */
    if (j_is_check(op) || is_compare(op))
        return GAP;
    else {
        J_OPCODE x = op & J_TABLE_BITS;
        switch (floatiness_(op))
        {   case _J_FLOATING: return x==J_FIXFR ? INTREG:FLTREG;
            case _J_DOUBLE:   return x==J_FIXDR ? INTREG:DBLREG;
            default:          return INTREG;
        }
    }
}

static void ReplaceWithLoad(Icode *target, VRegnum r1, Binder *binder)
{
#ifdef RANGECHECK_SUPPORTED
    if (binder == NULL)
        target->op = J_NOOP;
    else
#endif
    {   target->op = CSEaccessOp(r1, J_LDRV);
        target->r2.r = GAP;
        target->r3.b  = binder;
    }
}

static CSEUseList *ReplaceIcode(CSEUseList *deferred, ExprnUse *ref, CSEDef *def)
{
    BlockHead *b = ref->block;
    if (!IsRealIcode(ref))
      blk_defs2_(b) = def;
    else {
      Icode *target = &useicode_(ref);
      J_OPCODE op = target->op;
      VRegnum r1 = target->r1.r;
      if (debugging(DEBUG_CSE)) {
          print_jopcode_1(target);
          cc_msg("     %ld/%ld\n", (long)blklabname_(b), (long)icoden_(ref));
      }
      if (def != NULL) {
          Exprn *ex = defex_(def);
#ifdef TARGET_ALLOWS_COMPARE_CSES
          if (is_compare(exop_(ex))) {

              defmask_(def) = (defmask_(def) & Q_UBIT) | Q_UKN;
              target->op = J_NOOP;
          } else
#endif
          if (pseudo_reads_r2(exop_(ex))) {
              VRegnum r = bindxx_(defbinder_(def, 0));
              if (pseudo_reads_r1(op))
                  target->r1.r = r;
              else {
                  target->op = exop_(ex);
                  target->r2.r = r;
                  note_slave(target->r1.r, r);
              }
              target->r3.i = e1k_(ex);
          } else if (nvals_(ref) == 0)
              ReplaceWithLoad(target, r1, defbinder_(def, valno_(ref)));
          else
              deferred = mk_CSEUseList(deferred, def, ref);
      }
    }
    cse_refs++;
    return deferred;
}

static Binder *AddCSEBinder(J_OPCODE op, BindList **bl, VRegnum r)
{
    Binder *bnew;
/* /* The following line is a hack to fix things up now that vregsort() */
/* faults r == GAP.  Fix properly soon.                                 */
/* It is manifested by f(n) { if (n>1023) g(); if (n>1023) h(); }       */
    bnew = gentempvarofsort(r == GAP ? INTREG : vregsort(r));

    /* Two lists of binders introduced by CSE are required: one for computing
       stack frame offsets (to add to lists present before CSE in block heads
       and SETSPENVs), and one to determine binder spill order.  These are not
       the same: binders introduced for ADCONs and the like need to be in the
       second list but not the first (since they are simply discarded if they
       spill: if they were in the first list, they would consume stack to no
       purpose).
     */
    csespillbinders = mkBindList(csespillbinders, bnew);
    if (!pseudo_reads_r2(op) && !is_compare(op)) {
        *bl = mkBindList(*bl, bnew);
        bindbl_(bnew) = *bl;
    } else
        bindbl_(bnew) = NULL;
    bindstg_(bnew) |= b_bindaddrlist;
    return bnew;
}

static CSEDef *mk_CSEDef(Exprn *ex, BlockHead *b)
{
    CSEDef *def;
    if (is_calln(ex))
        def = CSEDef_New(exnres_(ex));
#ifdef TARGET_ALLOWS_COMPARE_CSES
    else if (is_compare(exop_(ex)))
        def = CSENew(CSEDef);
#endif
    else
        def = CSEDef_New(1);
    defblock_(def) = b; defic_(def) = NULL; defex_(def) = ex;
    defrefs_(def) = NULL; defuses_(def) = NULL; defbinder_(def, 0) = NULL;
    defsub_(def) = defnextsub_(def) = defsuper_(def) = NULL;
    return def;
}

static CSEDef *AddToCSEList(Exprn *ex, BlockHead *b, bool mustbenew)
{
    CSE *cse;
    CSEDef *def;
    for (cse = cselist ; cse != NULL ; cse = cdr_(cse))
        if (ex == cseex_(cse)) break;
    if (cse == NULL)
        cse = cselist = (CSE *)CSEList3(cselist, ex, NULL);
    if (!mustbenew)
        for (def = csedef_(cse); def != NULL; def = cdr_(def))
            if (defblock_(def) == b) return def;

    def = mk_CSEDef(ex, b);
    cdr_(def) = csedef_(cse); csedef_(cse) = def;
    return def;
}

static bool KillExprRef_i(CSEDef *def, Icode *ic)
{   ptrdiff_t icoden = ic - blkcode_(cse_currentblock);
    CSERef **refp;
    for ( ; def != NULL; def = cdr_(def))
        if (defblock_(def) == cse_currentblock)
            for (refp = &defrefs_(def); *refp != NULL; refp = &cdr_(*refp))
            {   ExprnUse *use = refuse_(*refp);
                if (use->block == cse_currentblock && icoden_(use) == icoden)
                {   *refp = cdr_(*refp);
                    return YES;
                }
            }
    return NO;
}

bool cse_KillExprRef(ExSet *s, Icode *ic) {
  /* remove any local or global CSE references to the instruction ic */
  for (; s != NULL; s = cdr_(s)) {
    Exprn *ex = s->exprn;
    CSE *cse;

    if (KillExprRef_i(localcsedefs, ic)) return YES;
    for (cse = cselist; cse != NULL; cse = cdr_(cse))
        if (cseex_(cse) == ex && KillExprRef_i(csedef_(cse), ic))
            return YES;
  }
  return NO;
}


static bool WorthReplacement(Exprn *ex)
{
/* Same rules for what's worth making into a CSE whether for local or   */
/* or non-local use.                                                    */

/* The following line is really unnecessary (csescan doesn't create     */
/* Exprns for these things).                                            */
    if (exop_(ex) == J_MOVDIR ||
        exop_(ex) == J_MOVLIR ||
        exop_(ex) == J_MOVFIR) return NO;
#ifdef TARGET_HAS_CONST_R_ZERO
/* In this case it is SILLY to lift zero as a constant! */
    if (exop_(ex) == J_MOVK && e1k_(ex) == 0) return NO;
#endif
    if (extype_(ex) == E_LOAD) {
      Location *loc = exloc_(ex);
      if (!ispublic(loc)) return NO;
      if (loctype_(loc) & LOC_anyVAR)
           return !(bindstg_(locbind_(loc)) & b_globalregvar);
      if (exop_(locbase_(loc)) == J_ADCONV) return NO;
      return YES;
    }
    return exop_(ex) != J_STRING && exop_(ex) != J_ADCONV;
    /* J_ADCONF and J_ADCOND also used to be excluded here. There seems */
    /* no good reason - after all, CSEs for them vanish if spilt, and   */
    /* J_ADCON (which one would have thought analogous) is included.    */
    /* One might question the inclusion of J_ADCONV, too, but bear in   */
    /* mind that, for local CSEs, it may be eliminated by transformation*/
    /* of opK to opVK.                                                  */
}

bool cse_AddLocalCSE(Exprn *node, int valno, int nvals, BlockHead *b)
{
    ExprnUse *def = exuses_(node);
    Icode *deficode;
    CSEDef *csedef;
    CSERef *ref;

    if (!WorthReplacement(node)) return NO;
    ref = CSENew(CSERef);

    if (def == NULL)
        syserr(syserr_addlocalcse, exid_(node));
    /* If  node  is not killed between here and the start of the block,
       then the local CSE may be later subsumed by a non-local CSE.
       Otherwise, we must take care to avoid that happening.
       In the latter case, the CSEDef is attached to the list localcsedefs;
       in the former, to a CSE in cselist
       I believe subsuming an ADCON or MOVK is not helpful, thanks to
       slave_list, and may be harmful if the non-local CSE must be spilt
       but a register could be allocated for the local one. (Hence the
       !pseudo_reads_r2() test)
     */
    deficode = &useicode_(def);
    if (!pseudo_reads_r2(deficode->op) && !cse_KilledInBlock(exid_(node))) {
        csedef = AddToCSEList(node, b, NO);
        cseset_insert(blklabname_(b), defuses_(csedef), NULL);
        if (debugging(DEBUG_CSE) && CSEDebugLevel(1))
            cc_msg("(subsumable) ");
    } else {
        for (csedef = localcsedefs ; csedef != NULL ; csedef = cdr_(csedef))
            if (defic_(csedef) == deficode) break;
        if (csedef == NULL) {
            csedef = mk_CSEDef(node, b);
            setflag_(def, U_LOCALCSE);
            cdr_(csedef) = localcsedefs;
            localcsedefs = csedef;
        }
    }
    defic_(csedef) = deficode;
    cdr_(ref) = defrefs_(csedef); defrefs_(csedef) = ref;
    refuse_(ref) = ExprnUse_New(NULL, 0, valno);
    setnvals_(refuse_(ref), nvals);

    if (debugging(DEBUG_CSE) && CSEDebugLevel(1))
        cc_msg("-- local CSE reference [%ld]\n", exid_(node));
    return YES;
}

static VRegSetP ExprnsWantedBy(LabelNumber *q, bool allpaths)
{
    if (is_exit_label(q))
        return NULL;
    else {
        CSEBlockHead *p = blkcse_(q->block);
        VRegSetP s = cseset_copy(allpaths ? p->wantedonallpaths :
                                            p->wantedlater);
        if (p->killedinverted)
            cseset_intersection(s, p->killed);
        else
            cseset_difference(s, p->killed);
        return cseset_union(s, p->wanted);
    }
}

static void ExprnsReaching(BlockHead *p)
{
    VRegSetP s1, s2;
    if (blkflags_(p) & BLKSWITCH) {
        LabelNumber **v = blktable_(p);
        int32 i, n = blktabsize_(p);
        s1 = ExprnsWantedBy(v[0], NO);
        s2 = ExprnsWantedBy(v[0], YES);
        for (i = 1 ; i < n ; i++) {
            VRegSetP s = ExprnsWantedBy(v[i], NO);
            cseset_union(s1, s);
            cseset_discard(s);
            s = ExprnsWantedBy(v[i], YES);
            cseset_intersection(s2, s);
            cseset_discard(s);
        }
    } else {
        s1 = ExprnsWantedBy(blknext_(p), NO);
        s2 = ExprnsWantedBy(blknext_(p), YES);
        if (blkflags_(p) & BLK2EXIT) {
            VRegSetP s = ExprnsWantedBy(blknext1_(p), NO);
            cseset_union(s1, s);
            cseset_discard(s);
            s = ExprnsWantedBy(blknext1_(p), YES);
            cseset_intersection(s2, s);
            cseset_discard(s);
        }
    }
    blk_wantedlater_(p) = s1;
    blk_wantedonallpaths_(p) = s2;
}

static bool ContainsLoadr(Exprn *p)
{ /* A temporary bodge.  Expressions containing a register whose value is
   * unknown are valid local CSEs, but not valid outside their block (because
   * before the LOADR was created, earlier loads to the register would have
   * passed unknown).  The earlier treatment of registers (where each was a
   * Location) got this right, but had undesirable space & time costs.  More
   * thought needed.  Here, we simply prevent anything with a leaf which is a
   * LOADR from becoming a candidate CSE.
   */
    if (p == NULL) return NO;
    switch (extype_(p)) {
    case E_LOADR:
        return YES;
    case E_TERNARY:
        if (ContainsLoadr(e3_(p))) return YES;
    case E_BINARY:
        if (ContainsLoadr(e2_(p))) return YES;
    case E_BINARYK:
    case E_UNARY:
        return ContainsLoadr(e1_(p));
    case E_LOAD:
        {   Location *loc = exloc_(p);
            if (loctype_(loc) & LOC_anyVAR) return NO;
            return ContainsLoadr(locbase_(loc));
        }
    case E_CALL:
        {   int32 i;
            for (i = 0 ; i < exnargs_(p) ; i++)
                if (ContainsLoadr(exarg_(p, i))) return YES;
        }
        return NO;
    default:
        return NO;
    }
}

static void AddCSE(int32 n, void *arg)
{
    Exprn *ex = exprn_(n);
    if ( !ContainsLoadr(ex) && WorthReplacement(ex))
        AddToCSEList(ex, (BlockHead *)arg, YES);
}

CSEBlockHead *CSEBlockHead_New(void)
{
    CSEBlockHead *q = CSENew(CSEBlockHead);
    q->available = NULL; q->wanted = NULL;
    q->wantedlater = NULL; q->wantedonallpaths = NULL;
    q->killed = NULL;
    q->defs = NULL;
    q->killedinverted = NO;
    q->reached = NO;
    q->loopempty = 0;
    q->scanned = NO;
    q->locvals = NULL;
    q->cmp = NULL;
    q->ternaryr = GAP;
    q->defs2 = NULL;
    q->refs = NULL;
    return q;
}

#define dominates(p, q) cseset_member(blklabname_(p), blk_dominators_(q))

static bool PruneSuccessors(LabelNumber *lab, VRegSetP s)
{
    if (is_exit_label(lab)) return NO;

    {   BlockHead *p = lab->block;
        VRegSetP old = blk_dominators_(p);
        VRegSetP s1 = cseset_copy(s);
        bool oldreached = blk_reached_(p), same;
        cseset_intersection(s1, old);
        cseset_insert(lab_name_(lab), s1, NULL);
        same = cseset_equal(s1, old);
        cseset_discard(old);
        blk_dominators_(p) = s1;
        blk_reached_(p) = YES;
        return !same || !oldreached;
    }
}

bool cse_AddPredecessor(LabelNumber *lab, BlockHead *p)
{
    if (!is_exit_label(lab) &&
        !BlockList_Member(p, blk_pred_(lab->block))) {
      blk_pred_(lab->block) = mk_CSEBlockList(blk_pred_(lab->block), p);
      return YES;
    } else
      return NO;
}

void cse_RemovePredecessor(LabelNumber *lab, BlockHead *b)
{
    if (!is_exit_label(lab))
      blk_pred_(lab->block) = (BlockList *)generic_ndelete((IPtr)b, (List *)blk_pred_(lab->block));
}

static void PruneDominatorSets(void) {
    BlockHead *p;
    bool changed;
    do {
        changed = NO;
        for (p = top_block; p != NULL; p = blkdown_(p)) {
            VRegSetP s = blk_dominators_(p);
            if (blk_reached_(p)) {
                if (blkflags_(p) & BLKSWITCH) {
                    LabelNumber **v = blktable_(p);
                    int32 i, n = blktabsize_(p);
                    for (i=0; i<n; i++)
                        changed = changed | PruneSuccessors(v[i], s);
                } else {
                    changed = changed | PruneSuccessors(blknext_(p), s);
                    if (blkflags_(p) & BLK2EXIT)
                        changed = changed | PruneSuccessors(blknext1_(p), s);
                }
            }
        }
    } while (changed);
}

static void FindDominators(void)
{
    BlockHead *p;
    {   VRegSetP allblocks = NULL;
        for (p = top_block; p != NULL; p = blkdown_(p))
            cseset_insert(blklabname_(p), allblocks, NULL);
        for (p = blkdown_(top_block); p != NULL; p = blkdown_(p))
            blk_dominators_(p) = cseset_copy(allblocks);
        cseset_discard(allblocks);
    }
    blk_reached_(top_block) = YES;
    cseset_insert(blklabname_(top_block), blk_dominators_(top_block), NULL);
    PruneDominatorSets();
    for (p = top_block; p != NULL; p = blkdown_(p)) {
        if (!blk_reached_(p)) {
            cseset_discard(blk_dominators_(p));
            blk_dominators_(p) = NULL;
        } else if (blkflags_(p) & BLKSWITCH) {
            LabelNumber **v = blktable_(p);
            int32 i, n = blktabsize_(p);
            for (i=0; i<n; i++)
                cse_AddPredecessor(v[i], p);
        } else {
            cse_AddPredecessor(blknext_(p), p);
            if (blkflags_(p) & BLK2EXIT)
                cse_AddPredecessor(blknext1_(p), p);
        }
    }
}

static void CSEFoundLoop(BlockHead *header, BlockHead *tail, BlockList *members) {
  if (debugging(DEBUG_CSE) && CSEDebugLevel(2)) {
    if (header == NULL)
      cc_msg("fake outer loop:");
    else
      cc_msg("loop %ld<-%ld:", (long)blklabname_(header), (long)blklabname_(tail));
    for (; members != NULL; members = cdr_(members))
      cc_msg(" %ld", (long)blklabname_(members->blklstcar));
    cc_msg("\n");
  }
}

static LoopList *mk_LoopList(
    LoopList *next,
    BlockList *members, BlockHead *preheader, BlockHead *header,
    BlockHead *tail) {
  LoopList *l = CSENew(LoopList);
  ll_mem_(l) = members; ll_prehdr_(l) = preheader; ll_hdr_(l) = header; ll_tail_(l) = tail;
  cdr_(l) = next;
  return l;
}

static void ReplaceInBlockList(BlockList *bl, BlockHead *oldb, BlockHead *newb) {
    for (; bl != NULL; bl = cdr_(bl))
        if (bl->blklstcar == oldb) {
            bl->blklstcar = newb;
            return;
        }
}

static BlockHead *cse_InsertBlockBetween(BlockHead *before, BlockHead *after) {
    BlockHead *newb = insertblockbetween(before, after, YES);
    blkcse_(newb) = CSEBlockHead_New();
    blk_dominators_(newb) = cseset_copy(blk_dominators_(after));
    blkflags_(newb) |= BLKLOOP;
    blk_pred_(newb) = mk_CSEBlockList(NULL, before);
    ReplaceInBlockList(blk_pred_(after), before, newb);

    {   /* Must add the new block to dominator sets now, or if it's a preheader
           we will fail to find a place to insert a preheadeer for another loop
           with the same header.
         */
        BlockHead *b = top_block;
        int32 labno = blklabname_(newb);
        for (; b != NULL; b = blkdown_(b))
            if (dominates(after, b))
                cseset_insert(labno, blk_dominators_(b), NULL);
        cseset_delete(blklabname_(after), blk_dominators_(newb), NULL);
    }
    return newb;
}

static BlockHead *MakePreheader(BlockHead *pred, BlockHead *header)
{
    BlockHead *preheader = cse_InsertBlockBetween(pred, header);
    blkflags_(preheader) |= BLKLOOP;
    return preheader;
}

static bool LoopEmptyBlock(BlockHead *b)
{
   /* for loop optimisation's purposes, we may ignore blocks which
      can't interfere (this means they neither reference nor kill
      any Exprn, but since loop analysis precedes cse_scanblock the simple
         !(blk_wanted_(b) == NULL && blk_killed_(b) == NULL)
      isn't available.  Instead, we use a weaker approximation.
    */
    int i;
    if (blkflags_(b) & BLK2EXIT) return NO;
    if (blk_loopempty_(b) != LOOP_EMPTY) {
        for (i = 0; i < blklength_(b); i++)
            if (blkcode_(b)[i].op != J_SETSPGOTO) return NO;
        blk_loopempty_(b) = LOOP_EMPTY;
    }
    return YES;
}

typedef struct LLL_Loop {
    struct LLL_Loop *cdr;
    BlockList *members;             /* list of basic blocks inside       */
    BlockHead *tail;
    VRegSetP memberset;
} LLL_Loop;

typedef struct LoopListList {
    struct LoopListList *cdr;
    BlockHead *header;
    LLL_Loop *loops;
} LoopListList;

static LoopListList *looplists;

static void AddLoop(BlockHead *p, LabelNumber *q)
{
    /* The flowgraph contains an arc from p to q: if q dominates p,
       this means there is a loop with header q
     */
    if (!is_exit_label(q) && dominates(q->block, p)) {
        BlockHead *header = q->block;
        BlockList *bl, *members;
        LoopListList *l = looplists;
        for (; l != NULL; l = cdr_(l))
            if (l->header == header) {
                LLL_Loop *ll = l->loops;
                for (; ll != NULL; ll = cdr_(ll))
                    if (ll->tail == p) return; /* already got this one */
                break;
            }
        bl = mk_CSEBlockList(NULL, p);
        members = mk_CSEBlockList(NULL, header);
        while (bl != NULL) {
            BlockHead *b = bl->blklstcar;
            bl = cdr_(bl);
            if (!BlockList_Member(b, members)) {
                BlockList *pred = blk_pred_(b);
                members = mk_CSEBlockList(members, b);
                for (; pred != NULL; pred = cdr_(pred))
                    bl = mk_CSEBlockList(bl, pred->blklstcar);
            }
        }
        /* allow for possible compilation of while(a) b; as
           if (a) do b; while (a);
           (Move the loop header before the if)
         */
        if (blk_cmp_(header) != NULL) {
            BlockList *pred = blk_pred_(header);
            BlockHead *b = NULL;
            for (; pred != NULL; pred = cdr_(pred))
                if (!BlockList_Member(pred->blklstcar, members)) {
                    if (b != NULL) {
                        b = NULL;
                        break;
                    }
                    b = pred->blklstcar;
                }
            if (b != NULL && dominates(b, header)
                && ExSetsOverlap(blk_cmp_(b), blk_cmp_(header))
                && Q_issame(blkflags_(b) & Q_MASK, Q_NEGATE(blkflags_(p) & Q_MASK))
                && blknext1_(b) == blknext_(p)) {

                LabelNumber *l1 = blknext_(b), *l2 = blknext1_(b);
                BlockHead *newb = cse_InsertBlockBetween(b, header);
                blknext_(newb) = l1; blknext1_(newb) = l2;
                blklength_(b)--;
                blkcode_(newb) = &blkcode_(b)[blklength_(b)];
                blklength_(newb) = 1;
                blkflags_(newb) |= (blkflags_(b) & Q_MASK) | BLK2EXIT;
                blkflags_(b) = (blkflags_(b) & ~(BLK2EXIT | Q_MASK)) | BLKREXPORTED | BLKREXPORTED2;
                ExSet_TransferExprnsInSet(&blk_available_(b), &blk_available_(newb), blk_cmp_(b));
                blk_cmp_(b) = NULL;
                members = mk_CSEBlockList(members, newb);
                if (l1 == blklab_(header)) l1 = l2;
                if (!is_exit_label(l1))
                    ReplaceInBlockList(blk_pred_(l1->block), b, newb);
                header = newb;
                {   LoopListList *ll = looplists;
                    LLL_Loop *l;
                    for (; ll != NULL; ll = cdr_(ll))
                        for (l = ll->loops; l != NULL; l = cdr_(l))
                            if (l->tail == b) {
                                BlockList *bl = l->members;
                                for (; bl != NULL; bl = cdr_(bl))
                                    if (bl->blklstcar == b) {
                                        bl->blklstcar = newb;
                                        break;
                                    }
                                l->tail = newb;
                            } else if (BlockList_Member(b, l->members))
                                l->members = mk_CSEBlockList(l->members, newb);
                }
            }
        }
        {   LoopListList **ll = &looplists;
            for (ll = &looplists; (l = *ll) != NULL; ll = &cdr_(l))
                if (l->header == header) break;

            CSEFoundLoop(header, p, members);

            if (l == NULL) *ll = l = (LoopListList *) CSEList3(NULL, header, NULL);

            {   LLL_Loop *newl = CSENew(LLL_Loop);
                cdr_(newl) = l->loops; newl-> members = members; newl->tail = p;
                newl->memberset = NULL;
                l->loops = newl;
            }
        }
    }
}

static BlockHead *AddBlockBeforeHeader(BlockHead *header, char *s) {
  BlockHead *b = NULL;
  BlockList *bl, *prev = NULL;
  LabelNumber *blab = NULL;
  for (bl = blk_pred_(header); bl != NULL; prev = bl, bl = cdr_(bl))
    if (!dominates(header, bl->blklstcar)) {
      BlockHead *pred = bl->blklstcar;
      if (debugging(DEBUG_CSE) && CSEDebugLevel(2)) {
        cc_msg("insert %s between %ld and %ld: ",
               s,
               (long)lab_name_(blklab_(pred)),
               (long)lab_name_(blklab_(header)));
      }
      if (b == NULL) {
        b = MakePreheader(pred, header);
        bl->blklstcar = b;
        blab = blklab_(b);
        if (debugging(DEBUG_CSE) && CSEDebugLevel(2))
          cc_msg("%s = %ld\n", s, (long)blklabname_(b));
      } else {
        changesuccessors(pred, blab, blklab_(header));
        cdr_(prev) = cdr_(bl);
        bl = prev;
      }
      cse_AddPredecessor(blab, pred);
    }
  return b;
}

static void AddBlockToLoopsContaining(BlockHead *b, BlockHead *newbh) {
/* Add newbh to the members of any loop containing b (except those for which
   it is the header)
 */
  LoopListList *x;
  LLL_Loop *l;
  for (x = looplists; x != NULL; x = cdr_(x))
    if (x->header != b)
      for (l = x->loops; l != NULL; l = cdr_(l)) {
        BlockList *members = l->members;
        for (; members != NULL; members = cdr_(members))
          if (members->blklstcar == b)
            cdr_(members) = mk_CSEBlockList(cdr_(members), newbh);
      }
}

typedef struct LoopSet {
    struct LoopSet *cdr;
    LLL_Loop *loops;
    VRegSetP xn, un;
} LoopSet;

static void InsertPreheaders(void) {
  LoopListList *ll;
  LLL_Loop *l, *nextl;
  for (ll = looplists; ll != NULL; ll = cdr_(ll))
    if (cdr_(ll->loops) != NULL) {
      /* If there is more than one loop with the same header, convert
         the member blocklist for each loop into a VRegSet in which
         blocks which are empty as far as loop analysis is concerned
         are ignored. (We do this to make comparison easier).
       */
      LoopSet *loopsets = NULL;
      for (l = ll->loops; l != NULL; l = cdr_(l)) {
        BlockList *m  = l->members;
        for (; m != NULL; m = cdr_(m)) {
          BlockHead *b = m->blklstcar;
          if (blk_loopempty_(b) != LOOP_NONEMPTY) {
            if (LoopEmptyBlock(b)) continue;
            blk_loopempty_(b) = LOOP_NONEMPTY;
          }
          l->memberset = cseset_insert(blklabname_(b), l->memberset, NULL);
        }
      }
      /* Now allocate the loops to sets ordered by inclusion of loop
         member sets.
       */
      for (l = ll->loops; l != NULL; l = nextl) {
        LoopSet *ls, **lsp = &loopsets;
        nextl = cdr_(l);
        for (; (ls = *lsp) != NULL; lsp = &cdr_(ls)) {
          int order = cseset_compare(l->memberset, ls->xn);
          if (order == VR_SUBSET) break;
          order = cseset_compare(l->memberset, ls->un);
          if (order != VR_SUPERSET) {
            cdr_(l) = ls->loops; ls->loops = l;
            ls->un = cseset_union(ls->un, l->memberset);
            ls->xn = cseset_intersection(ls->xn, l->memberset);
            while (order == VR_UNORDERED) {
              LoopSet *nexts = cdr_(ls);
              if (nexts == NULL ||
                  cseset_compare(l->memberset, nexts->xn) == VR_SUBSET)
                goto nextloop;
              cdr_(ls) = cdr_(nexts);
              ls->un = cseset_union(ls->un, nexts->un);
              { LLL_Loop *nxl, *xl = nexts->loops;
                for (; xl != NULL; xl = nxl) {
                  nxl = cdr_(xl);
                  cdr_(xl) = ls->loops; ls->loops = xl;
                }
              }
              order = cseset_compare(l->memberset, nexts->un);
            }
            goto nextloop;
          }
        }
        ls = CSENew(LoopSet);
        cdr_(ls) = *lsp; ls->loops = l; cdr_(l) = NULL;
        ls->un = cseset_copy(l->memberset);
        ls->xn = cseset_copy(l->memberset);
        *lsp = ls;
nextloop:;
      }
      /* for each set of loops but the first, invent a new header and insert
         it between the original header and its predecessor
       */
      if (debugging(DEBUG_CSE) && CSEDebugLevel(2)) {
        LoopSet *ls;
        cc_msg("loop sets (header %ld): ", (long)blklabname_(ll->header));
        for (ls = loopsets; ls != NULL; ls = cdr_(ls)) {
          int c = '{';
          LLL_Loop *l = ls->loops;
          for (; l != NULL; l = cdr_(l)) {
            cc_msg("%c%ld", c, (long)blklabname_(l->tail));
            c = ' ';
          }
          if (cdr_(ls) == NULL)
            cc_msg("}\n");
          else
            cc_msg("}, ");
        }
      }
      if (cdr_(loopsets) != NULL) {
        BlockHead *header = ll->header;
        /* we want to deal with the loops largest first */
        LoopSet *ls = (LoopSet *)dreverse((List *)cdr_(loopsets));
        ll->loops = loopsets->loops;
        for (; ls != NULL; ls = cdr_(ls)) {
          BlockHead *newheader = AddBlockBeforeHeader(header, "new header");
          LLL_Loop *l;
          LoopListList *n;
          blkflags_(newheader) &= ~BLKLOOP;
          for (l = ls->loops; l != NULL; l = cdr_(l))
            changesuccessors(l->tail, blklab_(newheader), blklab_(header));
          n = CSENew(LoopListList);
          cdr_(n) = cdr_(ll);
          n->header = newheader; n->loops = ls->loops;
          cdr_(ll) = n;
          AddBlockToLoopsContaining(header, newheader);
          ll = n;
        }
      }
    }

  for (ll = looplists; ll != NULL; ll = cdr_(ll)) {
    BlockHead *header = ll->header;
    BlockHead *preheader = AddBlockBeforeHeader(header, "preheader");
    AddBlockToLoopsContaining(header, preheader);
    for (l = ll->loops; l != NULL; l = cdr_(l))
      all_loops = mk_LoopList(all_loops, l->members, preheader, header, l->tail);
  }
}

static void FindLoops(void)
{
    BlockHead *p;
    BlockList *allbuttop = NULL;
    looplists = NULL;
    for (p=top_block; p != NULL; p = blkdown_(p))
        if (blkflags_(p) & BLKSWITCH) {
            LabelNumber **v = blktable_(p);
            int32 i, n = blktabsize_(p);
            for (i=0; i<n; i++)
                AddLoop(p, v[i]);
        } else {
            AddLoop(p, blknext_(p));
            if (blkflags_(p) & BLK2EXIT)
                AddLoop(p, blknext1_(p));
    }
    InsertPreheaders();
    for (p = blkdown_(top_block); p != NULL; p = blkdown_(p))
        allbuttop = mk_CSEBlockList(allbuttop, p);

    {   /* Since we no longer necessarily lift expressions from the fake outer
           loop into its preheader, we need to know which blocks are inside
           real loops (to avoid lifting into such blocks).
         */
        LoopList *lp;
        loopmembers = NULL;
        for (lp = all_loops; lp != NULL; lp = cdr_(lp)) {
           BlockList *m = ll_mem_(lp);
           for (; m != NULL; m = cdr_(m)) {
               cseset_insert(blklabname_(m->blklstcar), loopmembers, NULL);
           }
        }
        /* @@@ This implies that the loop preheader for a loop is not included
               in the members of a containing loop (or it's pointless) : I
               believe this no longer to be the case.
         */
        for (lp = all_loops; lp != NULL; lp = cdr_(lp))
            if (cseset_member(blklabname_(blkup_(ll_prehdr_(lp))), loopmembers))
                cseset_insert(blklabname_(ll_prehdr_(lp)), loopmembers, NULL);
    }

    /* Add a spurious whole function loop */
    {   BlockHead *h = MakePreheader(top_block, blknext_(top_block)->block);
        CSEFoundLoop(NULL, NULL, allbuttop);
        all_loops = mk_LoopList(all_loops, allbuttop, h, NULL, NULL);
        blkflags_(h) |= BLKOUTER;
    }
}

static bool PruneReached(BlockHead *defblock, LabelNumber *lab)
{
    if (is_exit_label(lab)) return NO;
    {   BlockHead *b = lab->block;
        if (!blk_reached_(b)) return NO;
        if (defblock == b) return NO;
        blk_reached_(b) = NO;
        return YES;
    }
}

static void FindRefIcodes(CSEDef *def, ExprnUse *uses)
{
    BlockHead *defblock = defblock_(def);
    ExprnUse *defuse;
    CSERef *l, **prevp;
    /* There may be many occurrences of this expression in the block
     * defining it (because it is killed between them): we want the last,
     * which since the list is reversed we come to first
     */
    for (defuse = uses ; defuse != NULL ; defuse = cdr_(defuse))
        if (defuse->block == defblock) break;
    if (defuse == NULL)
        if (blklength_(defblock) != 0)
            syserr(syserr_cse_lost_def);
        else  /* must be an extracted loop invariant */
            defic_(def) = NULL;
    else if (!IsRealIcode(defuse))
        defic_(def) = blkcode_(defblock)-1;
    else
        defic_(def) = &useicode_(defuse);
    for (l = defrefs_(def), prevp = &defrefs_(def) ; l != NULL ; l = cdr_(l)) {
        /* Again, there may be many references: we want the first in the block
         * (which we come to last): for all the others, the expression will
         * have a different value.
         */
        ExprnUse *first = NULL;
        ExprnUse *use;
        for (use = uses ; use != NULL ; use = cdr_(use))
            if (use->block == refblock_(l)) first = use;
        if (first == NULL)
            syserr(syserr_cse_lost_use);
        if (flags_(first) & U_NOTREF) {
            *prevp = cdr_(l);
            if (debugging(DEBUG_CSE))
                cc_msg(" (%ld notref)", (long)blklabname_(refblock_(l)));
        } else {
            refuse_(l) = first;
            prevp = &cdr_(l);
        }
    }
}

static void DiscardDef(CSEDef *discard, CSEDef *keep)
{
    cseset_discard(defuses_(discard));
    defuses_(discard) = NULL;
    while (defrefs_(discard) != NULL) {
        CSERef *p;
        ExprnUse *use = refuse_(defrefs_(discard));
        for (p = defrefs_(keep); p != NULL; p = cdr_(p))
            if (refuse_(p) == use) break;
        if (p == NULL)
            defrefs_(keep) = mk_CSERef(defrefs_(keep), use);
        defrefs_(discard) = CSERef_DiscardOne(defrefs_(discard));
    }
}

static void RemoveSubRefs(CSEDef *sub, CSEDef *super) {
    CSERef *subref;
    for (subref = defrefs_(sub); subref != NULL; subref = cdr_(subref)) {
        CSERef *p, **pp;
        ExprnUse *use = refuse_(subref);
        for (pp = &defrefs_(super); (p = *pp) != NULL; pp = &cdr_(p))
            if (refuse_(p) == use) {
                *pp = CSERef_DiscardOne(p);
                break;
            }
    }
}

static void MakeSubdef(CSEDef *sub, CSEDef *super)
{ /* Add sub to the loopinv chain of super.
     Remove the refs to super which are also refs to sub.
     Do not alter super->uses, or its use in ordering defs will be
     defeated (it has no other use).
   */
    if (defsuper_(sub) != NULL) {
        CSEDef *p, **pp = &defsub_(defsuper_(sub));
        for (; (p = *pp) != sub; pp = &defnextsub_(p))
            if (p == NULL) syserr(syserr_cse_makesubdef);
        *pp = defnextsub_(sub);
    }
    defsuper_(sub) = super;
    defnextsub_(sub) = defsub_(super); defsub_(super) = sub;

    RemoveSubRefs(sub, super);
}

static void LinkRefs(CSE *cse, CSEDef *def)
{ /* For the definition  def  of  cse, find the uses of it (blocks for which
   * it is wanted) which the definition must reach.
   */
    BlockHead *defblock = defblock_(def);
    bool local;
    if (defuses_(def) != NULL) {
    /* Non-null defuses here means a local CSE which may be subsumed */
        if (debugging(DEBUG_CSE))
            cc_msg("  %ld local", (long)blklabname_(defblock));
        local = YES;
    } else {
        int32 exid = exid_(cseex_(cse));
        int32 defnest = blknest_(defblock);
        BlockHead *p;
        bool changed;
        if (debugging(DEBUG_CSE))
            cc_msg("  %ld r", (long)blklabname_(defblock));
        for (p = top_block; p != NULL; p = blkdown_(p))
            blk_reached_(p) = dominates(defblock, p);
        if (extype_(cseex_(cse)) != E_UNARYK) {
        /* Constants can't be killed, so they'll reach everything dominated
           by the block containing their definition.
         */
            do {
                changed = NO;
                for (p=top_block; p!=NULL; p = blkdown_(p)) {
                    if ( (!blk_reached_(p) && blk_dominators_(p) != NULL) ||
                                              /* (that is, not unreachable) */
                         (p != defblock && blockkills(exid, p)) ) {

                        if (blkflags_(p) & BLKSWITCH) {
                            LabelNumber **v = blktable_(p);
                            int32 i, n = blktabsize_(p);
                            for (i=0; i<n; i++)
                                changed = changed | PruneReached(defblock, v[i]);
                        } else {
                            changed = changed | PruneReached(defblock, blknext_(p));
                            if (blkflags_(p) & BLK2EXIT)
                                changed = changed | PruneReached(defblock, blknext1_(p));
                        }
                    }
                }
            } while (changed);
        }
        for (p = top_block; p != NULL; p = blkdown_(p)) {
            if (blk_reached_(p) && p != defblock) {
                cseset_insert(blklabname_(p), def->uses, NULL);
                if ( cseset_member(exid, blk_wanted_(p)) &&
                     blknest_(p) >= defnest
                 /* this isn't exactly right; it should test that p is inside
                    all loops which defblock is
                  */
                    ) {
                    defrefs_(def) = mk_CSERef(defrefs_(def), p);
                    if (debugging(DEBUG_CSE))
                        cc_msg(" %ld", (long)blklabname_(p));
                }
            }
        }
        local = NO;
    }

    if (defrefs_(def) != NULL) {
        /* if this isn't a local def, defrefs is a list of blocks; change it
           now to a list of ExprnUse.  (which it already is for local defs)
         */
        if (!local)
            FindRefIcodes(def, exuses_(cseex_(cse)));

        if ( (blkflags_(defblock) & BLKOUTER) &&
             ( defrefs_(def) == NULL || /* findreficodes may have discarded some */
               cdr_(defrefs_(def)) == NULL) ) {
            if (debugging(DEBUG_CSE)) cc_msg(": unwanted outer loop inv");
        } else if (defrefs_(def) != NULL) {
            CSEDef **pp, *p;
            bool discard = NO;
            for (pp = &csedef_(cse) ; (p = *pp) != NULL ; pp = &cdr_(p)) {
                int cf = cseset_compare(defuses_(def), defuses_(p));
                /* Equality can only happen here if one def is local */
                if (cf == VR_SUBSET ||
                           (cf == VR_EQUAL && local)) {
                    if (pseudo_reads_r2(exop_(cseex_(cse)))) {
                        MakeSubdef(def, p);
                        if (debugging(DEBUG_CSE))
                            cc_msg(": subdef of %ld", (long)blklabname_(defblock_(p)));
                    } else {
                        DiscardDef(def, p);
                        if (debugging(DEBUG_CSE))
                            cc_msg(": killed (%ld)", (long)blklabname_(defblock_(p)));
                        discard = YES;
                    }
                    break;
                } else if (cf == VR_SUPERSET || cf == VR_EQUAL) {
                    if (pseudo_reads_r2(exop_(cseex_(cse)))) {
                        if ( defsuper_(p) != NULL &&
                             cseset_compare(defuses_(def), defuses_(defsuper_(p))) == VR_SUPERSET)
                            RemoveSubRefs(p, def);
                        else {
                            MakeSubdef(p, def);
                            if (debugging(DEBUG_CSE))
                                cc_msg(": has subdef %ld", (long)blklabname_(defblock_(p)));
                        }
                    } else {
                        DiscardDef(p, def);
                        if (debugging(DEBUG_CSE))
                            cc_msg(": kills %ld", (long)blklabname_(defblock_(p)));
                    }
                }
            }
            if (!discard) {
                cdr_(def) = p;
                *pp = def;
            }
        }
    }
    if (debugging(DEBUG_CSE)) cc_msg("\n");
}

static void LinkRefsToDefs(void)
{
    CSE *cse;
    for ( cse = cselist ; cse != NULL ; cse = cdr_(cse) ) {
        CSEDef *def = csedef_(cse), *next;
        csedef_(cse) = NULL;
        if (debugging(DEBUG_CSE))
            cse_print_node(cseex_(cse));
        for (; def != NULL ; def = next) {
        /* There used to be code here to discard this def if it was a ref of
           one previously encountered, (to save work, since LinkRefs() would
           anyway discard it).  Now we want to do something more complicated,
           the check has been retired.
           We remove def from the list, linkrefs will add it in the appropriate
           place (sorted subdefs first)
         */
            next = cdr_(def);
            LinkRefs(cse, def);
        }
        if (debugging(DEBUG_CSE)) {
            cc_msg(" sorted:\n");
            for (def = csedef_(cse); def != NULL ; def = cdr_(def)) {
                CSERef *ref = defrefs_(def);
                cc_msg("  %ld:", blklabname_(defblock_(def)));
                for (; ref != NULL; ref = cdr_(ref))
                    cc_msg(" %ld", blklabname_(refuse_(ref)->block));
                if (defsub_(def) != NULL) {
                    CSEDef *sub = defsub_(def);
                    char *s = " <";
                    for (; sub != NULL; sub = defnextsub_(sub)) {
                        cc_msg("%s%ld", s, blklabname_(defblock_(sub)));
                        s = " ";
                    }
                    cc_msg(">");
                }
                cc_msg("\n");
            }
        }
    }
}

static bool SafeToLift(Exprn *ex)
{
    J_OPCODE op = exop_(ex) & J_TABLE_BITS;
    switch (extype_(ex)) {
    case E_UNARYK:
        return YES;
    case E_BINARY:
        switch (op)
        {
    case J_CHKUR: case J_CHKLR:
    case J_CHKNEFR: case J_CHKNEDR:
    case J_DIVR:  case J_REMR:
    case J_DIVFR: case J_DIVDR:
            return NO;
    default:
            /* what about other floating pt ops ? */
            return (SafeToLift(e2_(ex)) && SafeToLift(e1_(ex)));
        }

    case E_TERNARY:
        return (SafeToLift(e3_(ex)) && SafeToLift(e2_(ex)) && SafeToLift(e1_(ex)));

    case E_BINARYK:
        switch (op)
        {   case J_CHKUK: case J_CHKLK: case J_CHKNEK: return NO;
            case J_DIVK:  case J_REMK: if (e2k_(ex) == 0) return NO;
        }
        /* drop through - what about floating pt ? */

    case E_UNARY:
        return SafeToLift(e1_(ex));

    default:
    case E_MISC:
        syserr(syserr_cse_safetolift);

    case E_CALL:
    case E_LOADR:
        return NO;

    case E_LOAD:
        {   Location *loc = exloc_(ex);
            if (loctype_(loc) & LOC_anyVAR) return YES;
            return (cse_AdconBase(locbase_(loc), NO) != NULL);
        }
    }
}

static void AddIfSafe(int32 n, void *arg)
{
    if (SafeToLift(exprn_(n))) AddCSE(n, arg);
}

static void FindLoopInvariants(void)
{
    LoopList *p;
    for (p = all_loops ; p != NULL ; p = cdr_(p)) {
        BlockHead *b = ll_prehdr_(p);
        BlockList *bl;
        VRegSetP w = cseset_copy(blk_wantedlater_(b));
        VRegSetP we = cseset_copy(blk_wantedonallpaths_(b));
        VRegSetP u = NULL;
        for (bl = p->members ; bl != NULL ; bl = cdr_(bl)) {
            BlockHead *c = bl->blklstcar;
            cseset_union(u, blk_wanted_(c));
            if (blk_killedinverted_(c)) {
                cseset_intersection(w, blk_killed_(c));
                cseset_intersection(we, blk_killed_(c));
            } else {
                cseset_difference(w, blk_killed_(c));
                cseset_difference(we, blk_killed_(c));
            }
        }
        cseset_difference(w, we);
        cseset_intersection(w, u);
        cseset_intersection(we, u);
        cseset_discard(u);
        if (debugging(DEBUG_CSE)) {
            cc_msg("Loop L%ld:", (long)blklabname_(b));
            for (bl = p->members ; bl != NULL ; bl = cdr_(bl))
                cc_msg(" %ld", (long)blklabname_(bl->blklstcar));
            cc_msg(": safe"); cse_printset(we);
            cc_msg("; unsafe"); cse_printset(w);
            cc_msg("\n");
        }
        cseset_map(we, AddCSE, (VoidStar)b);
        cseset_map(w, AddIfSafe, (VoidStar)b);
    }
}

static BindList *nconcbl(BindList *l, BindList *bl)
{
/* Returns its first argument, adjusted to have as its tail the BindList bl
   if it didn't on entry (Bindlists in the places being adjusted share tails,
   so after the first has been adjusted many others will already be right).
 */
    BindList *p = l;
    BindList *prev = NULL;
    for ( ; p != NULL ; prev = p, p = cdr_(p) )
        if (p == bl) return l;
    if (prev == NULL) return bl;
    cdr_(prev) = bl;
    return l;
}

static Icode *StoreCSE2(Icode *newic, VRegnum r1, Binder *binder)
{
    newic = newic+1;
    INIT_IC(*newic, CSEaccessOp(r1, J_STRV));
    newic->r1.r = r1;
    newic->r3.b = binder;
    return newic;
}

static Icode *StoreCSE(Icode *newic, CSEDef *def)
{ /*  ADCONs etc get treated specially here, in that rather than store
      into the CSE binder, we use an ADCON with the register for the CSE
      binder as r1.  This is so that, if the binder gets spilled, the ADCON
      is voided.  (The load of the CSE register is inserted before the
      original instruction, so that can be turned into a use of the CSE
      in case it isn't spilled).
   */
    VRegnum r1 = newic->r1.r;
    if (is_calln(defex_(def))) {
        int32 i, nres = exnres_(defex_(def));
        r1 = R_A1;
        /* Local CSE def which is a non-local ref */
        /* @@@ presumably, this should never happen now */
        /*   r2 = call->r1.r;*/
        for (i = 1; i < nres; i++)
            newic = StoreCSE2(newic, r1+i, defbinder_(def, i));
    }
#ifdef TARGET_ALLOWS_COMPARE_CSES
    if (is_compare(exop_(defex_(def)))) {
        newic->op = (newic->op & ~Q_MASK) | defmask_(def);
        blkflags_(defblock_(def)) |= BLKCCEXPORTED;
        return newic;
    }
#endif

    if (j_is_check(exop_(defex_(def))))
        return newic;

    if (defbinder_(def, 0) == NULL)
        syserr(syserr_storecse, (long)blklabname_(defblock_(def)),
                                (long)exid_(defex_(def)));
    if (pseudo_reads_r2(newic->op)) {
        VRegnum b = bindxx_(defbinder_(def, 0));
        Icode *ic = newic+1;
        *ic = *newic;
        ic->r2.r = newic->r1.r = b;
        if (newic->r2.r != GAP) {
            forget_slave(ic->r1.r, newic->r2.r);
            note_slave(b, newic->r2.r);
        }
        note_slave(ic->r1.r, b);
        return ic;
    } else if (pseudo_reads_r1(newic->op)) {
        VRegnum b = bindxx_(defbinder_(def, 0));
        Icode *ic = newic+1;
        *ic = *newic;
        newic->op = exop_(defex_(def));
        newic->r2.r = ic->r1.r;
        if (ic->r1.r != GAP) note_slave(b, ic->r1.r);
        ic->r1.r = newic->r1.r = b;
        return ic;
    } else
        return StoreCSE2(newic, r1, defbinder_(def, 0));
}

static int32 DefsSize(CSEDef *defs)
{
    int32 ndefs = 0;
    for (; defs != NULL; defs = cdr_(defs))
        if (is_calln(defex_(defs)))
            ndefs += exnres_(defex_(defs));
        else
        if (!is_compare(exop_(defex_(defs)))
            && !j_is_check(exop_(defex_(defs))))
            ndefs++;
    cse_count += ndefs;
    return ndefs;
}

typedef struct CopyList CopyList;
typedef struct CopyCSE CopyCSE;
typedef struct CopyListList CopyListList;

struct CopyList {
    CopyList *cdr;
    Icode  icode;
    CopyCSE *cse;
    Exprn  *exprn;
    int level;
};
#define cl_ic_(p) ((p)->icode)
#define cl_cse_(p) ((p)->cse)
#define cl_exprn_(p) ((p)->exprn)
#define cl_level_(p) ((p)->level)

struct CopyListList {
    CopyListList *cdr;
    CopyList *p;
    int resno;
};
#define cl_(q) ((q)->p)
#define cll_resno_(q) ((q)->resno)
#define mk_CopyListList(a,b,c) ((CopyListList*)CSEList3((a),(b),(c)))

typedef struct CSEDefList CSEDefList;

struct CSEDefList {
    CSEDefList *cdr;
    CSEDef *def;
};
#define dl_def_(p) ((p)->def)
#define mk_CSEDefList(a,b) ((CSEDefList *)CSEList2((a),(b)))

struct CopyCSE {
    CopyCSE *cdr;
    CopyList *def;
    CopyListList *refs;
    CSEDefList *csedefs;
};

static CopyCSE *addeddefs;

static CopyList *mk_CopyList(
    CopyList *cl, Exprn *exprn, const Icode *const ic, int level)
{
    CopyList *q = CSENew(CopyList);
    cdr_(q) = cl; cl_cse_(q) = NULL; cl_exprn_(q) = exprn; cl_level_(q) = level;
    cl_ic_(q) = *ic;
    return q;
}

typedef struct LiftedTernaryList LiftedTernaryList;
struct LiftedTernaryList {
    LiftedTernaryList *cdr;
    Exprn *ex;
    union { CopyList *cl; Icode *ic; BlockHead *b; } e1, e2, e3;
};
#define lt_ex_(p) ((p)->ex)
#define lt_mask_(p) (exmask_(lt_ex_(p)))
#define lt_ic1_(p) ((p)->e1.ic)
#define lt_ic2_(p) ((p)->e2.ic)
#define lt_ic3_(p) ((p)->e3.ic)
#define lt_cl1_(p) ((p)->e1.cl)
#define lt_cl2_(p) ((p)->e2.cl)
#define lt_cl3_(p) ((p)->e3.cl)
#define lt_b1_(p) ((p)->e1.b)
#define lt_b2_(p) ((p)->e2.b)
#define lt_b3_(p) ((p)->e3.b)

typedef struct {
    LiftedTernaryList *ternaries;
    int32 callcount;
} AEB_Res;

static CopyList *AddExprnsBelow(
    CopyList *cl, Exprn *exprn, int level, VRegnum targetr,
    CopyList **clp, AEB_Res *resp);

static VRegnum aeb_argumentreg(
    CopyList **cl, Exprn *e, AEB_Res *resp, int resno, int level, VRegnum targetr)
{
    CopyList *arg;
    *cl = AddExprnsBelow(*cl, e, level+1, targetr, &arg, resp);
    if (cl_cse_(arg) == NULL)
        return cl_ic_(arg).r1.r;
    else {
        Icode ic;
        VRegnum oldr = cl_ic_(arg).r1.r,
                newr = targetr == GAP ? vregister(vregsort(oldr)) : targetr;
        if (pseudo_reads_r2(exop_(e)))
        {
            INIT_IC(ic, cl_ic_(arg).op);
            ic.r1.r = newr;
            ic.r3 = cl_ic_(arg).r3;
            *cl = mk_CopyList(*cl, NULL, &ic, level);
        } else {
            INIT_IC(ic, CSEaccessOp(oldr, J_LDRV));
            ic.r1.r = newr;
            ic.r3.i = 0;
            *cl = mk_CopyList(*cl, NULL, &ic, level);
        }
        cl_cse_(arg)->refs = mk_CopyListList(cl_cse_(arg)->refs, *cl, resno);
        return newr;
    }
}

static CopyCSE *mk_CopyCSE(CopyList *p)
{
    CopyCSE *q = CSENew(CopyCSE);
    cdr_(q) = addeddefs; q->def = p;
    q->refs = NULL; q->csedefs = NULL;
    addeddefs = q;
    return q;
}

static CopyList *AddExprnsBelow(
    CopyList *cl, Exprn *exprn, int level, VRegnum targetr,
    CopyList **clp, AEB_Res *resp)
{
    CopyList *p;
    if (is_compare(exop_(exprn)))
        p = NULL;
    else
        for (p = cl ; p != NULL ; p = cdr_(p))
            if (cl_exprn_(p) == exprn) {
                if (cl_cse_(p) == NULL)
                    cl_cse_(p) = mk_CopyCSE(p);
                break;
            }

    if (p == NULL) {
        Icode ic;
        RegSort r1sort;
        INIT_IC(ic, exop_(exprn));
        r1sort = DestRegSort(ic.op);
        ic.r1.r = targetr != GAP ? targetr :
                   r1sort == GAP ? GAP :
                                   vregister(r1sort);
        switch (extype_(exprn)) {
        case E_UNARYK:
            ic.r3.i = e1k_(exprn);
            break;
        case E_UNARY:
            {   int resno = 0;
                if (ic.op == J_RESULT2)
                    resno = 1, ic.op = J_MOVR;
                ic.r3.r = aeb_argumentreg(&cl, e1_(exprn), resp, resno, level+1, GAP);
            }
            break;
        case E_BINARYK:
            ic.r2.r = aeb_argumentreg(&cl, e1_(exprn), resp, 0, level+1, GAP);
            ic.r3.i = e2k_(exprn);
            break;
        case E_BINARY:
            ic.r2.r = aeb_argumentreg(&cl, e1_(exprn), resp, 0, level+1, GAP);
            ic.r3.r = aeb_argumentreg(&cl, e2_(exprn), resp, 0, level+1, GAP);
            break;
        case E_TERNARY:
            {   LiftedTernaryList *p = CSENew(LiftedTernaryList);
                ic.op = J_NOOP | exmask_(exprn);
                aeb_argumentreg(&cl, e1_(exprn), resp, 0, level+1, GAP);
                cdr_(p) = resp->ternaries;
                resp->ternaries = p;
                lt_cl1_(p) = cl;
                ic.r1.r = aeb_argumentreg(&cl, e2_(exprn), resp, 0, level+1, GAP);
                lt_cl2_(p) = cl;
                aeb_argumentreg(&cl, e3_(exprn), resp, 0, level+1, ic.r1.r);
                lt_cl3_(p) = cl;
                lt_ex_(p) = exprn;
                break;
            }
        case E_LOAD:
            {   /* Loads need greater care, because they have been transformed
                   into compute address; ldrk  but transforming back is subject
                   to constraints I'd rather not know about.
                   (eg on ARM  ldrr<<2 r1, r2, r3 but
                               addr<<2 r1, r2, r3; ldrfk f1, r1, 0)
                   To sidestep this problem, transformed loads have been marked
                   as such and must be transformed back here.
                 */
                Location *loc = exloc_(exprn);
                if (loctype_(loc) & LOC_anyVAR) {
                    ic.r3.b = locbind_(loc);
                    ic.op = J_KtoV(ic.op);
                } else {
                    Exprn *base = locbase_(loc);
                    if (locrealbase_(loc)) {
                      /* an untransformed load: must be copied as it stands */
                        ic.r2.r = aeb_argumentreg(&cl, base, resp, 0, level+1, GAP);
                        ic.r3.i = locoff_(loc);
                    } else {
                      /* base must be one of ADDR, SUBR or ADCONV */
                        J_OPCODE baseop = exop_(base);
                        switch (UnshiftedOp(baseop)) {
                        case J_SUBR:
#ifdef TARGET_HAS_SCALED_ADDRESSING
                            ic.op |= J_NEGINDEX;
                            /* and fall through to treat as ADDR */
                        case J_ADDR:
                            ic.op |= (baseop & J_SHIFTMASK);
#else
                        case J_ADDR:
#endif
                            ic.op = J_KTOR(ic.op);
                            ic.r2.r = aeb_argumentreg(&cl, e1_(base), resp, 0, level+1, GAP);
                            ic.r3.r = aeb_argumentreg(&cl, e2_(base), resp, 0, level+1, GAP);
                            break;
                        case J_ADCONV:
                            ic.r3.b = e1b_(base);
                            ic.r2.r = locoff_(loc);
                            ic.op = J_addvk(ic.op);
                            break;
                        default:
                            syserr(syserr_baseop, (long)baseop);
                        }
                    }
                }
            }
            break;
        case E_CALL:
            {   int32 nargs = exnargs_(exprn), i;
                CopyList *arglist = NULL;
                ++resp->callcount;
                for (i = 0 ; i < nargs ; i++) {
                    Icode ic1;
                    bool isfp = exargisfp_(exprn, i);
                    Exprn *thisarg = exarg_(exprn, i);
                    Exprn *nextarg;
                    VRegnum argreg = isfp ? R_FA1+i :
                        R_A1+i-k_fltregs_(exargdesc_(exprn));
/* CSE_WORD1/2 only used if !TARGET_FP_ARGS_CALLSTD2                    */
                    if ( exop_(thisarg) == CSE_WORD1 &&
                         (i+1) < nargs &&
                         !k_argisfp_(exargdesc_(exprn), i+1) &&
                         exop_(nextarg = exarg_(exprn, i+1)) == CSE_WORD2 &&
                         e1_(thisarg) == e1_(nextarg) ) {
                        CopyList *arg;
                        cl = AddExprnsBelow(cl, e1_(thisarg), level+1, GAP, &arg, resp);
                        INIT_IC(ic1, J_MOVDIR);
                        ic1.r1.r = argreg;
                        ic1.r2.r = argreg + 1;
                        ic1.r3 = cl_ic_(arg).r1;
                        arglist = mk_CopyList(arglist, NULL, &ic1, level+1);
                        i++;
                    }
                    else if (exop_(thisarg) == CSE_WORD1 ||
                               exop_(thisarg) == CSE_WORD2)
                        syserr(syserr_cse_wordn);
                    else {
                        INIT_IC(ic1, isfp ? J_MOVDR : J_MOVR);
                        ic1.r1.r = argreg;
                        ic1.r3.r = aeb_argumentreg(&cl, exarg_(exprn, i),
                                               resp, 0, level+1, GAP);
                        arglist = mk_CopyList(arglist, NULL, &ic1, level+1);
                    }
                }
                cl = (CopyList *)nconc(dreverse((List *)arglist), (List *)cl);
                {   RegSort restype = exfntype_(exprn);
                    ic.r2.i = exargdesc_(exprn) | K_PURE;
                    ic.r1.r = V_resultreg(restype);
                    ic.r3.b = exfn_(exprn);
                    cl = mk_CopyList(cl, exprn, &ic, level);
                    cl_cse_(cl) = mk_CopyCSE(cl);
                    *clp = cl;
                    return cl;
                }
            }
        }
        cl = p = mk_CopyList(cl, exprn, &ic, level);
    }
    *clp = p;
    return cl;
}

typedef struct {
    CopyList *cl;
    int32 ndefs;
    LiftedTernaryList *ternaries;
} IcodeToCopyRes;

static CopyList *IcodeToCopy_i(CSEDef *defs, AEB_Res *resp)
{
    CopyList *res = NULL, *p;
    addeddefs = NULL;
    for ( ; defs != NULL ; defs = cdr_(defs))
        if (defic_(defs) == NULL) {
            res = AddExprnsBelow(res, defex_(defs), 0, GAP, &p, resp);
            if (cl_cse_(p) == NULL) {
                cl_cse_(p) = mk_CopyCSE(p);
            }
            cl_cse_(p)->csedefs = mk_CSEDefList(cl_cse_(p)->csedefs, defs);
        }
    return (CopyList *)dreverse((List *)res);
}

static IcodeToCopyRes IcodeToCopy(BlockHead *b, CSEDef *defs, BindList *bl) {
    IcodeToCopyRes res;
    AEB_Res aeb;
    aeb.callcount = 0;
    aeb.ternaries = NULL;
    {   CopyList *c = IcodeToCopy_i(defs, &aeb);
        int32 n = length((List *) c);
        BindList **bp = &cdr_(bl);
        for (; addeddefs != NULL; addeddefs = cdr_(addeddefs))
            if (addeddefs->csedefs == NULL) {
        /* Observe that if we're doing this, bl must be non-null - so
         * adding new binders generated here after the first element of
         * bl is safe (and saves trouble with the code for modifying
         * SETSPENVs).
         */
            VRegnum r1 = addeddefs->def->icode.r1.r;
            CSEDef *csedef = mk_CSEDef(addeddefs->def->exprn, b);
            J_OPCODE op = addeddefs->def->icode.op;
            if (is_calln(addeddefs->def->exprn)) {
                int32 i, nres = exnres_(addeddefs->def->exprn);
                for (i = 1; i < nres; i++) {
                    defbinder_(csedef, i) = AddCSEBinder(op, bp, r1);
                    n++;
                }
            }
            defic_(csedef) = &addeddefs->def->icode;
            if (!j_is_check(defic_(csedef)->op)) {
                defbinder_(csedef, 0) = AddCSEBinder(op, bp, r1);
                n++;
            }
            addeddefs->csedefs = mk_CSEDefList(NULL, csedef);
        }
        if (aeb.callcount == 1 && !(blkflags_(b) & BLKCALL))
            blkflags_(b) |= BLKCALL;
        else if (aeb.callcount >= 1)
            blkflags_(b) |= BLKCALL | BLK2CALL;
        res.cl = c;
        res.ndefs = n;
        res.ternaries = (LiftedTernaryList *)dreverse((List *)aeb.ternaries);
        return res;
    }
}

static Icode *CopyIcode(Icode *newic, CopyList *c, LiftedTernaryList *tl) {
    for ( ; c != NULL ; c = cdr_(c)) {
        *newic = cl_ic_(c);
        if (cl_cse_(c) != NULL) {
            CopyListList *refs = cl_cse_(c)->refs;
            CSEDefList *defs = cl_cse_(c)->csedefs;
            J_OPCODE op = newic->op;
            for (; defs != NULL; defs = cdr_(defs)) {
                CSEDef *def = dl_def_(defs);
                Binder *b = defbinder_(def, 0);
                if (def->super != NULL && defs == cl_cse_(c)->csedefs) {
                /* Only do this when the superdef is in some other block.
                 * (i.e. only for the first in a sequence of defs for the
                 * same cse).
                 */
                    VRegnum r = bindxx_(defbinder_(defsuper_(def), 0));
                    newic->r2.r = r;
                    note_slave(newic->r1.r, r);
                }
                if (b != NULL) {
                    if (pseudo_reads_r2(op) &&
                        defrefs_(def) == NULL &&
                        cl_level_(c) == 0) {
                        VRegnum r = bindxx_(defbinder_(def, 0));
                        if (newic->r2.r != GAP) {
                            forget_slave(newic->r1.r, newic->r2.r);
                            note_slave(r, newic->r2.r);
                        }
                        *(newic+1) = *newic;
                        newic->op = J_NOOP;
                        newic++;
                        newic->r1.r = r;
                    } else
                        newic = StoreCSE(newic, def);
                }
                for (; refs != NULL; refs = cdr_(refs)) {
                    Icode *ic = &cl_ic_(cl_(refs));
    #ifdef RANGECHECK_SUPPORTED
                    if (b == NULL)
                        ic->op = J_NOOP;
                    else
    #endif
                    if (pseudo_reads_r2(cl_ic_(c).op)) {
                        ic->r2.r = bindxx_(b);
                        note_slave(ic->r1.r, bindxx_(b));
                    } else if (cll_resno_(refs) == 1)
                        ic->r3.b = defbinder_(def, 1);
                    else
                        ic->r3.b = b;
                }
            }
        }
        newic++;
        {   LiftedTernaryList *p;
            for (p = tl; p != NULL; p = cdr_(p)) {
                if (lt_cl1_(p) == c) lt_ic1_(p) = newic;
                if (lt_cl2_(p) == c) lt_ic2_(p) = newic;
                if (lt_cl3_(p) == c) lt_ic3_(p) = newic;
            }
        }
    }
    return newic;
}

static CSERef *FindRealRef(CSEDef *d) {
    for (; d != NULL; d = defnextsub_(d)) {
        if (defrefs_(d) != NULL)
            return defrefs_(d);
        if (d->subdefs != NULL) {
            CSERef *ref = FindRealRef(defsub_(d));
            if (ref != NULL) return ref;
        }
    }
    return NULL;
}

static BindList *ReferenceCSEDefs(BindList *bl, CSEDef *def, CSEUseList **deferredp)
{
    CSEUseList *deferred = NULL;
    for ( ; def != NULL ; def = cdr_(def)) {
        CSERef *ref = defrefs_(def);
#ifdef TARGET_ALLOWS_COMPARE_CSES
        if (is_compare(exop_(defex_(def))))
            defmask_(def) = defic_(def)->op & Q_MASK;
#endif
        if (ref != NULL || defsub_(def) != NULL) {
            bool defer = NO;
            if (
#ifdef TARGET_ALLOWS_COMPARE_CSES
                is_compare(exop_(defex_(def))) ||
#endif
                j_is_check(exop_(defex_(def)))) {
                defbinder_(def, 0) = NULL;
                if (debugging(DEBUG_CSE))
                    cc_msg("\n: ");
            } else {
                J_OPCODE op = exop_(defex_(def));
                VRegnum r1;
                CSERef *realref = FindRealRef(def);
                if (realref == NULL) syserr(syserr_referencecsedefs);
                r1 = (op == CSE_COND) ? blk_ternaryr_(refuse_(realref)->block)
                                      : useicode_(refuse_(realref)).r1.r;
                defbinder_(def, 0) = AddCSEBinder(op, &bl, r1);
                if (debugging(DEBUG_CSE))
                    cc_msg("\n$b [%ld]: ",
                           defbinder_(def, 0), (long)bindxx_(defbinder_(def, 0)));
                if (is_calln(defex_(def))) {
                    int32 i, nres = exnres_(defex_(def));
                    for (i = 1; i < nres; i++) {
                        Binder *b = defbinder_(def, i) = AddCSEBinder(op, &bl, r1);
                        if (debugging(DEBUG_CSE))
                            cc_msg("$b [%ld]: ", b, (long)bindxx_(b));
                    }
                }
            }
            if (debugging(DEBUG_CSE)) {
                cc_msg("%ld/", (long)blklabname_(defblock_(def)));
                if (blklength_(defblock_(def)) == 0)
                    cc_msg("loop inv: ");
                else
                    cc_msg("%ld: ", (long)(defic_(def)-blkcode_(defblock_(def))));
                cse_print_node(defex_(def));
            }
            for ( ; ref != NULL ; ref = cdr_(ref))
                deferred = ReplaceIcode(deferred, refuse_(ref), def);
        }
    }
    *deferredp = deferred;
    return bl;
}

static CSERef *FindAnyRef(CSEDef *sub) {
    CSEDef *p;
    for (p = sub; p != NULL; p = defnextsub_(p))
        if (defrefs_(p) != NULL)
            return defrefs_(p);
    for (p = sub; p != NULL; p = defnextsub_(p))
        if (defsub_(p) != NULL) {
            CSERef *ref = FindAnyRef(defsub_(p));
            if (ref != NULL)
                return ref;
        }
    return NULL;  /* should never happen */
}

static void AddCSEDefsToBlock(CSEDef *def)
{
    CSEDef *next;
    for (; def != NULL; def = next) {
        next = cdr_(def);
        if (defrefs_(def) != NULL || defsub_(def) != NULL) {
            BlockHead *b = defblock_(def);
            if (blkflags_(b) & BLKOUTER) {
            /* for expressions lifted out of the fake outer "loop", we wish to
               lift them not to the loop preheader, but to the nearest block
               which dominates all references.
             */
                CSERef *ref = defrefs_(def),
                       *anyref = ref;
                CSEDef *sub = defsub_(def);
                VRegSetP d;
                bool discard = NO;
                if (ref != NULL)
                    b = refuse_(ref)->block, ref = cdr_(ref);
                else
                    b = defblock_(sub), sub = defnextsub_(sub);
                d = cseset_copy(blk_dominators_(b));
                cseset_difference(d, loopmembers);
                for (; ref != NULL; ref = cdr_(ref))
                    cseset_intersection(d, blk_dominators_(refuse_(ref)->block));
                for (; sub != NULL; sub = sub->nextsub) {
                    cseset_intersection(d, blk_dominators_(defblock_(sub)));
                    if (anyref == NULL) anyref = defrefs_(sub);
                }
                if (anyref == NULL) anyref = FindAnyRef(defsub_(def));
                for (;;) {
                    bool dummy;
                    for (b = top_block; b != NULL; b = blkdown_(b))
                        if (cseset_member(blklabname_(b), d)
                            && cseset_compare(d, blk_dominators_(b)) <= VR_EQUAL)
                            /* (equal or subset) */
                            break;
                    if (b == NULL) syserr(syserr_addcsedefs);
                    /* Don't lift code which alters condition codes
                       into a block which relies on their setting on entry.
                     */
                    if (!(blkflags_(b) & BLKCCLIVE)
                        || (IsRealIcode(refuse_(anyref))
                            && !alterscc(&useicode_(refuse_(anyref)))))
                        break;
                    cseset_delete(blklabname_(b), d, &dummy);
                }
                {  /* if the block to which we've decided to lift the
                      expression contains a reference, it's not a lifted CSE.
                      There may be many references in the block, thanks to an
                      amalgamated local CSE.
                    */
                    CSEDef **subp = &defsub_(def);
                    /* First check for a lifted reference */
                    for (; (sub = *subp) != NULL; subp = &defnextsub_(sub))
                        if (defblock_(sub) == b) {
                            defsuper_(sub) = NULL;
                            *subp = defnextsub_(sub);
                            {   CSEDef *p, *oldsubdefs = defsub_(sub);
                                defsub_(sub) = defsub_(def);
                                subp = &defsub_(sub);
                                for (; (p = *subp) != NULL; subp = &defnextsub_(p))
                                    defsuper_(p) = sub;
                                *subp = oldsubdefs;
                            }
                            discard = YES; break;
                        }
                    if (!discard) {
                        CSERef **prev = &defrefs_(def);
                        CSERef **defpref = NULL;
                        Icode *deficode = NULL;
                        for (; (ref = *prev) != NULL; prev = &cdr_(ref))
                            if (b == refuse_(ref)->block) {
                                if (pseudo_reads_r2(exop_(defex_(def)))) {
                                    discard = YES;
                                    break;
                                } else {
                                    Icode *reficode;
                                    if (IsRealIcode(refuse_(ref)))
                                        reficode = &useicode_(refuse_(ref));
                                    else
                                        reficode = blkcode_(b)-1;
                                    if (deficode != NULL && deficode < reficode)
                                        continue;
                                    defpref = prev;
                                    deficode = reficode;
                                }
                            }
                        if (deficode != NULL) {
                            defic_(def) = deficode;
                            *defpref = cdr_(*defpref);
                        }
                    }
                }
                if (debugging(DEBUG_CSE))
                    if (!(blkflags_(b) & BLKOUTER))
                        cc_msg("%ld %s : %ld\n",
                               (long)exid_(defex_(def)),
                               (discard ? "discarded" :
                                defic_(def) == NULL ? "(lifted)" : "(not lifted)"),
                               (long)blklabname_(b));

                if (discard) continue;
                defblock_(def) = b;
            }
            {   CSEDef **prevp = &blk_defs_(b);
                CSEDef *p = blk_defs_(b);
                /* CSEDefs are ordered by the position of their definition
                   within the block, with those corresponding to lifted
                   expressions (where there is no definition in the block)
                   coming last.
                 */
                for (; p != NULL; p = cdr_(p)) {
                    if ( defic_(def) != 0 &&
                         (defic_(p) == 0 || defic_(p) > defic_(def)))
                        break;
                    prevp = &cdr_(p);
                }
                *prevp = def;
                cdr_(def) = p;
            }
        }
    }
}

#ifdef TARGET_ALLOWS_COMPARE_CSES
static bool CantMarkCCLive(LabelNumber *from, BlockHead *to) {
    BlockHead *p;
    LabelNumber *lab;
    VRegSetP visited = NULL;
    for (lab = from; ; lab = blknext_(p)) {
        bool already;
        if (is_exit_label(lab))
            return YES;
        cseset_insert(lab_name_(lab), visited, &already);
        if (already)
            return YES;
        p = lab->block;
        if (p == to)
            break;
        if (blkflags_(p) & (BLKSWITCH+BLK2EXIT))
            return YES;
    }
    for (p = from->block; ; p = blknext_(p)->block) {
        blkflags_(p) |= BLKCCLIVE;
        if (p == to) break;
    }
    cseset_discard(visited);
    return NO;
}
#endif

static int CompPtr(void const *a, void const *b) {
    IPtr d = (IPtr)a - (IPtr)b;
    if (d == 0)
        return 0;
    else if (d < 0)
        return -1;
    else
        return 1;
}

static int32 LookUpIC(Icode const *p, Icode * const *ic, int32 n) {
    int32 i;
    for (i = 0; i < n; i++)
        if (p == ic[i])
            return i;
    syserr("LookUpIc");
    return n;
}

static BindList *ModifyCode(void)
{
    CSE *cse;
    BindList *bl = NULL;
    CSEUseList *deferred = NULL;
    /* Hang CSE definitions off the heads of the blocks containing them
     * (sorted by the order of their occurrence in the block).
     */
#ifdef TARGET_ALLOWS_COMPARE_CSES
    /* compares as CSEs must inhibit lifting of expressions which affect the
     * condition codes into any block between the successor of the defining
     * block and the referencing block (inclusive).
     * (observe that a block can contain at most one compare, so it would be
     *  foolish to process localcsedefs here).
     */
    for (cse = cselist ; cse != NULL ; cse = cdr_(cse))
        if (is_compare(exop_(cse->exprn))) {
            CSEDef *def = cse->def;
            for (; def != NULL; def = cdr_(def)) {
                BlockHead *defblock = defblock_(def);
                CSERef *ref = defrefs_(def);
                for (; ref != NULL; ref = cdr_(ref)) {
                /* the blocks between the defining and referencing ones must
                 * all be single-exit (other types would destroy the condition
                 * codes).
                 */
                    BlockHead *refblock = refuse_(ref)->block;
                    /* nb MarkCCLive applied to both paths from the defining block */
                    if (CantMarkCCLive(blknext_(defblock), refblock) &
                        CantMarkCCLive(blknext1_(defblock), refblock))
                        syserr(syserr_cse_modifycode_2, (long)blklabname_(refblock),
                                                        (long)blklabname_(defblock));
                }
            }
        }
#endif

    for (cse = cselist ; cse != NULL ; cse = cdr_(cse))
        AddCSEDefsToBlock(cse->def);
    AddCSEDefsToBlock(localcsedefs);

    /* Allocate binders for referenced CSE defs, and also replace the  */
    /* references by loads from the binders. We can't do the latter    */
    /* yet for references using more than one result of a CSE (pure    */
    /* functions calls returning a structure) because to do so would   */
    /* require lengthening the block containing the reference, so      */
    /* add these to the deferred list.                                 */
    {   BlockHead *b;
        for (b = top_block; b != NULL; b = blkdown_(b))
             bl = ReferenceCSEDefs(bl, blk_defs_(b), &deferred);
    }
    /* Allocate members of the deferred list to the blocks containing  */
    /* the references (sorting them by position within the block)      */
    {   CSEUseList *next;
        for (; deferred != NULL; deferred = next) {
            CSEUseList **pp, *p;
            BlockHead *b = u_block_(ul_use_(deferred));
            ptrdiff_t icn = icoden_(ul_use_(deferred));
            next = cdr_(deferred);
            for (pp = &blk_refs_(b); (p = *pp) != NULL; pp = &cdr_(p))
                if (icoden_(ul_use_(p)) > icn)
                    break;
            cdr_(deferred) = p;
            *pp = deferred;
        }
    }
    /* Now add stores into the binders created for eliminated expressions.
     * This means lengthening the code in existing blocks: we could be clever
     * about doing this (largest first, allowing the space for most of the rest
     * to be reused).  Later, perhaps.
     * While I'm doing this, destructively alter all non-null binder chains in
     * block heads to end in the cse list, and all null ones to be the cse list.
     * (Except for the first block - the binders don't exist on function entry.
     *  We're going to create a SETSPENV to introduce them at the end of the
     *  first block).  The intent is to cause the extent of cse binders to be
     * the whole function.  Later, we can be more precise.
     * Do chains in J_SETSPxx first.  These need care to avoid changing the
     * new binder chain for a SETSPENV immediately before a return from null.
     * (Otherwise, return from a function which stores things on the stack but
     * doesn't create a frame will be compromised).
     * There can't be returns without a preceding SETSPENV: cg has been fixed
     * to ensure it.  See cg_return.
     */
    {   SetSPList *p;
        for ( p = setsplist ; p != NULL ; p = cdr_(p) ) {
            BlockHead *b = p->block;
            Icode *ic = p->icode;
            ic->r2.bl = nconcbl(ic->r2.bl, bl);
            if ( ic->op == J_SETSPENV &&
                 ( (ic+1) - blkcode_(b) != blklength_(b) ||
                   !is_exit_label(blknext_(b))))
                ic->r3.bl = nconcbl(ic->r3.bl, bl);
            /* J_SETSPGOTO is done */
        }
    }
    {   BlockHead *b;
        for ( b = blkdown_(top_block) ; b != NULL ; b = blkdown_(b) ) {
/* AM thinks that the code consider the possible presence of            */
/* double_pad_binder and integer_binder (see cg.c) means that this      */
/* insertion (which I take to be adding to bindlists at the start of    */
/* blocks) needs to be rather more careful.                             */
            CSEDef *defs = blk_defs_(b);
            CSEDef *defs2 = blk_defs2_(b);
            CSEUseList *refs = blk_refs_(b);

            blkstack_(b) = nconcbl(blkstack_(b), bl);

            if (defs != NULL || defs2 != NULL || refs != NULL) {
            /* Now we can have a block containing both real CSE defs and
               lifted ones
             */
                IcodeToCopyRes icr = IcodeToCopy(b, defs, bl);
                int32 oldlength;
                Icode *newic;
                Icode *old;
                int32 i;
                int32 ndefs = DefsSize(defs) + icr.ndefs;
                if (defs2 != NULL) ndefs++;
                {   CSEUseList *up = refs;
                    for (; up != NULL; up = cdr_(up))
                        ndefs += nvals_(ul_use_(up));
                }
                oldlength = blklength_(b);
                newic = newicodeblock(oldlength+ndefs);
                old = blkcode_(b);
                blkcode_(b) = newic;
                /* CSEs for ternary expressions must be handled         */
                /* specially, because there's no jopcode to define the  */
                /* CSE or to be replaced by its reference. There can be */
                /* at most one in a block (and the def if present is at */
                /* the front of the list of defs for the block)         */
                /* We must take care not to treat lifted ternaries as   */
                /* ordinary defs: they are handled later.               */
                if (defs2 != NULL) {
                    VRegnum r1 = blk_ternaryr_(b);
                    INIT_IC(*newic, CSEaccessOp(r1, J_LDRV));
                    newic->r1.r = r1;
                    newic->r3.b = defbinder_(defs2, 0);
                    newic++;
                }
                if (defs != NULL
                    && exop_(defex_(defs)) == CSE_COND
                    && defic_(defs) != NULL) {
                    VRegnum r1 = blk_ternaryr_(b);
                    INIT_IC(*newic, CSEaccessOp(r1, J_STRV));
                    newic->r1.r = r1;
                    newic->r3.b = defbinder_(defs, 0);
                    newic++;
                    defs = cdr_(defs);
                }
                for ( i = 0 ; i != blklength_(b) ; i++ ) {
                    *newic = old[i];
                    while (refs != NULL && i == icoden_(ul_use_(refs))) {
                        ExprnUse *use = ul_use_(refs);
                        int32 i,
                              base = valno_(use),
                              nres = nvals_(use);
                        VRegnum r1 = newic->r1.r;
                        --newic;
                        for (i = 0; i <= nres; i++) {
                            ++newic;
                            INIT_IC(*newic, J_NOOP);
                            ReplaceWithLoad(newic, r1, defbinder_(ul_def_(refs), base+i));
                            newic->r1.r = base+i;
                        }
                        refs = cdr_(refs);
                    }
                    while (defs != NULL && &old[i] == defic_(defs)) {
                    /* At the moment, there may be both a non-local and
                       a local def for the same icode (if the exprn was
                       killed earlier in the block)
                     */
                        newic = StoreCSE(newic, defs);
                        defs = cdr_(defs);
                    }
                    newic++;
                }
                if (defs != NULL) {
                    if ( oldlength != 0 &&
                         (blkflags_(b) & (BLKSWITCH | BLK2EXIT))) {
                    /* J_CASEBRANCH or J_CMP must be the last op in the block.
                       Copy from newic rather than old, because J_CMPK may have
                       had its r1 field changed from GAP.
                     */
                        Icode ic;
                        ic = *--newic;
                        newic = CopyIcode(newic, icr.cl, icr.ternaries);
                        *newic++ = ic;
                    } else
                        newic = CopyIcode(newic, icr.cl, icr.ternaries);
                }
                if (oldlength != 0) freeicodeblock(old, oldlength);
                blklength_(b) += ndefs;
                if ((newic - blkcode_(b)) != blklength_(b)) {
                    Icode *ic = blkcode_(b);
                    for (; ic != newic; ic++)
                       print_jopcode(ic);
                    syserr(syserr_cse_modifycode, (long)blklabname_(b),
                              (long)(newic - blkcode_(b)), (long)blklength_(b));
                }
                /* The call to CopyIcode above copied the icodes making */
                /* up a ternary expression, but into a single block. We */
                /* must now break up the single block into several, and */
                /* generate the conditional branches, to recreate the   */
                /* real expressions.                                    */
                if (icr.ternaries != NULL) {
                    int32 i, n = 3 * length((List *)icr.ternaries);
                    Icode **ic = CSENewN(Icode *, n);
                    BlockHead **bv = CSENewN(BlockHead *, n);
                    LiftedTernaryList *p,
                                      *lastternary;
                    BlockHead *prev = b, *nextb = blkdown_(b);
                    Icode *icstart = blkcode_(b);
                    LabelNumber *next = blknext_(b),
                                *next1 = blknext1_(b);
                    int32 bflags = blkflags_(b);
                    blkflags_(b) &= ~(BLK2EXIT | BLKSWITCH | Q_MASK);
                    for (i = 0, p = icr.ternaries; p != NULL; p = cdr_(p), i += 3) {
                        ic[i] = lt_ic1_(p);
                        ic[i+1] = lt_ic2_(p);
                        ic[i+2] = lt_ic3_(p);
                    }
                    qsort(ic, n, sizeof(Icode const *), CompPtr);
                    for (i = 0; i < n; i++) {
                        BlockHead *p = insertblockbetween(prev, nextb, NO);
                        blkcse_(p) = CSEBlockHead_New();
                        blk_dominators_(p) = cseset_copy(blk_dominators_(nextb));
                        blklength_(prev) = ic[i] - icstart;
                        blkcode_(p) = ic[i];
                        bv[i] = p;
                        prev = p;
                        icstart = ic[i];
                    }
                    blklength_(prev) = newic - icstart;
                    for (p = icr.ternaries; p != NULL; p = cdr_(p)) {
                        int32 i1 = LookUpIC(lt_ic1_(p), ic, n),
                              i2 = LookUpIC(lt_ic2_(p), ic, n),
                              i3 = LookUpIC(lt_ic3_(p), ic, n);
                        BlockHead *next =  lt_b1_(p) = bv[i1],
                                  *next1 = lt_b2_(p) = bv[i2],
                                  *after = lt_b3_(p) = bv[i3];
                        if (i1 == 0)
                            prev = b;
                        else
                            prev = bv[i1-1];
                        if (i3 == n-1)
                            lastternary = p;
                        blkflags_(prev) |= BLK2EXIT;
                        blkflags_(prev) |= lt_mask_(p);
                        blknext_(prev) = blklab_(next1);
                        blknext1_(prev) = blklab_(next);
                        blknext_(next) = blklab_(after);
                        blknext_(next1) = blklab_(after);
                        blk_pred_(next) = mk_CSEBlockList(NULL, prev);
                        blk_pred_(next1) = mk_CSEBlockList(NULL, prev);
                        blk_pred_(after) = mk_CSEBlockList(mk_CSEBlockList(NULL, next), next1);
                    }
#ifdef TARGET_ALLOWS_COMPARE_CSES
                    prev = bv[n-1];
                    /* If the expression setting the condition for the  */
                    /* last lifted ternary is also the expression       */
                    /* setting the condition for the block it's being   */
                    /* lifted to, kill the latter                       */
                    if (bflags & BLK2EXIT) {
                        if (blk_cmp_(b) != NULL
                            && ExSet_Member(e1_(lt_ex_(lastternary)), blk_cmp_(b))
                            && !CantMarkCCLive(blklab_(lt_b1_(lastternary)), prev)
                            && !CantMarkCCLive(blklab_(lt_b2_(lastternary)), prev)) {
                            blkcode_(prev)[blklength_(prev)-1].op = J_NOOP;
                        } else
                            blk_cmp_(prev) = blk_cmp_(b);
                        blk_cmp_(b) = NULL;
                    }
#endif
                    if (bflags & BLKSWITCH) {
                        LabelNumber **table = (LabelNumber **)next;
                        int32 i, n = (int32)next1;
                        blkflags_(prev) |= BLKSWITCH;
                        blktable_(prev) = table;
                        blktabsize_(prev) = n;
                        for (i = 0; i < n; i++)
                            ReplaceInBlockList(blk_pred_(lab_block_(table[i])), b, prev);
                    } else {
                        if (bflags & BLK2EXIT) {
                            blkflags_(prev) |= bflags & (BLK2EXIT | Q_MASK);
                            blknext1_(prev) = next1;
                            ReplaceInBlockList(blk_pred_(lab_block_(next1)), b, prev);
                        }
                        blknext_(prev) = next;
                        ReplaceInBlockList(blk_pred_(lab_block_(next)), b, prev);
                    }
                }
            }
        }
    }
    {   int32 i = blklength_(top_block);
        Icode *newic = newicodeblock(i+1);
        Icode *old = blkcode_(top_block);
        blkcode_(top_block) = newic;
        if (i != 0) memcpy(newic, old, (int) i*sizeof(Icode));
        freeicodeblock(old, i);
        INIT_IC(newic[i], J_SETSPENV);
        newic[i].r2.bl = NULL;
        newic[i].r3.bl = bl;
        blklength_(top_block) = i+1;
    }
    greatest_stackdepth += sizeofbinders(bl, YES);
    return csespillbinders;
}

static clock_t csetime;

static void cse_setup(void)
{   csespillbinders = NULL;
    setsplist = NULL;
    vregset_init();
    cselist = NULL; localcsedefs = NULL;
}

static BindList *cse_eliminate_i(void)
{   BlockHead *p;
    BindList *bl = NULL;
    clock_t t0 = clock();
    clock_t ts = t0;
    int32 refs = cse_refs, count = cse_count;

    nsets = 0; newsets = 0; setbytes = 0;
    end_emit();

    cse_setup();
    for (p = top_block; p != NULL; p = blkdown_(p))
      blkcse_(p) = CSEBlockHead_New();

    FindDominators();
    if (cse_enabled && (!usrdbg(DBG_LINE) || usrdbg(DBG_OPT_CSE))) {

      phasename = "CSE_Available";
      cse_scanblocks(top_block);
      /* which can alter arcs in the flowgraf, so now we recompute
         the dominator sets
       */
      phasename = "CSELoops";
      {   VRegSetP allblocks = NULL;
          for (p = top_block; p != NULL; p = blkdown_(p))
              cseset_insert(blklabname_(p), allblocks, NULL);
          for (p = blkdown_(top_block); p != NULL; p = blkdown_(p)) {
              cseset_union(blk_dominators_(p), allblocks);
              blk_reached_(p) = NO;
          }
          cseset_discard(allblocks);

          PruneDominatorSets();
          for (p = top_block; p != NULL; p = blkdown_(p))
              if (!blk_reached_(p)) {
                  cseset_discard(blk_dominators_(p));
                  blk_dominators_(p) = NULL;
              }
          FindLoops();

          {   LoopList *lp;
              for (lp = all_loops; lp != NULL; lp = cdr_(lp)) {
                  BlockList *bp = ll_mem_(lp);
                  for (; bp != NULL; bp = cdr_(bp))
                      blknest_(bp->blklstcar)++;
              }
          }

          if (debugging(DEBUG_CSE | DEBUG_STORE)) {
              cc_msg("dominators found - %d csecs\n", clock()-t0);
          }

          if (debugging(DEBUG_CSE) && CSEDebugLevel(2)) {
              for (p=top_block; p!=NULL; p = blkdown_(p)) {
                  cc_msg("Block %ld dominated by ", blklabname_(p));
                  cse_printset(blk_dominators_(p));
                  cc_msg("\n");
              }
              cc_msg("loop members ");
              cse_printset(loopmembers);
              cc_msg("\n");
          }
      }

      phasename = "CSEdataflow";
      t0 = clock();
      {   bool changed;
          do {
              if (debugging(DEBUG_CSE | DEBUG_STORE))
                  cc_msg("CSE dataflow iteration\n");
              changed = NO;
              for (p=bottom_block; p!=NULL; p = blkup_(p)) {
                  VRegSetP oldwantedlater = blk_wantedlater_(p);
                  VRegSetP oldwantedonallpaths = blk_wantedonallpaths_(p);
                  ExprnsReaching(p);
                  if (!cseset_equal(oldwantedlater, blk_wantedlater_(p)) ||
                      !cseset_equal(oldwantedonallpaths,
                                    blk_wantedonallpaths_(p)))
                      changed = YES;
                  cseset_discard(oldwantedlater);
                  cseset_discard(oldwantedonallpaths);
              }
          } while (changed);
      }
      if (debugging(DEBUG_CSE | DEBUG_STORE)) {
          clock_t now = clock();
          cc_msg("CSE dataflow complete - %d csecs\n", now-t0);
          t0 = now;
      }
      phasename = "CSEfind";
      /* Now things have converged, and for each block we have
       *  available   as before
       *  wantedlater the set of expressions evaluated by some subsequent block
       *              and not killed on any path from here to the evaluating
       *              block.
       *  wantedonallpaths the set of expressions evaluated by some block on each
       *              path from the block, and not killed between here and there.
       * An expression is a candidate for elimination if it is in both available
       * and wantedlater.
       * For loop headers, wantedonallpaths is the set of loop-constant
       * expressions which can be pulled out of the loop with complete safety.
       */
      for (p = bottom_block; p != NULL; p = blkup_(p)) {
          /* This loop goes from the bottom upwards so that, in the list of
           * definitions for a given CSE, ones in earlier blocks come first,
           * so we have a good chance of rejecting definitions which are
           * subsumed without doing any work.
           */
          VRegSetP cses = cseset_copy(blk_wantedlater_(p));
          cseset_intersection(cses, blk_available_(p));
          cseset_map(cses, AddCSE, (VoidStar) p);
          cseset_discard(cses);
          if (debugging(DEBUG_CSE) && CSEDebugLevel(3)) {
              cc_msg("L%li:", (long)blklabname_(p));
              if (blkflags_(p) & BLKLOOP) cc_msg("*");
              cc_msg(" w"); cse_printset(blk_wanted_(p));
              cc_msg(" wl"); cse_printset(blk_wantedlater_(p));
              cc_msg(" we"); cse_printset(blk_wantedonallpaths_(p));
              cc_msg(" k"); if (blk_killedinverted_(p)) cc_msg("~");
                            cse_printset(blk_killed_(p));
              cc_msg(" a"); cse_printset(blk_available_(p)); cc_msg("\n");
          }
      }
      if (debugging(DEBUG_CSE | DEBUG_STORE))
          cc_msg("candidate cses found - ");
      FindLoopInvariants();
      if (debugging(DEBUG_CSE | DEBUG_STORE)) {
          clock_t now = clock();
          cc_msg("candidate loop invariants found - %d csecs\n", now-t0);
          t0 = now;
      }
      LinkRefsToDefs();
      if (debugging(DEBUG_CSE | DEBUG_STORE)) {
          clock_t now = clock();
          cc_msg("cse references linked - %d csecs\n", now-t0);
          t0 = now;
      }
      phasename = "CSEeliminate";
      bl = ModifyCode();
      if (debugging(DEBUG_CSE | DEBUG_STORE)) {
          clock_t now = clock();
          cc_msg("%ld CSEs, %ld references - %d csecs, CSE total %d\n",
                      (long)(cse_count-count), (long)(cse_refs-refs),
                      now-t0, now-ts);
          csetime += (now - ts);
      }
      if (nsets > maxsets) maxsets = nsets;
      if (setbytes > maxbytes) maxbytes = setbytes;
    }

    for (p = top_block; p != NULL; p = blkdown_(p)) {
        /* from syntax to binder store to survive imminent drop_local_store() */
        BlockList *b = NULL, *oldb = blk_pred_(p);
        for (; oldb != NULL; oldb = cdr_(oldb))
            b = mkBlockList(b, oldb->blklstcar);
        blk_pred_(p) = b;
        cseallocrec.alloctype = AT_Bind;
        blk_dominators_(p) = cseset_copy(blk_dominators_(p));
    }
    return bl;
}

extern BindList *cse_eliminate(void)
{
    BindList *bl = cse_eliminate_i();
    if (debugging(DEBUG_CG) ||
        (debugging(DEBUG_CSE) && CSEDebugLevel(1)))
        flowgraf_print("CSE transforms to:", NO);
    return bl;
}

extern void cse_reinit(void)
{
    all_loops = NULL;
    cseallocrec.alloctype = CSEAllocType;
}

extern void cse_init(void)
{
    csetime = 0;
    maxsets = 0; maxbytes = 0;
    cse_count = 0; cse_refs = 0;
/*
 * The next few are done this way so that the object file for the compiler
 * will not contain any initialised statics initialised to relocatable
 * values.  Doing so may (on some systems) make it easier to build a
 * relocatable version of the compiler (e.g. as an Archimedes/RISC-OS
 * relocatable module).
 */
    cseallocrec.alloctype = CSEAllocType;
    cseallocrec.statsloc = &nsets;
    cseallocrec.statsloc1 = &newsets;
    cseallocrec.statsbytes = &setbytes;
}

extern void cse_tidy(void)
{
    if (!debugging(DEBUG_STORE | DEBUG_CSE)) return;
    cc_msg("CSE max sets: %ld (%ld bytes): time %d cs\n",
                (long)maxsets, (long)maxbytes, csetime);
    cc_msg("%ld cses, %ld references\n", (long)cse_count, (long)cse_refs);
}

/* end of mip/cse.c */
