/*
 * mip/cseguts.h: CSE: internal interfaces
 * Copyright (C) Acorn Computers Ltd., 1988.
 * Copyright 1991-1997 Advanced Risc Machines Limited. All rights reserved
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _cseguts_h
#define _cseguts_h 1

#include "regsets.h"

#define CSEDebugLevel(n) (cse_debugcount > (n))

#define E_UNARYK  0  /* unaryop const */
#define E_UNARY   1  /* unaryop Exprn */
#define E_BINARYK 2  /* Exprn binaryop const */
#define E_BINARY  3  /* Exprn binaryop Exprn */
#define E_TERNARY 4  /* Exprn ? Exprn : Exprn */
#define E_LOAD    5  /* load value of vreg/binder/[Exprn, #offset] */
#define E_MISC    6  /* nasties.  should be none? */
#define E_LOADR   7
#define E_CALL    8

typedef struct ExprnUse ExprnUse;
struct ExprnUse {
    ExprnUse *cdr;
    BlockHead *block;
    int32 val_flags_icoden;
    /* was (but can't make that 16-bit int safe)
      struct {
        int valbase: 2,
            valn : 2,
            flags: 5,
            icoden: 23;
      } ix;
    */
};

#define U_COMMUTATIVEFN 0x40 /* Just arg to find_exprn: not in an ExprnUse */
#define U_NOTDEF2 0x20       /*  ...  (U_PEEK, too)                        */
#define U_NOTREF 0x10
#define U_NOTDEF 0x08
#define U_PEEK   0x04
#define U_STORE  0x02
#define U_LOCALCSE 0x01

#define u_block_(x) ((x)->block)
#define flags_(x) ((((x)->val_flags_icoden) >> 4) & 0x1f)
#define icoden_(x) ((ptrdiff_t)(((x)->val_flags_icoden) >> 9))
#define IsRealIcode(x) (icoden_(x) != -1)
#define valno_(x) (((x)->val_flags_icoden) & 0x3)
#define nvals_(x) ((((x)->val_flags_icoden)>>2) & 0x3)
#define vfi_(v, f, i) ((((int32)(i)) << 9) | ((int32)(f) << 4) | (v))
#define setflag_(x, f) ((x)->val_flags_icoden |= (f) << 4)
#define setvalno_(x, v) ((x)->val_flags_icoden = ((x)->val_flags_icoden & ~3L) | (v))
#define setnvals_(x, v) ((x)->val_flags_icoden = ((x)->val_flags_icoden & ~(3L<<2)) | ((v)<<2))

#define useicode_(a) (blkcode_(u_block_(a))[icoden_(a)])

typedef struct LocList LocList;
typedef struct Exprn Exprn;
typedef struct Location Location;
struct Exprn {
    Exprn *cdr;                 /* hash bucket chain */
    J_OPCODE op;
    int32  nodeid;      /* nodeno<<5 | alias | type, see mknodeid_().   */
    ExprnUse *uses;
    VRegSetP leaves;    /* set of Locations on which this Exprn depends */
    union {
        struct {
            bool waslive;
            LocList *locs;      /* locations with this value */
        } a;         /* during available expression analysis */
        int32 newid; /* during renumbering */
    } v;
    union {
        struct {                /* unary operand */
            Exprn *e1;
        } unary;
        struct {
            IPtr  m;            /* immediate const (or binder for ADCON) */
        } unaryk;
        struct {
            Binder *b;          /* see unaryk */
        } unarykb;
        struct {
            FloatCon *f;        /* ADCONF/D, MOVF/DK */
        } unarykf;
        struct {
            Int64Con *i;        /* ADCONLL */
        } unaryki64;
        struct {
            StringSegList *s;   /* STRING */
        } unaryks;
        struct {                /* load or store some location */
            Location *loc;
        } loc;
        struct {
            Exprn *e1;
            int32  m;
        } binaryk;
        struct {
            Exprn *e1;
            Exprn *e2;
        } binary;
        struct {
            VRegnum r;
            Location *rloc;
        } loadr;
        struct {
            Exprn *e1;
            Exprn *e2;
            Exprn *e3;
            J_OPCODE mask;
        } ternary;
        struct {
            Binder *primary;
            int32  argres;      /* a jopcode argdesc, with the function result
                                   type replacing the K_FLAGS */
            Exprn *arg[1];
        } call;
    } u;
};

#define mknodeid_(nodeno, aliasandtype)   ((nodeno)<<5 | (aliasandtype))
#define exid_(e) ((e)->nodeid >> 5)
#define extype_(e) ((e)->nodeid & 0xf)
#define EX_ALIASandTYPE 0x1f
#define EX_ALIAS 0x10
#define exalias_(e) ((e)->nodeid & EX_ALIAS)
#define e1_(e) ((e)->u.binary.e1)
#define e1k_(e) ((e)->u.unaryk.m)
#define e1b_(e) ((e)->u.unarykb.b)
#define e1f_(e) ((e)->u.unarykf.f)
#define e1i64_(e) ((e)->u.unaryki64.i)
#define e1s_(e) ((e)->u.unaryks.s)
#define e2_(e) ((e)->u.binary.e2)
#define e2k_(e) ((e)->u.binaryk.m)
#define e3_(e) ((e)->u.ternary.e3)
#define exmask_(e) ((e)->u.ternary.mask)
#define exloc_(e) ((e)->u.loc.loc)
#define exloadr_(e) ((e)->u.loadr.r)
#define exloadrloc_(e) ((e)->u.loadr.rloc)
#define exfn_(e) ((e)->u.call.primary)
#define exfntype_(e) ((e)->u.call.argres & REGSORTMASK)
#define exargdesc_(e) ((e)->u.call.argres & ~REGSORTMASK)
#define exnargs_(e) k_argregs_((e)->u.call.argres)
#define exargisfp_(e,i) k_argisfp_((e)->u.call.argres,i)
#define exnres_(e) k_resultregs_((e)->u.call.argres)
#define exarg_(e, i) ((e)->u.call.arg[i])
#define exlocs_(e) ((e)->v.a.locs)
#define exwaslive_(e) ((e)->v.a.waslive)
#define exnewid_(e) ((e)->v.newid)
#define exuses_(e) ((e)->uses)
#define exop_(e) ((e)->op)
#define exleaves_(e) ((e)->leaves)

#define is_calln(ex) ((exop_(ex) == J_CALLK || exop_(ex) == J_OPSYSK) && \
                      exnres_(ex) > 1)

typedef struct ExSet ExSet;
struct ExSet {
    ExSet *cdr;
    Exprn *exprn;
};
#define exs_ex_(p) ((p)->exprn)

typedef uint32 LocType;

/* LOC_xxx values can be store references (0..7) or LOC_VAR/LOC_PVAR    */
#define LOC_(MEM_x)    (MEM_x)
#define LOC_VAR        8
#define LOC_PVAR       9      /* non-local or address taken */
#define LOC_anyVAR     (8+16) /* used in masks to detect LOC_VAR/LOC_VAR. */
#define LOC_REG        16
#define LOC_REALBASE   32
#define LOC_CONST      64
#define LOC_LOCAL     128
#define LOC_PEEK      256

struct Location {
    Location *cdr;
    ExSet *curvalue;  /* the set of current values or Null if unknown */
    uint32  idandtype; /* id<<8 | LOC_xxxx | LOC_REALBASE, see mkidandtype_() */
    VRegSetP users;   /* the set of Exprns killed if this location is   */
                      /* stored into (those with this as a leaf).       */
    VRegSetP aliasusers; /* union over possible aliases of alias->users */
                         /* (includes this->users)                      */
    Exprn *load;      /* the Exprn which is a load from this location.
                       * (A plain load if the location is narrow)
                       */
    Location *synonym;
    union {
        struct {
            Binder *binder;
            LocType type;
        } var;
        struct {
            VRegnum regno;
        } reg;
        struct {
            int32 offset;
            Exprn  *base;
        } mem;
    } u;
};

#define mkidandtype_(id, type) ((id) << 8 | (type))
#define locvalue_(p) ((p)->curvalue)
#define loctype_(p) ((p)->idandtype & 0x1f)
#define locrealbase_(p) ((p)->idandtype & LOC_REALBASE)
#define locconst_(p) ((p)->idandtype & LOC_CONST)
#define locid_(p)   ((p)->idandtype >> 8)
#define locreg_(p)  ((p)->u.reg.regno)
#define locbind_(p) ((p)->u.var.binder)
#define locvartype_(p) ((p)->u.var.type)
#define locbase_(p) ((p)->u.mem.base)
#define locoff_(p)  ((p)->u.mem.offset)
#define locsynonym_(p) ((p)->synonym)
#define locload_(p) ((p)->load)
#define ispublic(p) (loctype_(p) != LOC_VAR && !((p)->idandtype & LOC_LOCAL))

typedef struct CSEDef CSEDef;
typedef struct CSEUseList CSEUseList;
typedef struct SavedLocVals SavedLocVals;

struct CSEBlockHead {
   VRegSetP wanted, available;
   VRegSetP wantedlater;
   VRegSetP wantedonallpaths;
   VRegSetP killed;
   CSEDef *defs;
   ExSet *cmp;
   SavedLocVals *locvals;
   VRegnum ternaryr;
   CSEDef *defs2;
   CSEUseList *refs;
   char reached, killedinverted, loopempty, scanned;
};

#define blk_defs_(p) (blkcse_(p)->defs)
#define blk_wanted_(p) (blkcse_(p)->wanted)
#define blk_wantedlater_(p) (blkcse_(p)->wantedlater)
#define blk_wantedonallpaths_(p) (blkcse_(p)->wantedonallpaths)
#define blk_available_(p) (blkcse_(p)->available)
#define blk_killed_(p) (blkcse_(p)->killed)
#define blk_killedexprns_(p) (blkcse_(p)->killedexprns)
#define blk_reached_(p) (blkcse_(p)->reached)
#define blk_pred_(p) (blkusedfrom_(p))
#define blk_killedinverted_(p) (blkcse_(p)->killedinverted)
#define blk_scanned_(p) (blkcse_(p)->scanned)
#define LOOP_NONEMPTY 1
#define LOOP_EMPTY 2
#define blk_loopempty_(p) (blkcse_(p)->loopempty)
#define blk_locvals_(p) (blkcse_(p)->locvals)
#define blk_cmp_(p) (blkcse_(p)->cmp)
#define blk_ternaryr_(p) (blkcse_(p)->ternaryr)
#define blk_defs2_(p) (blkcse_(p)->defs2)
#define blk_refs_(p) (blkcse_(p)->refs)

#define blockkills(n, b) (!cseset_member(n, blk_killed_(b)) == (blk_killedinverted_(b)))

#define blklabname_(p) lab_name_(blklab_(p))

#define EXPRNSEGSIZE 512
#define EXPRNINDEXSIZE 64
#define EXPRNSEGBITS 9

extern Exprn **exprnindex[EXPRNINDEXSIZE];
#define exprn_(id) (exprnindex[(id)>>EXPRNSEGBITS])[(id)&(EXPRNSEGSIZE-1)]

#define CSEAlloc SynAlloc
#define CSEAllocType AT_Syn
#define CSEList4 syn_list4
#define CSEList3 syn_list3
#define CSEList2 syn_list2
#define CSENew NewSyn
#define CSENewN NewSynN
#define CSENewK NewSynK

#define cseset_insert(x, s, oldp) s = vregset_insert(x, s, oldp, &cseallocrec)
#define cseset_delete(x, s, oldp) s = vregset_delete(x, s, oldp)
#define cseset_copy(s) vregset_copy(s, &cseallocrec)
#define cseset_discard(s) vregset_discard(s)
#define cseset_equal(s1, s2) vregset_equal(s1, s2)
#define cseset_compare(s1, s2) vregset_compare(s1, s2)
#define cseset_union(s1, s2) s1 = vregset_union(s1, s2, &cseallocrec)
#define cseset_intersection(s1, s2) s1 = vregset_intersection(s1, s2)
#define cseset_difference(s1, s2) s1 = vregset_difference(s1, s2)
#define cseset_member(x, s) vregset_member(x, s)
#define cseset_map(s, f, arg) vregset_map(s, f, arg)
#define cseset_size(s) (length((List *)(s)))

extern void cse_printset(VRegSetP s);

extern VRegSetAllocRec cseallocrec;

typedef struct SetSPList SetSPList;
struct SetSPList {
    SetSPList *cdr;
    BlockHead *block;
    Icode *icode;
};

extern SetSPList *setsplist;

void cse_print_loc(Location const *x);

void cse_print_node(Exprn const *p);

void cse_printexits(int32 flags, LabelNumber *exit, LabelNumber *exit1);

bool cse_KillExprRef(ExSet *s, Icode *p);

bool cse_AddLocalCSE(Exprn *node, int valno, int nvals, BlockHead *b);
/* A use of node has occurred, with a previous evaluation in the same
   basic block (b) still alive.  No decision has been made as to the
   desirability of making it into a CSE.  Return value indicates whether
   a CSE has been made.
 */

bool cse_AddPredecessor(LabelNumber *lab, BlockHead *b);
void cse_RemovePredecessor(LabelNumber *lab, BlockHead *b);

void cse_scanblocks(BlockHead *top);

Exprn *cse_AdconBase(Exprn *ex, bool allowvaroffsets);

extern ExprnUse *ExprnUse_New(ExprnUse *old, int flags, int valno);

bool cse_KilledInBlock(int32 expid);

bool ExSetsOverlap(ExSet const *a, ExSet const *b);

void ExSet_TransferExprnsInSet(VRegSetP *s1, VRegSetP *s2, ExSet const *set);
  /* For all Exprns in <set> which are also in the set (of Exprn ids)   */
  /* <s1>, remove them from <s1> and add to <s2>.                       */

CSEBlockHead *CSEBlockHead_New(void);

#define MOVKinSet(set)  ExSet_OpMember(set, J_MOVK, 0)
#define MOVFKinSet(set) ExSet_OpMember(set, J_MOVFK, 0)
#define MOVDKinSet(set) ExSet_OpMember(set, J_MOVDK, 0)

ExSet *ExSet_OpMember(ExSet *set, J_OPCODE op, int32 ignorebits);
bool ExSet_Member(Exprn const *e, ExSet const *set);

#if defined TARGET_HAS_SCALED_ADDRESSING || defined TARGET_HAS_SCALED_OPS || \
    defined TARGET_HAS_SCALED_ADD
#  define OpIsShifted(op) (((op) & J_SHIFTMASK) != 0)
#  define UnshiftedOp(op) ((op) & ~J_SHIFTMASK)
#  define OpWithShift(op, op1) ((op) | ((op1) & J_SHIFTMASK))
#  define NEGINDEX J_NEGINDEX
int32 ShiftedVal(J_OPCODE op, int32 b);
#else
#  define ShiftedVal(op,b) (b)
#  define OpIsShifted(op) NO
#  define UnshiftedOp(op) (op)
#  define OpWithShift(op, op1) (op)
#  define NEGINDEX 0L
#endif

FloatCon *CSE_NewDCon(DbleBin const *val);
FloatCon *CSE_NewFCon(FloatBin const *val);
Int64Con *CSE_NewLLCon(int64 const *ip);

bool CSE_Compare_F(int *res, FloatBin const *a, FloatBin const *b);
bool CSE_Compare_D(int *res, FloatCon const *a, FloatCon const *b);

FloatCon *CSE_EvalUnary_F(J_OPCODE op, ExSet *ex);
FloatCon *CSE_EvalUnary_D(J_OPCODE op, ExSet *ex);
bool CSE_EvalUnary_I(J_OPCODE op, int32 *resp, ExSet *ex);

FloatCon *CSE_EvalBinary_F(J_OPCODE op, ExSet *as, FloatCon const *b);
FloatCon *CSE_EvalBinary_D(J_OPCODE op, ExSet *as, FloatCon const *b);
bool CSE_EvalBinary_I(J_OPCODE op, int32 *resp, Exprn *ax, int32 b);

FloatCon *CSE_CanonicalFPConst(FloatCon *old);
Int64Con *CSE_CanonicalLLConst(Int64Con *old);

typedef struct {
  union {
    DbleBin d;
    int64 i;
  } val;
  bool isdouble;
} DRes;

bool CSE_EvaluableDoubleValuedCall(
  Expr const *fn, VRegnum res, int32 argres, ExSet *arg[], DRes *dresp);

bool CSE_EvaluableIntValuedCall(
  Expr const *fn, VRegnum res, int32 argres, ExSet *arg[], int32 *resp);

extern BlockHead *cse_currentblock;

#endif
/* end of mip/cseguts.h */
