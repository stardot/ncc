/*
 * file jopprint.c - things maybe used while debugging compiler
 * Copyright (C) Codemist Ltd., April 1986.
 * Copyright (C) Acorn Computers Ltd., 1988.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$ Codemist 23
 * Checkin $Date$
 * Revising $Author$
 */

#ifdef __STDC__
#  include <string.h>
#  include <stdarg.h>
#  include <stdlib.h>
#else
#  include <strings.h>
#  include <varargs.h>
#endif
#include "globals.h"
#include "cgdefs.h"
#include "store.h"
#define DEFINE_JOPTABLE 1
#include "jopcode.h"
#include "aeops.h"
#include "aetree.h"    /* for pr_stringsegs */
#include "flowgraf.h"   /* for is_exit_label */

#if defined ENABLE_CG || defined ENABLE_REGS || defined ENABLE_CSE

char *condition_name(uint32 w)
/* only for print_opname(). Argument already masked with Q_MASK */
{
    char *op;
    switch(w)
    {
case Q_EQ:  op = "(if EQ)"; break;
case Q_NE:  op = "(if NE)"; break;
case Q_HS:  op = "(if HS)"; break;
case Q_LO:  op = "(if LO)"; break;
case Q_MI:  op = "(if MI)"; break;
case Q_PL:  op = "(if PL)"; break;
case Q_VS:  op = "(if VS)"; break;
case Q_VC:  op = "(if VC)"; break;
case Q_HI:  op = "(if HI)"; break;
case Q_LS:  op = "(if LS)"; break;
case Q_GE:  op = "(if GE)"; break;
case Q_LT:  op = "(if LT)"; break;
case Q_GT:  op = "(if GT)"; break;
case Q_LE:  op = "(if LE)"; break;
case Q_AL + Q_UBIT:                /* bits moved, rationalise */
case Q_AL:  op = "       "; break;
case Q_UEQ: op = "(ifUEQ)"; break;
case Q_UNE: op = "(ifUNE)"; break;
case Q_UKN + Q_UBIT:
case Q_UKN: op = "(ifUKN)"; break;
default:  { static char opbuf[12];
            op = opbuf;
            sprintf(opbuf, "(Q_%.2x???)", (Uint)(w >> 24));
            break;
          }
    }
    return op;
}

extern void jopprint_opname(J_OPCODE o)
{
    char v[20];
    strcpy(v, joptable[o & J_TABLE_BITS].name);
    if (o & J_SIGNED) strcat(v, "s");
    if (o & J_UNSIGNED) strcat(v, "u");
    if (o & J_ALIGNMENT)
        strcat(v, ((o & J_ALIGNMENT) >> J_ALIGNPOS)*3 + "a1\0a2\0a4\0a8");
    if (o & J_BASEALIGN4) strcat(v, "w");
#if defined TARGET_HAS_SCALED_ADDRESSING || defined TARGET_HAS_SCALED_OPS || \
    defined TARGET_HAS_SCALED_ADD
    if (o & J_NEGINDEX) strcat(v, "m");
    if (o & J_SHIFTMASK)
    {   int32 m = (o & J_SHIFTMASK) >> J_SHIFTPOS;
        if ((m & SHIFT_RIGHT) == 0) strcat(v, "<<");
        else if (m & SHIFT_ARITH) strcat(v, ">>");
        else strcat(v, ">>L");
        sprintf(v+strlen(v), "%ld", (long)(m & SHIFT_MASK));
    }
#endif
    cc_msg("%-12s", v);
}

typedef struct BindListIndex BindListIndex;
struct BindListIndex {
    /* int32 */ IPtr h0;        /* same h0 field as a Binder (for puns) */
    BindListIndex *next;
    Binder *binder;
    BindList *bindlist;
};

static unsigned32 bindlist_n;
static BindListIndex *bindlist_index;

void pr_bindlist(BindList *p)
{
    int ch = '{';
    for (; p!=NULL; p = p->bindlistcdr) {
        Binder *b = p->bindlistcar;
        if (bindlist_n != 0) {
            if (h0_(b) == s_binder) {
                if (ch == '{') {
                    BindListIndex *bi = (BindListIndex *)SynAlloc(sizeof(BindListIndex));
                    h0_(bi) = ++bindlist_n;
                    bi->binder = b;
                    bi->next = bindlist_index;
                    bi->bindlist = p;
                    bindlist_index = bi;
                    p->bindlistcar = (Binder *)bi;
                    cc_msg("%c#%ld= $b", ch, (long)bindlist_n ^ 0x80000000, b);
                } else
                    cc_msg("%c$b", ch, b);
                ch = ',';
            } else {
                unsigned32 bindlist_nn = (unsigned32)h0_(b);
                cc_msg("%c#%ld#}", ch, (long)bindlist_nn ^ 0x80000000);
                return;
            }
        } else {
            cc_msg("%c$b", ch, b);
            ch = ',';
        }
    }
    cc_msg("%s", (ch=='{') ? "{}" : "}");
}

static void pr_bindlist_head(BindList *p, int32 n)
{
    int ch = '{';
    for (; --n >= 0; p = p->bindlistcdr, ch = ',')
        cc_msg("%c$b", ch, p->bindlistcar);
    cc_msg("%c*}", ch);
}

static void pr_argdesc(int32 d)
{
#ifdef TARGET_FP_ARGS_IN_FP_REGS        /* TARGET_FP_ARGS_CALLSTD1      */
    cc_msg("%ld(%ld,%ld", k_argwords_(d), k_intregs_(d), k_fltregs_(d));
#else
    cc_msg("%ld(", k_argwords_(d));
#endif
    /*if (k_resultregs_(d) > 1) */cc_msg("=>%ld", k_resultregs_(d));
    cc_msg(")");
    if (d & K_SPECIAL_ARG) cc_msg("[+]");/* + implicit arg   */
    if (d & K_PURE) cc_msg("P");
    if (d & K_COMMUTATIVE) cc_msg("C");
    if (d & K_INLINE) cc_msg("I");       /* will be inlined  */
    if (d & K_VACALL) cc_msg("...");     /* call vararg fn   */
}

void jopprint_op3(J_OPCODE op, VRegInt r2, VRegInt r3)
{
    if (uses_stack(op) ||
        op == J_CALLK || op == J_TAILCALLK ||
        op==J_ADCON || op == J_INIT || op == J_INITF || op == J_INITD)
    {   Binder *bb = r3.b;
        if (bb == NULL || h0_(bb) == s_identifier)
            /* To allow print_jopcode to be called from local cgs */
            cc_msg("$r", (Symstr *)bb);
        else
        {   cc_msg("$b", bb);
            if (bindstg_(bb) & bitofstg_(s_auto))
            {   VRegnum r = bindxx_(bb);
                if (r != GAP) cc_msg(" [r%ld]", (long)r);
            }
        }
    }
    else if (uses_r3(op))
    {   if (r3.r == GAP)
            cc_msg("<**missing register**>");
        else
            cc_msg("%ld", (long)r3.r);
    }
    else switch (op)
    {   case J_MOVDK: case J_CMPDK:
        case J_ADDDK: case J_SUBDK:
        case J_MULDK: case J_DIVDK:
        case J_MOVFK: case J_CMPFK:
        case J_ADDFK: case J_SUBFK:
        case J_MULFK: case J_DIVFK:
        case J_ADCONF:case J_ADCOND:
            if (r3.f->floatstr[0] == '<')
            {   DbleBin db, *dp;
                char b[128];
                if (r3.f->floatlen == ts_float)
                {   fltrep_widen(&r3.f->floatbin.fb, &db);
                    dp = &db;
                } else
                    dp = &r3.f->floatbin.db;
                fltrep_sprintf(b, "%g", dp);
                cc_msg("<expr = %s>", b);
            } else
                cc_msg("%s", r3.f->floatstr);
            break;
        case J_ADCONLL:
            pr_int64(&r3.i64->bin.i);
            break;
        case J_ENDPROC:
            cc_msg("-");
            break;
        case J_B: case J_BXX: case J_LABEL:
            cc_msg("L%ld", (long)lab_xname_(r3.l));
            break;
        case J_ENTER:
        case J_SAVE:
            pr_argdesc(r3.i);
            break;
        case J_STRING:
            pr_stringsegs(r3.s);
            break;
        case J_SETSPENV:
            if (bindlist_n == 0 && r3.bl != NULL && r2.bl != NULL)
            {   int32 lm = length((List *)r3.bl);
                int32 lr2 = length((List *)r2.bl);
                if (lm > lr2) {
                    pr_bindlist_head(r3.bl, lm - lr2);
                    cc_msg(" from ");
                    pr_bindlist(r2.bl);
                } else {
                    pr_bindlist(r3.bl);
                    cc_msg(" from ");
                    pr_bindlist_head(r2.bl, lr2 - lm);
                }
            } else {
                pr_bindlist(r3.bl);
                cc_msg(" from ");
                pr_bindlist(r2.bl);
            }
            break;
        case J_SETSPGOTO:
            cc_msg("L%ld from ", (long)lab_xname_(r3.l));
            pr_bindlist(r2.bl);
            break;
        default:
            cc_msg("%ld", (long)r3.i);
            if (r3.i > 1000 || r3.i < -1000) cc_msg("  [%#lx]", (long)r3.i);
            break;
    }
}

void print_jopcode_1(const Icode *const ic)
{

    const J_OPCODE op = ic->op & J_TABLE_BITS;

        cc_msg("%8s", condition_name(ic->op & Q_MASK));
        jopprint_opname(ic->op & ~J_DEADBITS);

        if (gap_r1(op)) cc_msg("-, ");
        else if (uses_r1(op) || pseudo_reads_r1(op))
        {   if (ic->r1.r == GAP) cc_msg("-, ");
            else cc_msg("%ld, ", (long)ic->r1.r);
        }
        else cc_msg("%ld, ", (long)ic->r1.i);

        if (gap_r2(op) ||
            (pseudo_reads_r2(op) && ic->r2.r == GAP))
            cc_msg("-, ");
        else if (op==J_INFOLINE || op==J_COUNT)
            cc_msg("'%s', ", ic->r2.str);
        else if (op==J_CALLK || op==J_CALLR || op==J_OPSYSK ||
                 op==J_TAILCALLK || op==J_TAILCALLR)
        {   pr_argdesc(ic->r2.i);
            cc_msg(", ");
        } else
            cc_msg("%ld, ",
               (long)((uses_r2(op) || pseudo_reads_r2(op)) ? ic->r2.r : ic->r2.i));

        jopprint_op3(op, ic->r2, ic->r3);
        if (uses_r4(op))
        {   if (ic->r4.r == GAP) cc_msg("<**missing register**>");
            else cc_msg(", %ld", (long)ic->r4.r);
        }
        if (ic->flags != 0) cc_msg(" {%x}", ic->flags);
}

void print_jopcode(const Icode *const ic)
{   const J_OPCODE op = ic->op & ~J_DEADBITS;
    cc_msg("        ");
    print_jopcode_1(ic);
    cc_msg("\n");
    if (op == J_CASEBRANCH || op == J_THUNKTABLE)
    {   Icode ib;
        LabelNumber **v = ic->r2.lnn;
        int32 i, ncase = ic->r3.i;
        ib.op = J_BXX;
        ib.flags = 0;
        ib.r1.r = ib.r2.r = ib.r4.r = GAP;
        for (i=0; i<ncase; i++)
        {   ib.r3.l = v[i];
            print_jopcode(&ib);
        }
    }
}

void flowgraf_printblock(BlockHead *p, bool deadbits)
{
    Icode    *c = blkcode_(p), *limit;
    cc_msg("L%li: ", (long)lab_name_(blklab_(p)));
    if (c == (Icode *)DUFF_ADDR && blklength_(p) > 0) {
        cc_msg("block eliminated by crossjumping\n\n");
        return;
    }
    if (blkflags_(p) & BLKSTACKI)
        cc_msg("stack %ld\n", blkstacki_(p));
    else {
        pr_bindlist(blkstack_(p));
        cc_msg("\n");
    }
    for (limit = c + blklength_(p); c < limit; ++c) {
        if (deadbits)
          cc_msg("%c%c%c%c", (c->op & J_DEAD_R1 ? '1': '-'),
                             (c->op & J_DEAD_R2 ? '2': '-'),
                             (c->op & J_DEAD_R3 ? '3': '-'),
                             (c->op & J_DEAD_R4 ? '4': '-'));
        print_jopcode(c);
    }
    if (!(blkflags_(p) & BLKSWITCH)) {
        Icode ib;
        ib.flags = 0;
        ib.r1.r = ib.r2.r = ib.r3.r = ib.r4.r = GAP;
        if (blkflags_(p) & BLK2EXIT) {
            if (deadbits) cc_msg("    ");
            ib.op = J_B + (blkflags_(p) & Q_MASK);
            ib.r3.l = blknext1_(p);
            print_jopcode(&ib);
        }
        if (!(blkflags_(p) & BLK0EXIT)) {
            if (deadbits) cc_msg("    ");
            ib.op = J_B;
            ib.r3.l = blknext_(p);
            print_jopcode(&ib);
        }
    }
}

static void flowgraf_print_start(void) {
    bindlist_n = 0x80000000;
    bindlist_index = NULL;
}

static void flowgraf_print_end(void) {
    BindListIndex *p = bindlist_index;
    for (; p != NULL; p = p->next)
        p->bindlist->bindlistcar = p->binder;

    bindlist_index = NULL;
    bindlist_n = 0;
}

void flowgraf_print(const char *mess, bool deadbits)
{   BlockHead *p;
    cc_msg("\n\n%s\n\n", mess);
    flowgraf_print_start();
    for (p = top_block; p != NULL; p = blkdown_(p))
        flowgraf_printblock(p, deadbits);
    flowgraf_print_end();
}


/* Q_implies : returns whether cond1 implies cond2 */

bool Q_implies(int32 cond1, int32 cond2)
{
    cond1 &= Q_MASK;
    cond2 &= Q_MASK;
    if (cond1 == Q_UNDEF || cond2 == Q_UNDEF ||
        (cond1 & ~Q_UBIT) == Q_UKN || (cond2 & ~Q_UBIT) == Q_UKN)
        return NO;
    /* equal condition: a == b -> a == b */
    if (cond1 == cond2) return YES;
    if ((cond1 == Q_UEQ || cond1 == Q_EQ) && (cond2 == Q_EQ || cond2 == Q_UEQ))
        return YES;
    if ((cond1 == Q_UNE && cond1 == Q_NE) && (cond2 == Q_NE || cond2 == Q_UNE))
        return YES;
    /* narrowing condition: a == b -> a >= b */
    if ((cond1 == Q_EQ || cond1 == Q_UEQ) && (cond2 == Q_GE || cond2 == Q_LE ||
        cond2 == Q_HS || cond2 == Q_LS))
        return YES;
    /* narrowing condition: a > b -> a >= b */
    if (cond1 == Q_GT && cond2 == Q_GE) return YES;
    if (cond1 == Q_LT && cond2 == Q_LE) return YES;
    if (cond1 == Q_HI && cond2 == Q_HS) return YES;
    if (cond1 == Q_LO && cond2 == Q_LS) return YES;
    /* narrowing condition: a > b -> a != b */
    if ((cond2 == Q_NE || cond2 == Q_UNE) && (cond1 == Q_GT || cond1 == Q_HI ||
        cond1 == Q_LT || cond1 == Q_LO))
        return YES;

    return NO;
}


#else
void print_jopcode_1(const Icode *const ic)
{
    IGNORE(ic);
}
void print_jopcode(const Icode *const ic)
{
    IGNORE(ic);
}
void flowgraf_print(const char *mess)
{
    IGNORE(mess);
}
#endif

#ifdef ENABLE_REGS
void print_xjopcode(const Icode *const ic, char *fmt, ...)
{   va_list ap;
    char b[256];
    va_start(ap, fmt);
    print_jopcode_1(ic);
    vsprintf(b, fmt, ap);
    cc_msg(" %s\n", b);
    va_end(ap);
    /* since this is only used by regalloc we do not need to print out   */
    /* branch tables of CASEBRANCH as it never calls it with such things */
}
#else
void print_xjopcode(const Icode *const ic, char *fmt, ...)
{
    IGNORE(ic); IGNORE(fmt);
}
#endif

/* end of jopprint.c */
