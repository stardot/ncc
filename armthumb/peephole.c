/*
 * C compiler file arm/peephole.c.
 * Copyright (C) Codemist Ltd, 1988.
 * Copyright (C) Acorn Computers Ltd., 1988
 * Copyright (C) Advanced Risc Machines Ltd., 1991
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <string.h>

#include "globals.h"
#include "mcdep.h"
#include "mcdpriv.h"
#include "arminst.h"
#include "aeops.h"
#include "armops.h"
#include "jopcode.h"
#include "store.h"
#include "errors.h"
#include "cg.h"        /* for procflags, greatest_stackdepth */
#include "regalloc.h"  /* regmask */
#include "builtin.h"
#include "inlnasm.h"

#define TRACING 1
#define PendingStackSize 30
#define PeepholeWindowSize 10

static PendingOp *pendingstack;
static PendingOp *pending;

typedef union {
  int32 i;
  unsigned32 u;
  PendingOp *p;
} IntOrP;

#define p_value_unused(u, a)     (!((u)->use & regbit(a)))
#define p_value_unchanged(u, a)  (!((u)->def & regbit(a)))
#define p_reg_unused(u, a)       (!(((u)->use | (u)->def) & regbit(a)))
#define p_psr_unchanged(u)       (!((u)->def & M_PC))
#define p_regset_unused(u, a)    (!(((u)->use | (u)->def) & (a)))
#define p_regset_unchanged(u, a) (!((u)->def & (a)))

typedef enum P_OpType {
   pot_none,
   pot_and,
   pot_or,
   pot_andnot,
   pot_prop,
   pot_peep,
   pot_peep_m,
   pot_op,
   pot_op_m,
   pot_opinset,
   pot_opinset_m
} P_OpType;

typedef enum {
   prt_none,
   prt_kill,
   prt_swapr2r3,
   prt_set,
   prt_asr,
   prt_reverse_fn,
   prt_proc
} P_ReplaceType;

typedef enum {
   pf_op,
   pf_peep,
   pf_r1,
   pf_r2,
   pf_r3,
   pf_r4,
   pf_dataflow,
   pf_cond,
   pf_dead_r1,
   pf_dead_r2,
   pf_dead_r3,
   pf_dead_r4,
   pf_none
} P_Field;

typedef struct PeepReplace {
  /* P_ReplaceType */ unsigned char type;
  /* P_Field */ unsigned char field;
  /* P_FieldType */ unsigned char valtype;
  int32 val;
  /* we really want val to be something like :
    union {
      struct {
        unsigned char inst;
        unsigned char field;
      } field;
      int32 value;
      bool (*proc)(PendingOp *, PendingOp **, RegisterUsage *);
    } val;
  but that can't be initialised.
  */
} PeepReplace;

#define pr_type_(r) ((P_ReplaceType)((r)->type))
#define pr_field_(r) ((P_Field)((r)->field))
#define pr_valtype_(r) ((P_FieldType)((r)->valtype))
#define pr_val_(r) ((r)->val)

typedef bool RProc(PendingOp *, PendingOp **, RegisterUsage *);

#define valproc_(p) (rprocs[(p)->val])

typedef enum {
   pct_none,
   pct_eq,
   pct_ne,
   pct_lt,
   pct_le,
   pct_gt,
   pct_ge,
   pct_ltu,
   pct_leu,
   pct_gtu,
   pct_geu,
   pct_proc0,
   pct_proc1,
   pct_proc2,
   pct_negate,
   pct_or
} PC_Type;

typedef enum {
  pft_none,
  pft_val,
  pft_exprn,
  pft_field,
  pft_inst
} P_FieldType;

typedef enum PE_Op {
  peo_proc1,
  peo_proc2,
  peo_or,
  peo_and,
  peo_eor,
  peo_add,
  peo_sub,
  peo_shr,
  peo_shl,
  peo_div,
  peo_mul,
  peo_last
} PE_Op;

typedef bool P1Proc(IntOrP *resp, IntOrP r1);
typedef bool P2Proc(IntOrP *resp, IntOrP r1, IntOrP r2);

static bool p_log2(IntOrP *resp, IntOrP r1);
static bool p_bit(IntOrP *resp, IntOrP r1);
static bool p_bitcount(IntOrP *resp, IntOrP r1);
static bool p_lsb(IntOrP *resp, IntOrP r1);
static bool p_lsbpair(IntOrP *resp, IntOrP r1);
static bool p_shift_p(IntOrP *resp, IntOrP r1);
static bool p_shift_k(IntOrP *resp, IntOrP r1, IntOrP r2);
static bool p_q_swap(IntOrP *resp, IntOrP r1);
static bool p_q_tounsigned(IntOrP *resp, IntOrP r1);
static bool p_shift_op(IntOrP *resp, IntOrP r1);
static bool p_shift_d(IntOrP *resp, IntOrP r1);

#define pep_argct_log2 1
#define pep_argct_bit 1
#define pep_argct_bitcount 1
#define pep_argct_lsb 1
#define pep_argct_lsbpair 1
#define pep_argct_shift_p 1
#define pep_argct_shift_k 2
#define pep_argct_q_swap 1
#define pep_argct_q_tounsigned 1
#define pep_argct_shift_op 1
#define pep_argct_shift_d 1

typedef enum PE_Proc {
  pep_bit,
  pep_bitcount,
  pep_lsb,
  pep_lsbpair,
  pep_log2,
  pep_shift_k,
  pep_shift_p,
  pep_q_swap,
  pep_q_tounsigned,
  pep_shift_op,
  pep_shift_d,
  pep_last
} PE_Proc;

static P1Proc * const pprocs[pep_last] = {
  p_bit,
  p_bitcount,
  p_lsb,
  p_lsbpair,
  p_log2,
  (P1Proc *)p_shift_k,
  p_shift_p,
  p_q_swap,
  p_q_tounsigned,
  p_shift_op,
  p_shift_d
};

typedef struct PeepExprn {
  /* PE_Op */   unsigned char op;
  /* PE_Proc */ unsigned char fn;
  /* P_FieldType */ unsigned char f1type,
                                  f2type;
  int32 f1,
        f2;
} PeepExprn;

typedef struct PeepConstraint{
  /* PC_Type */ unsigned char type;
  /* PC_Proc */ unsigned char fn;
  /* P_FieldType */ unsigned char f1type, f2type;
  int32 f1, f2;
  /* we really want f1 & f2 to be something like :
    union {
      struct {
        unsigned char inst;
        unsigned char field;
      } field;
      int32 value;
    };
  but that can't be initialised.
  */
} PeepConstraint;

#define pe_op_(c) ((PE_Op)((c)->op))
#define pe_fn_(c) ((PE_Proc)((c)->fn))

#define pc_type_(c) ((PC_Type)((c)->type))
#define pc_fn_(c) ((PC_Proc)((c)->fn))
#define f1type_(c) ((P_FieldType)((c)->f1type))
#define f2type_(c) ((P_FieldType)((c)->f2type))
#define f1_(c) ((c)->f1)
#define f2_(c) ((c)->f2)

#define inst_(f) ((int)((f)>>8))
#define field_(f) ((P_Field)((f)&255))
#define pexprn_(f) (&peepexprns[f])

typedef enum {
  pub_r1,
  pub_r2,
  pub_r3,
  pub_r4,
  pub_r3mask
} P_Use;

#define pu_r1 (1<<pub_r1)
#define pu_r2 (1<<pub_r2)
#define pu_r3 (1<<pub_r3)
#define pu_r4 (1<<pub_r4)
#define pu_r3mask (1<<pub_r3mask)

typedef struct PeepOpDef {
  /* P_OpType */ unsigned char type;
  unsigned char maynotuse,   /* constraints on register fields between the */
               maynotkill;  /* participants in a peephole */
                    /* (only used in top level - the PeepOpDef in a PeepOp */
  unsigned char setcount;
  int32 n;
} PeepOpDef;

typedef struct PeepOp {
  PeepOpDef p;
  unsigned char dead;
  unsigned char replacecount;
  short replaceix;
} PeepOp;

#define peepop_(p) ((J_OPCODE)(p)->n)
#define peepprop_(p) (peepprops[(p)->n])
#define peepsub1_(p) (peepsubs[((p)->n)>>16])
#define peepsub2_(p) (peepsubs[((p)->n)&0xffff])
#define peepset_(p) (peepsets[((p)->n)&0xffff])
#define peepopi_(p) (peepsets[((p)->n)&0xffff])
#define peepmask_(p) (peepsets[((p)->n)>>16])

#define G_ANY   0x7f
#define G_POSSIBLEALIASSTR   1
#define G_NONSTACKMEM 2
#define G_ANYSTR 4
#define G_ALLOWCONDEXEC 0x80

typedef struct PeepHole {
  PeepOp const *insts;
  PeepConstraint const *constraint;
  unsigned char instcount;
  unsigned char constraintcount;
  unsigned char trace;
  unsigned char gapconstraint;
} PeepHole;

typedef enum {
  pcp_notcall,
  pcp_regset_unused,
  pcp_regset_unkilled,
  pcp_notleafproc,
  pcp_movc_pres_r1r2,
  pcp_nostackrefsbelow,
  pcp_difficult_constant,
  pcp_config,
  pcp_pcs_flags,
  pcp_physreg_notread,
  pcp_fp_literal,
  pcp_asymmetric_fn,
  pcp_symmetric_fn,
  pcp_offsetinrange
} PC_Proc;

#define pcp_argct_notcall 1
#define pcp_argct_regset_unused 1
#define pcp_argct_regset_unkilled 1
#define pcp_argct_notleafproc 0
#define pcp_argct_movc_pres_r1r2 2
#define pcp_argct_nostackrefsbelow 2
#define pcp_argct_difficult_constant 1
#define pcp_argct_config 1
#define pcp_argct_pcs_flags 1
#define pcp_argct_physreg_notread 2
#define pcp_argct_fp_literal 2
#define pcp_argct_asymmetric_fn 1
#define pcp_argct_symmetric_fn 1
#define pcp_argct_offsetinrange 2

typedef int C0Proc(RegisterUsage *);
typedef int C1Proc(RegisterUsage *, IntOrP);
typedef int C2Proc(RegisterUsage *, IntOrP, IntOrP);

#define RETLABV ((int32)(IPtr)RETLAB)

#define p_dead_r1 1
#define p_dead_r2 2
#define p_dead_r3 4
#define p_dead_r4 8

static int32 const dead_bits[] = { /* translation from p_dead_xx */
   0,         J_DEAD_R1,           J_DEAD_R2,           J_DEAD_R1+J_DEAD_R2,
   J_DEAD_R3, J_DEAD_R3+J_DEAD_R1, J_DEAD_R3+J_DEAD_R2, J_DEAD_R3+J_DEAD_R1+J_DEAD_R2,
   J_DEAD_R4, J_DEAD_R1+J_DEAD_R4, J_DEAD_R2+J_DEAD_R4, J_DEAD_R1+J_DEAD_R2+J_DEAD_R4,
   J_DEAD_R3+J_DEAD_R4, J_DEAD_R3+J_DEAD_R1+J_DEAD_R4, J_DEAD_R3+J_DEAD_R2+J_DEAD_R4,
   J_DEAD_R3+J_DEAD_R1+J_DEAD_R2+J_DEAD_R4
};

#define M_SP regbit(R_SP)

#define J_SHIFTVAL (SHIFT_MASK << J_SHIFTPOS)
#define J_SHIFTR   (SHIFT_RIGHT << J_SHIFTPOS)
#define J_SHIFTA   (SHIFT_ARITH << J_SHIFTPOS)

#include "peeppat.c"

extern int Profiler_Count_Index_Max;
extern int Profiler_Count_Index;

int Profiler_Count_Index_Max = PeepholeMax;
int Profiler_Count_Index;

#ifdef ENABLE_LOCALCG

static int32 p_count[PeepholeMax+1];

static bool uses_r4_field(J_OPCODE op) {
 /* but not as a register */
  op &= ~J_ALIGNMENT;
  return op == J_MOVC || op == J_CLRC || op == J_PUSHC || op == J_B;
}

static void a_pr_jopcode_nodeadbits(PendingOp *p)
{
  J_OPCODE op = p->ic.op;
  if ((op & J_TABLE_BITS) <= J_LAST_JOPCODE)
  {
    print_jopcode_1(&p->ic);
    if (p->peep & P_RSHIFT)
      cc_msg(", %ld", (long)p->ic.r4.i);
  }
  else {
    char v[20];
    unsigned32 attr = a_attributes(op);
    strcpy(v, a_joptable[(op & J_TABLE_BITS)-J_LAST_JOPCODE-1].name);
    if (op & J_NEGINDEX) strcat(v, "m");
    if (op & J_ALIGNMENT)
        strcat(v, ((op & J_ALIGNMENT) >> J_ALIGNPOS)*3 + "a1\0a2\0a4\0a8");
    if (op & J_SHIFTMASK)
    {   int32 m = (op & J_SHIFTMASK) >> J_SHIFTPOS;
        if ((m & SHIFT_RIGHT) == 0) strcat(v, "<<");
        else if (m & SHIFT_ARITH) strcat(v, ">>");
        else strcat(v, ">>L");
        sprintf(v+strlen(v), "%ld", (long)(m & SHIFT_MASK));
    }
    cc_msg("%8s%-12s", "", v);
    if (attr & _a_gap_r1)
      cc_msg("-, ");
    else if (attr & _a_regmask_r1)
      cc_msg("%#lx, ", p->ic.r1.rr);
    else
      cc_msg("%ld, ", p->ic.r1.rr);
    if (attr & _a_call) {
      cc_msg("%ld(%ld,%ld", k_argwords_(p->ic.r2.rr), k_intregs_(p->ic.r2.rr), k_fltregs_(p->ic.r2.rr));
      if (k_resultregs_(p->ic.r2.rr) > 1) cc_msg("=>%ld", k_resultregs_(p->ic.r2.rr));
      cc_msg(")");
    } else if (attr & _a_gap_r2)
      cc_msg("-");
    else
      cc_msg("%ld", p->ic.r2.rr);
    if (attr & _a_regmask_r3)
      cc_msg(", %#lx", p->ic.r3.i);
    else
      cc_msg(", %ld", p->ic.r3.i);
    if (a_uses_r4(p))
      cc_msg(", %ld", (long)p->ic.r4.i);
    else if (uses_r4_field(op))
      cc_msg(", %#lx", (long)p->ic.r4.i);
  }
  if (p->peep == 0)
    cc_msg("\n");
  else
    cc_msg("  <%lx>\n", (long)p->peep);
}

static void pr_patno(int n) {
  if (n > 0)
    cc_msg("{%3d} ", n);
  else
    cc_msg("      ");
}

void a_pr_jopcode(PendingOp *p) {
  cc_msg("%c%c%c%c",(p->dataflow & J_DEAD_R1 ? '1': '-'),
                    (p->dataflow & J_DEAD_R2 ? '2': '-'),
                    (p->dataflow & J_DEAD_R3 ? '3': '-'),
                    (p->dataflow & J_DEAD_R4 ? '4': '-'));
  a_pr_jopcode_nodeadbits(p);
}

static void pr_res(PendingOp *pending, int n)
{ if (localcg_debug(2)) {
    cc_msg("%2d> ", pending - pendingstack);
    pr_patno(n);
    a_pr_jopcode(pending);
  }
  if (pending < pendingstack) syserr("pr_res not in stack");
}

static void pr_cur(int n, PendingOp *cur, PendingOp *limit)
{
  if (localcg_debug(2)) {
    cc_msg("    ");
    pr_patno(n+1);
    cc_msg("%16s", "");
    cc_msg("%2d: ", limit+1 - &pendingstack[0]);
    a_pr_jopcode(cur);
  }
}

#define count_p(n) (p_count[n]++)

#else

#define pr_res(a, b)
#define pr_cur(n, a, b)
#define count_p(n)

#endif



#define aru_updatekills 1
#define aru_ignorepreuse 2
#define aru_fieldbit_(r) (1<<((r)-pf_r1+2))
#define aru_ignore_r1 (1<<2)
#define aru_ignore_r2 (1<<3)
#define aru_ignore_r3 (1<<4)
#define aru_ignore_r4 (1<<5)

static void AccumulateRegisterUse(RegisterUsage *u, PendingOp *c, int flags) {
  if (a_loads_r1(c)) {
    if (flags & aru_updatekills)
      u->def |= regbit(c->ic.r1.rr);
  }
  if (a_reads_r1(c) && !(flags & aru_ignore_r1))
    u->use |= regbit(c->ic.r1.rr);

  if (a_loads_r2(c)) {
    if (flags & aru_updatekills)
      u->def |= regbit(c->ic.r2.rr);
  }
  if (a_reads_r2(c) && !(flags & aru_ignore_r2)) {
      if (!(c->peep & P_PRE && (flags & aru_ignorepreuse)))
        u->use |= regbit(c->ic.r2.rr);
  }
  if (a_uses_r3(c) && !(flags & aru_ignore_r3))
    u->use |= regbit(c->ic.r3.i);
  if (a_uses_r4(c) && !(flags & aru_ignore_r4))
    u->use |= regbit(c->ic.r4.i);
  if (((c->peep & P_CMPZ) || sets_psr(&c->ic)) && (flags & aru_updatekills))
    u->def |= regbit(R_PSR);
  if (reads_psr(&c->ic))
      u->use |= regbit(R_PSR);
  if (c->cond != Q_AL)
      u->use |= regbit(R_PSR);
  if (a_corrupts_r1(c) && (flags & aru_updatekills))
      u->def |= regbit(c->ic.r1.rr);
  if (a_corrupts_r2(c) && (flags & aru_updatekills))
      u->def |= regbit(c->ic.r2.rr);
  {
    RealRegUse use;
    RealRegisterUse(&c->ic, &use);
    u->use |= use.use.map[0];
    if (flags & aru_updatekills) u->def |= use.def.map[0] | use.c_in.map[0] | use.c_out.map[0];
  }
}

static bool NoStackReferencesBelow(PendingOp *p, PendingOp *limit, int32 n)
{
  for (; ++p <= limit; ) {
    int32 op = p->ic.op & ~J_ALIGNMENT;
    if ( (a_reads_r2(p) && p->ic.r2.rr == R_SP &&
           (a_uses_r3(p) || p->ic.r3.i < n ||
            op == J_PUSHC || op == J_MOVC)) ||
         (a_reads_r1(p) && p->ic.r1.rr == R_SP) ||
         (a_uses_r3(p) && p->ic.r3.i == R_SP && op != J_MOVR) ||
         (op == J_PUSHM || op == J_PUSHD || op == J_PUSHF)
       )
      return NO;
  }
  return YES;
}

#ifdef ENABLE_LOCALCG
#define AdjustStackRefs(a,b,c,d) Real__AdjustStackRefs(a,b,c,d)
#else
#define AdjustStackRefs(a,b,c,d) Real__AdjustStackRefs(a,b,c)
#endif

static void AdjustStackRefs(PendingOp *p, PendingOp *limit, int32 n, int ix)
{
  for (; p <= limit; p++)
    if (a_reads_r2(p) && p->ic.r2.rr == R_SP) {
      p->ic.r3.i -= n;
      pr_res(p, ix);
    } else if (a_uses_r3(p) && p->ic.r3.i == R_SP) {
      p->ic.op = J_ADDK;
      p->ic.r2.rr = R_SP;
      p->ic.r3.i = -n;
      pr_res(p, ix);
    }
}

static bool p_log2(IntOrP *resp, IntOrP r1) {
  resp->i = power_of_two(r1.i); return YES;
}

static bool p_bit(IntOrP *resp, IntOrP r1) {
  resp->i = 1L << r1.i;  return YES;
}

static bool p_bitcount(IntOrP *resp, IntOrP r1) {
  resp->i = 4 * bitcount(r1.i); return YES;
}

static bool p_lsb(IntOrP *resp, IntOrP r1) {
  if (r1.i == 0) return NO;
  resp->i = r1.i & -r1.i; return YES;
}

static bool p_lsbpair(IntOrP *resp, IntOrP r1) {
  int32 i1;
  if (r1.i == 0) return NO;
  i1 = r1.i & -r1.i;
  r1.i ^= i1;
  if (r1.i == 0) return NO;
  resp->i = i1 | (r1.i & -r1.i);
  return YES;
}

static bool p_shift_p(IntOrP *resp, IntOrP r1) {
  int32 shiftop = r1.i & ~(J_SIGNED|J_UNSIGNED);
  resp->i = shiftop == J_SHLR ? P_LSL :
            shiftop == J_RORR ? P_ROR :
            (r1.i & J_SIGNED) ? P_ASR :
                                P_LSR;
  return YES;
}

static bool p_shift_k(IntOrP *resp, IntOrP r1, IntOrP r2) {
  int32 shiftop = r1.i & ~(J_SIGNED|J_UNSIGNED);
  int32 type = shiftop == J_SHLK ? 0 :
               shiftop == J_RORK ? SHIFT_ARITH :
          /* perhaps a bit delicate : an extra bit would be better */
               (r1.i & J_SIGNED) ? (SHIFT_RIGHT+SHIFT_ARITH) :
                                   SHIFT_RIGHT;
  resp->i = (type + r2.i) << J_SHIFTPOS;
  return YES;
}

static bool p_q_swap(IntOrP *resp, IntOrP r1) {
  resp->i = Q_swap(r1.i); return YES;
}

static bool p_q_tounsigned(IntOrP *resp, IntOrP r1) {
  int32 op = r1.i;
  switch (op & Q_MASK) {
  case Q_AL+Q_UBIT: break;
  case Q_LT: op ^= Q_LT ^ Q_LO; break;
  case Q_LE: op ^= Q_LE ^ Q_LS; break;
  case Q_GT: op ^= Q_GT ^ Q_HI; break;
  case Q_GE: op ^= Q_GE ^ Q_HS; break;
  default:   syserr("q_tounsigned %lx", (long)op);
  }
  resp->i = op;
  return YES;
}

static bool p_shift_op(IntOrP *resp, IntOrP r1) {
  int32 sh = (r1.i & J_SHIFTMASK) >> J_SHIFTPOS;
  if ((sh & SHIFT_MASK) != 0)
    switch (sh & (SHIFT_ARITH+SHIFT_RIGHT)) {
    case 0:                       resp->i = J_SHLK; return YES;
    case SHIFT_ARITH:             resp->i = J_RORK; return YES;
    case SHIFT_RIGHT:             resp->i = J_SHRK; return YES;
    case SHIFT_RIGHT+SHIFT_ARITH: resp->i = J_SHRK+J_SIGNED; return YES;
    }
  resp->i = J_NOOP; return YES;
}

static bool p_shift_d(IntOrP *resp, IntOrP r1) {
  resp->i = (r1.i >> J_SHIFTPOS) & SHIFT_MASK;
  return YES;
}

static int32 opfield(PendingOp *op, P_Field field, int32 *dead) {
  switch (field) {
  case pf_op:   return op->ic.op;
  case pf_peep: return op->peep;
  case pf_r1:   *dead = a_uses_r1(op) && (op->dataflow & J_DEAD_R1); return op->ic.r1.rr;
  case pf_r2:   *dead = a_uses_r2(op) && (op->dataflow & J_DEAD_R2); return op->ic.r2.rr;
  case pf_r3:   *dead = a_uses_r3(op) && (op->dataflow & J_DEAD_R3); return op->ic.r3.i;
  case pf_r4:   *dead = a_uses_r4(op) && (op->dataflow & J_DEAD_R4); return op->ic.r4.i;
  case pf_dataflow: return op->dataflow;
  case pf_cond: return op->cond;
  case pf_dead_r1: return (op->dataflow & J_DEAD_R1) != 0;
  case pf_dead_r2: return (op->dataflow & J_DEAD_R2) != 0;
  case pf_dead_r3: return (op->dataflow & J_DEAD_R3) != 0;
  case pf_dead_r4: return (op->dataflow & J_DEAD_R4) != 0;
  default:      syserr(syserr_peep_bad_field);
                return 0;
  }
}

static void setopfield(PendingOp *op, P_Field field, int32 val, int32 dead) {
  switch (field) {
  case pf_op:   op->ic.op = val; break;
  case pf_peep: op->peep = val; break;
  case pf_r1:   op->dataflow &= ~J_DEAD_R1; if (dead) op->dataflow |= J_DEAD_R1;
                op->ic.r1.rr = val; break;
  case pf_r2:   op->dataflow &= ~J_DEAD_R2; if (dead) op->dataflow |= J_DEAD_R2;
                op->ic.r2.rr = val; break;
  case pf_r3:   op->dataflow &= ~J_DEAD_R3; if (dead) op->dataflow |= J_DEAD_R3;
                op->ic.r3.i = val; break;
  case pf_r4:   op->dataflow &= ~J_DEAD_R4; if (dead) op->dataflow |= J_DEAD_R4;
                op->ic.r4.i = val; break;
  case pf_dataflow: op->dataflow = val; break;
  case pf_cond: op->cond = val; break;
  case pf_dead_r1: op->dataflow = (op->dataflow & ~J_DEAD_R1) | (val != 0 ? J_DEAD_R1 : 0); break;
  case pf_dead_r2: op->dataflow = (op->dataflow & ~J_DEAD_R2) | (val != 0 ? J_DEAD_R2 : 0); break;
  case pf_dead_r3: op->dataflow = (op->dataflow & ~J_DEAD_R3) | (val != 0 ? J_DEAD_R3 : 0); break;
  case pf_dead_r4: op->dataflow = (op->dataflow & ~J_DEAD_R4) | (val != 0 ? J_DEAD_R4 : 0); break;
  default:      syserr(syserr_peep_bad_field);
                break;
  }
}

static bool isregfield(PendingOp *op, P_Field field) {
  switch (field) {
  case pf_r1: return a_uses_r1(op);
  case pf_r2: return a_uses_r2(op);
  case pf_r3: return a_uses_r3(op);
  case pf_r4: return a_uses_r4(op);
  default:    return NO;
  }
}

static bool ValidField(PendingOp *ops[], P_FieldType type, int32 val,
                       int n, IntOrP *resp, int32 *dead) {
  switch (type) {
  case pft_none:
    resp->i = 0;
    return YES;

  case pft_val:
    resp->i = val;
    return YES;

  case pft_field:
    if (inst_(val) <= n) {
      resp->i = opfield(ops[inst_(val)], field_(val), dead);
      return YES;
    }
    break;

  case pft_inst:
    if (inst_(val) <= n) {
      resp->p = ops[inst_(val)];
      return YES;
    }
    break;

  case pft_exprn:
    { const PeepExprn *ex = pexprn_(val);
      int32 ignore;
      IntOrP r1, r2;
      if (ValidField(ops, f1type_(ex), f1_(ex), n, &r1, &ignore) &&
          ValidField(ops, f2type_(ex), f2_(ex), n, &r2, &ignore))
        switch (pe_op_(ex)) {
        case peo_add: resp->i = r1.i + r2.i; return YES;
        case peo_sub: resp->i = r1.i - r2.i; return YES;
        case peo_or:  resp->i = r1.i | r2.i; return YES;
        case peo_and: resp->i = r1.i & r2.i; return YES;
        case peo_eor: resp->i = r1.i ^ r2.i; return YES;
        case peo_shr: resp->i = r1.i >> r2.i; return YES;
        case peo_shl: resp->i = r1.i << r2.i; return YES;
        case peo_div: resp->i = r1.i / r2.i; return YES;
        case peo_mul: resp->i = r1.i * r2.i; return YES;

        case peo_proc1:return (pprocs[pe_fn_(ex)])(resp, r1);
        case peo_proc2:return ((P2Proc *)pprocs[pe_fn_(ex)])(resp, r1, r2);
        }
    }
    break;
  }
  return NO;
}

/* This function checks whether a jopcode (after replacement) is still a
 * valid jopcode. The register usage and the register clashes are
 * currently checked.
 */

static bool isvalid_jopcode(PendingOp *p)
{
    char *errmsg = CheckJopcodeP(p, JCHK_MEM | JCHK_REGS);

    if (localcg_debug(2) && errmsg != NULL)
        cc_msg("--- Invalid substitution: %s\n", errmsg);
    return errmsg == NULL;
}

#define Op_Altered 1
#define Op_Cant 2

static int UpdateOp(PendingOp *op, PendingOp *ops[], PeepHole const *p,
                    int count, int ix, RegisterUsage *u, bool check_only) {
  int result = 0;
  IntOrP val;
  PeepReplace const *r = &replacements[ix];
  for (; --count >= 0; r++) {
    int32 dead = 0;
    switch (pr_type_(r)) {
    case prt_kill:
      /* Refuse to kill instructions with alter the condition codes, as
       * it isn't certain that the condition codes are dead.
       * Eg. consider ORRS a, a, b ; CMP x, y, RRX (carry is used)...
       * WD: add a DEAD_CC bit so that dead instructions which set CC can be removed
       */
      if (op->peep & P_SETCC)
          result = Op_Cant;
      else
      {
        op->ic.op = J_NOOP;
        op->peep = 0;
        op->dataflow = 0;
        result |= Op_Altered;
      }
      break;
    case prt_proc:
      if (valproc_(r)(op, ops, u)) result |= Op_Altered;
      break;
    case prt_set:
        ValidField(ops, pr_valtype_(r), pr_val_(r), p->instcount-1, &val, &dead);
        setopfield(op, pr_field_(r), val.i, dead);
        result |= Op_Altered;
        break;
    case prt_swapr2r3:
      { RealRegister r2 = op->ic.r2.rr;
        int32 df = op->dataflow;
        op->ic.r2.rr = op->ic.r3.i;
        op->ic.r3.i = r2;
        df &= ~(J_DEAD_R2 | J_DEAD_R3);
        if (op->dataflow & J_DEAD_R2) df |= J_DEAD_R3;
        if (op->dataflow & J_DEAD_R3) df |= J_DEAD_R2;
        op->dataflow = df;
        result |= Op_Altered;
        break;
      }
    case prt_asr:
      if (!check_only) {
        ValidField(ops, pr_valtype_(r), pr_val_(r), p->instcount-1, &val, &dead);
        AdjustStackRefs(op+1, ops[-1], val.i, p-patterns+1);
        result |= Op_Altered;
      }
      break;
    case prt_reverse_fn:
      /* The list here must include all those in asymmetric_fn() below */
      if (op->ic.r3.sym == bindsym_(exb_(sim.dsubtract)))
        op->ic.r3.sym = bindsym_(exb_(sim.drsb));
      else if (op->ic.r3.sym == bindsym_(exb_(sim.fsubtract)))
        op->ic.r3.sym = bindsym_(exb_(sim.frsb));
      else if (op->ic.r3.sym == bindsym_(exb_(sim.ddivide)))
        op->ic.r3.sym = bindsym_(exb_(sim.drdiv));
      else if (op->ic.r3.sym == bindsym_(exb_(sim.fdivide)))
        op->ic.r3.sym = bindsym_(exb_(sim.frdiv));
      else
        syserr("prt_reverse_fn");
      obj_symref((Symstr *)op->ic.r3.sym, xr_code, 0);
      result |= Op_Altered;
      break;
    }
  }
  if (!isvalid_jopcode(op))
      result |= Op_Cant;
  if (!check_only && (result & Op_Cant))
      syserr("UpdateOp - peephole substitution failed");
  return result;
}

static int notcall(RegisterUsage *u, IntOrP p) {
  IGNORE(u);
  return ((p.p->ic.op & J_TABLE_BITS) <= J_LAST_JOPCODE) ?
            (p.p->ic.op != J_OPSYSK &&
#ifdef ARM_INLINE_ASSEMBLER
             p.p->ic.op != J_SWI &&
#endif
             p.p->ic.op != J_CALLK && p.p->ic.op != J_CALLR &&
             p.p->ic.op != J_TAILCALLR && p.p->ic.op != J_TAILCALLK) :
           !(a_attributes(p.p->ic.op) & _a_call);
}

static int notleafproc(RegisterUsage *u) {
  IGNORE(u);
  return (procflags & NONLEAF) != 0 && !(pcs_flags & PCS_NOFP) &&
         !(procauxflags & bitoffnaux_(s_irq));
}

static int regset_unused(RegisterUsage *u, IntOrP p) {
  return !((u->use | u->def) & p.i);
}

static int regset_unkilled(RegisterUsage *u, IntOrP p) {
  return !(u->def & p.i);
}

static int movc_pres_r1r2(RegisterUsage *u, IntOrP p1, IntOrP p2) {
  IGNORE(u);
  return movc_preserving_r1r2(p1.p, p2.i != 0);
}

static int difficult_constant(RegisterUsage *u, IntOrP p) {
  IGNORE(u);
  return Arm_EightBits(p.i) < 0;
}

static int config_set(RegisterUsage *u, IntOrP p) {
  IGNORE(u);
  return (config & p.i) == p.i;
}

static int pcs_flags_set(RegisterUsage *u, IntOrP p) {
  IGNORE(u);
  return (pcs_flags & p.i) == p.i;
}

static int physreg_notread(RegisterUsage *u, IntOrP p1, IntOrP p2) {
  IGNORE(u);
  if ((a_uses_r3(p1.p) && p1.p->ic.r3.i == p2.i)
      || (a_reads_r2(p1.p) && p1.p->ic.r2.i == p2.i)
      || (a_reads_r1(p1.p) && p1.p->ic.r1.i == p2.i)
      || (a_uses_r4(p1.p) && p1.p->ic.r4.i == p2.i))
    return NO;
  if (notcall(u, p1))
    return YES;
  if (p2.i < NARGREGS && p2.i < k_argwords_(p1.p->ic.r2.i))
    return NO;
  return YES;
}

static int fp_literal(RegisterUsage *u, IntOrP p1, IntOrP p2) {
  IGNORE(u);
  return fpliteral(p1.p->ic.r3.f, p2.i) != 0;
}

static int asymmetric_fn(RegisterUsage *u, IntOrP p1) {
  IGNORE(u);
  return p1.p->ic.r3.sym == bindsym_(exb_(sim.dsubtract))
         || p1.p->ic.r3.sym == bindsym_(exb_(sim.ddivide))
         || p1.p->ic.r3.sym == bindsym_(exb_(sim.fsubtract))
         || p1.p->ic.r3.sym == bindsym_(exb_(sim.fdivide));
}

static int symmetric_fn(RegisterUsage *u, IntOrP p1) {
  IGNORE(u);
  return p1.p->ic.r3.sym == bindsym_(exb_(sim.dadd))
         || p1.p->ic.r3.sym == bindsym_(exb_(sim.dmultiply))
         || p1.p->ic.r3.sym == bindsym_(exb_(sim.fadd))
         || p1.p->ic.r3.sym == bindsym_(exb_(sim.fmultiply));
}

static int offsetinrange(RegisterUsage *u, IntOrP p1, IntOrP p2) {
  IGNORE(u);

  if ((p2.u & J_TABLE_BITS) >= J_LAST_JOPCODE || !j_is_ldr_or_str(p2.u))
      syserr("offsetinrange %x", p2.u);
  return (p1.i >= MinMemOffset(p2.u) && p1.i <= MaxMemOffset(p2.u));
}


static C0Proc * const cprocs[] = {
  (C0Proc *)notcall,
  (C0Proc *)regset_unused,
  (C0Proc *)regset_unkilled,
  (C0Proc *)notleafproc,
  (C0Proc *)movc_pres_r1r2,
            0, /* NoStackRefsBelow */
  (C0Proc *)difficult_constant,
  (C0Proc *)config_set,
  (C0Proc *)pcs_flags_set,
  (C0Proc *)physreg_notread,
  (C0Proc *)fp_literal,
  (C0Proc *)asymmetric_fn,
  (C0Proc *)symmetric_fn,
  (C0Proc *)offsetinrange
};

static bool MatchOp(PeepOpDef const *p, PendingOp *op) {
  J_OPCODE opc = op->ic.op;
  switch (p->type) {
  default:            syserr(syserr_peep_bad_optype); return NO;
  case pot_and:       return MatchOp(&peepsub1_(p), op) &&
                             MatchOp(&peepsub2_(p), op);
  case pot_or:        return MatchOp(&peepsub1_(p), op) ||
                             MatchOp(&peepsub2_(p), op);
  case pot_andnot:    return MatchOp(&peepsub1_(p), op) &&
                             !MatchOp(&peepsub2_(p), op);
  case pot_prop:      return (bool)((peepprop_(p))(op) != 0);
  case pot_peep:      opc = op->peep;
  case pot_op:        return opc == peepop_(p);
  case pot_peep_m:    opc = op->peep;
  case pot_op_m:      return (opc & peepmask_(p)) == peepopi_(p);
  case pot_opinset_m: opc &= peepmask_(p);
  case pot_opinset:   { int i;
                        int32 const *setp = &peepset_(p);
                        for (i = 0; i < p->setcount; i++)
                          if (opc == (J_OPCODE)setp[i])
                            return YES;
                      }
                      return NO;
  }
}

static int32 UseConstraint(PendingOp *op, int constraint) {
  int32 res = 0;
  int n = pu_r1;
  for (; constraint != 0 && n <= pu_r3mask; n <<= 1)
    if (constraint & n) {
      constraint ^= n;
      switch (n) {
      case pu_r1:     res |= regbit(op->ic.r1.rr); break;
      case pu_r2:     res |= regbit(op->ic.r2.rr); break;
      case pu_r3:     res |= regbit(op->ic.r3.i);  break;
      case pu_r4:     if (a_uses_r4(op))
                        res |= regbit(op->ic.r4.i);
                      break;
      case pu_r3mask: res |= op->ic.r3.i; break;
      }
    }
  if (constraint != 0) syserr(syserr_bad_useconstraint);
  return res;
}

static bool SatisfiedConstraint(
    PendingOp * ops[], PeepHole const *ph, int n, RegisterUsage *u, int i, bool okifinvalid) {

#define tracing(p) (localcg_debug(2) && (p)->trace)

#define CFailI2(op) \
{ if (tracing(ph)) \
    cc_msg("constraint %d fails: !(%ld %s %ld)\n", \
           i+1, (long)f1.i, op, (long)f2.i); \
  return NO; \
}

  PeepConstraint const *c = &ph->constraint[i];
  int32 ignore;
  IntOrP f1, f2;
  if (!ValidField(ops, f1type_(c), f1_(c), n, &f1, &ignore) ||
      !ValidField(ops, f2type_(c), f2_(c), n, &f2, &ignore))
    return okifinvalid;

  switch (pc_type_(c)) {
  case pct_eq:   if (f1.i == f2.i) return YES; CFailI2("==");
  case pct_ne:   if (f1.i != f2.i) return YES; CFailI2("!=");
  case pct_lt:   if (f1.i < f2.i) return YES; CFailI2("<");
  case pct_le:   if (f1.i <= f2.i) return YES; CFailI2("<=");
  case pct_gt:   if (f1.i > f2.i) return YES; CFailI2(">");
  case pct_ge:   if (f1.i >= f2.i) return YES; CFailI2(">=");
  case pct_ltu:  if (f1.u < f2.u) return YES; CFailI2("<u");
  case pct_leu:  if (f1.u <= f2.u) return YES; CFailI2("<=u");
  case pct_gtu:  if (f1.u > f2.u) return YES; CFailI2(">u");
  case pct_geu:  if (f1.u >= f2.u) return YES; CFailI2(">=u");

  case pct_negate: return !SatisfiedConstraint(ops, ph, n, u, (int)f1.i, !okifinvalid);
  case pct_or:   return SatisfiedConstraint(ops, ph, n, u, (int)f1.i, okifinvalid) ||
                        SatisfiedConstraint(ops, ph, n, u, (int)f2.i, okifinvalid);

  case pct_proc0:if (((C0Proc *)cprocs[pc_fn_(c)])(u)) return YES;
                 if (tracing(ph)) cc_msg("constraint %d fails\n", i);
                 return NO;
  case pct_proc1:if (((C1Proc *)cprocs[pc_fn_(c)])(u, f1)) return YES;
                 if (tracing(ph)) cc_msg("constraint %d fails\n", i);
                 return NO;
  case pct_proc2:if (pc_fn_(c) != pcp_nostackrefsbelow) {
                   if (((C2Proc *)cprocs[pc_fn_(c)])(u, f1, f2)) return YES;
                   if (tracing(ph)) cc_msg("constraint %d fails\n", i);
                 } else {
                   if (NoStackReferencesBelow(f1.p, ops[-1], f2.i)) return YES;
                   if (tracing(ph))
                     cc_msg("constraint %d fails: stack references in (%d, %d) below %ld\n",
                            i, f1.p-pendingstack, ops[-1]-pendingstack, (long)f2.i);
                 }
                 return NO;
  }
  return YES;
}

static int IgnoreField(int32 field) {
  return (pf_r1 <= field && field <= pf_r4) ? aru_fieldbit_(field)
                                            : 0;
}

static int FieldsToIgnore(PeepHole const *ph, int inst, int flags) {
  int n = ph->constraintcount;
  PeepConstraint const *pc = ph->constraint;
  for (; --n >= 0; pc++) {
    if (pc->type == pct_eq) {
      if (f1type_(pc) == pft_field && inst_(f1_(pc)) == inst) {
        flags |= IgnoreField(field_(f1_(pc)));
      }
      if (f2type_(pc) == pft_field && inst_(f2_(pc)) == inst) {
        flags |= IgnoreField(field_(f2_(pc)));
      }
    }
  }
  { PeepOp const *p = &ph->insts[inst];
    PeepReplace const *r = &replacements[p->replaceix];
    for (n = p->replacecount; --n >= 0; r++)
      if (pr_type_(r) == prt_set)
        flags |= IgnoreField(pr_field_(r));
  }
  return flags;
}

static bool MayMatch(PendingOp *ops[], PeepOp const peepops[],
                     PeepHole const *ph, int n, RegisterUsage *u) {
  int i;

  if (!MatchOp(&peepops[n].p, ops[n]) ||
      (ops[n]->dataflow & dead_bits[peepops[n].dead]) != dead_bits[peepops[n].dead])
    return NO;

  if (tracing(ph)) {
    cc_msg("%d: match %d ", ph - patterns+1, n);
    if (n != 0) cc_msg("at %d ", ops[n] - pendingstack);
  }

  for (i = 0; i < ph->constraintcount; i++)
    if (!SatisfiedConstraint(ops, ph, n, u, i, YES))
      return NO;

  if (n+1 == ph->instcount) {
    RealRegUse reg;
    RegisterUsage maynot, ucopy;
    ucopy = *u;
    maynot.use = 0;
    RealRegisterUse(&ops[n]->ic, &reg);     /* this take the corrupts with outputs into account */
    maynot.def = reg.c_out.map[0];
    for (i = 0; i <= n; i++) {
      PeepOp const *p = &peepops[i];
      PendingOp o; o = *(ops[i]);
      if (UpdateOp(&o, ops, ph, p->replacecount, p->replaceix, u, YES) & Op_Cant) return NO;
      if (tracing(ph)) { cc_msg("updated: "); a_pr_jopcode_nodeadbits(&o); }
      if (i == 0) {
        int ignore = FieldsToIgnore(ph, 0, aru_ignorepreuse);
        if (tracing(ph)) cc_msg("Ignore %x\n", ignore);
        AccumulateRegisterUse(&ucopy, &o, ignore);
      }
      maynot.use |= UseConstraint(ops[i], peepops[i].p.maynotuse);
      maynot.def |= UseConstraint(ops[i], peepops[i].p.maynotkill);
    }
    if ((ucopy.use & maynot.use) || (ucopy.def & maynot.def)) {
      if (tracing(ph))
        cc_msg("use constraint failure: ucopy <%lx %lx> maynot <%lx %lx>\n",
                (long)ucopy.use, (long)ucopy.def,
                (long)maynot.use, (long)maynot.def);
      return NO;
    }
  }

  if (tracing(ph)) cc_msg("constraints ok\n");
  return YES;
}

static void flush_pending(int leave)
{ PendingOp *p = &pendingstack[0];
  for (; p <= pending-leave; p++)
    if (p->ic.op != J_NOOP)
      show_inst_direct(p);
  if (leave == 0)
  { pending = &pendingstack[0];
    INIT_IC(pending->ic, J_NOOP);
  } else {
    int i;
    for (i = 0; i < leave; i++)
    { pendingstack[i] = pending[i-leave+1];
      pr_res(&pendingstack[i], 0);
    }
    pending = &pendingstack[leave-1];
  }
}

static bool InterferingStore(PendingOp *cur, PendingOp *prev) {
  if ((prev->ic.op & ~J_ALIGNMENT) == J_STRK && prev->ic.r2.rr == cur->ic.r2.rr && prev->ic.r3.i != cur->ic.r3.i)
    return NO;
  return a_modifies_mem(prev) != 0;
}

static void KillDeadBits(PendingOp **ops, int depth, RealRegister r, int ix) {
  /* Remove deadbit(s) from register r up to the point r is killed in the
   * pending ops ops[0] upwards till ops[depth].
   */
  /* WD: Note that this implemenation is not perfect in the sense that it can wrongly
     remove deadbits. This is because the current implemenation doesn't check
     ALL possible registers an instruction reads/writes. It will skip LDM/STM for
     example. However, this will not result in disaster, since it is *removing*
     deadbits and not adding. Nonetheless, a correct implementation is highly desirable...
  */
  PendingOp *limit = ops[depth];
  PendingOp *p = ops[0];
  bool cleared = NO;
  int32 olddataflow;
  for (; !cleared && p >= limit; p--) {
    /* If opnd1 is killed (a_loads_r1) and the realregister r = r1, then we clear
     * the dead bit and stop immediately. Idem for opnd2. (p->r1 != p->r2)
     * For uses, we have to check all operands because a real register may
     * be used more than once and thus can have more than one deadbit set.
     * If a register's deadbit is set, we know it's being used, so we can omit
     * the a_read_rx check.
     */
    olddataflow = p->dataflow;
    if (a_loads_r1(p) && p->ic.r1.rr == r){
       p->dataflow &= ~J_DEAD_R1;
       break;
    }
    if (a_loads_r2(p) && p->ic.r2.rr == r) {
       p->dataflow &= ~J_DEAD_R2;
       break;
    }
    if ((p->dataflow & J_DEAD_R1) && p->ic.r1.rr == r) {
       p->dataflow &= ~J_DEAD_R1;
       cleared = YES;
    }
    if ((p->dataflow & J_DEAD_R2) && p->ic.r2.rr == r) {
       p->dataflow &= ~J_DEAD_R2;
       cleared = YES;
    }
    if ((p->dataflow & J_DEAD_R3) && p->ic.r3.rr == r) {
      p->dataflow &= ~J_DEAD_R3;
      cleared = YES;
    }
    if ((p->dataflow & J_DEAD_R4) && p->ic.r4.rr == r) {
      p->dataflow &= ~J_DEAD_R4;
      cleared = YES;
    }
  }
  if (p >= limit && olddataflow != p->dataflow)
    pr_res(p, ix+1);
}

static PendingOp *peephole_jopcode(int pat, PendingOp *limit, PendingOp *cur, bool toplevel)
{
  PendingOp *ops[MaxInst+1];
  PeepOp const *peepops;
  RegisterUsage use;
  PendingOp dummy[MaxInst];
  int peepcount;
  int const *peepv;
  int peepix = 0; /* shut up compiler */
  int depth, second;
  PeepHole const *curpeep;
  ops[1] = cur;
retry:
  ops[0] = limit;
  peepcount = peepholeperop[cur->ic.op & J_TABLE_BITS].i;
  peepv = peepholeperop[cur->ic.op & J_TABLE_BITS].peepv;
  pr_cur(pat, cur, limit);
  for (pat = 0; pat < peepcount; pat++) {
    RealRegUse reg;
    PendingOp *prev;
    int matched = 0;
    peepix = peepv[pat];
    curpeep = &patterns[peepix];
    Profiler_Count_Index = peepix;
    peepops = curpeep->insts;
    use.def = use.use = 0;
    depth = 0;
    for (;;) {
      if (peepops[depth].p.type == pot_none) {
        if (!toplevel) goto next_pattern;
        ++depth;
        ops[depth] = &dummy[depth-matched-1];
        INIT_IC(dummy[depth-matched-1].ic, J_NOOP);
        dummy[depth-matched-1].peep = 0;
        dummy[depth-matched-1].dataflow = 0;
      } else {
        if (matched++ != 0) break;
        if (!MayMatch(ops+1, peepops, curpeep, depth, &use))
          goto next_pattern;
        ops[++depth] = cur;
      }
      RealRegisterUse(&ops[depth]->ic, &reg);
      use.def = reg.c_in.map[0];           /* record the corrupt inputs of the first instr */
      if (curpeep->instcount == depth) {
        second = depth;
        goto peephole_found;
      }
    }
    second = depth;
    for (prev = limit;
         pending - prev < PeepholeWindowSize;
         prev--) {
      (ops+1)[depth] = prev;
      if (MayMatch(ops+1, peepops, curpeep, depth, &use)) {
        if (++depth == curpeep->instcount)
          goto peephole_found;
        if (depth == MaxInst) syserr(syserr_bad_maxinst);
      } else if (curpeep->gapconstraint == G_ANY  /* ops required to be adjacent */
                 || ((curpeep->gapconstraint & G_POSSIBLEALIASSTR)
                     && InterferingStore(cur, prev))
                 || ((curpeep->gapconstraint & G_ANYSTR)
                     && a_modifies_mem(prev))
                 || ((curpeep->gapconstraint & G_NONSTACKMEM)
                     && a_uses_mem(prev)
                     && !a_uses_stack(prev))
                 || (!(curpeep->gapconstraint & G_ALLOWCONDEXEC)
                     && (prev->ic.op & ~Q_MASK) == J_CONDEXEC)
                        /* allowed only if matched in pattern */
                 || (prev->ic.op & ~Q_MASK) == J_B
                 || prev->ic.op == J_LABEL
                 || prev->ic.op == J_STACK
                        /* likewise */
                )
        break;
      if (prev == pendingstack) /* run out of ops */
        break;
      AccumulateRegisterUse(&use, prev, aru_updatekills);
    }
next_pattern:;
  }
  goto no_peephole_found;

peephole_found:
  {
    PendingOp opcopy[MaxInst];
    PendingOp *opp[MaxInst+1];
    int d;
#ifdef ENABLE_LOCALCG
    p_count[peepix]++;
#endif
    if (depth > second)
      for (d = 0; d < depth; d++) {
        PendingOp *op = (ops+1)[d];
        if (peepops[d].p.maynotkill & pu_r1) KillDeadBits(ops, depth, op->ic.r1.rr, peepix);
        if (peepops[d].p.maynotkill & pu_r2) KillDeadBits(ops, depth, op->ic.r2.rr, peepix);
        if (peepops[d].p.maynotkill & pu_r3) KillDeadBits(ops, depth, op->ic.r3.rr, peepix);
        if (peepops[d].p.maynotkill & pu_r4) KillDeadBits(ops, depth, op->ic.r4.rr, peepix);
      }
    opp[0] = ops[0];
    for (d = 1; d <= depth; d++) {
      opcopy[d-1] = *ops[d];
      opp[d] = &opcopy[d-1];
    }
    for (d = depth; --d >= 0; ) {
      PendingOp *op = (ops+1)[d];
      if (UpdateOp(op, opp+1, curpeep,
                   peepops[d].replacecount, peepops[d].replaceix, &use, NO) & Op_Altered) {
        if (d == 0) {
          if (op->ic.op != J_NOOP) {
            pat = peepix;
            goto retry;
          }

        } else if (d < second) {
          /* new op being added to top of pending stack.  If it's not the
             only one, we must take care to maintain the 'always one slot
             free' guarantee.  The call to flush_pending will invalidate
             any pointers to pendingstack we are holding, but we're not
             going to look at them again (since the loop on d is from lowest
             upward)
           */
          if (op->ic.op != J_NOOP)
            limit = peephole_jopcode(peepix, limit, op, NO);
          if (d > 0 && limit == &pendingstack[PendingStackSize-1]) {
            pending++;
            flush_pending(PeepholeWindowSize+2);
            limit = pending;
          }
        } else if (op->ic.op != J_NOOP && op != &pendingstack[0]) {
          PendingOp temp; temp = *op;
          INIT_IC(op->ic, J_NOOP);
          op->peep = 0;
          op->dataflow = 0;
          (void)peephole_jopcode(peepix, op-1, &temp, NO);
        } else {
          pr_res(op, peepix+1);
        }
      } else if (tracing(curpeep)) {
        cc_msg("unchanged ");
        pr_res(op, peepix+1);
      }
    }
  }
no_peephole_found:
  if (cur->ic.op != J_NOOP) {
    if (limit < pendingstack || limit->ic.op != J_NOOP) limit++;
    if (limit >= &pendingstack[PendingStackSize])
      syserr(syserr_pendingstack_overflow);
    *limit = *cur;
    pr_res(limit, pat == peepcount ? 0 : peepix+1);
  }
  Profiler_Count_Index = 0;
  return limit;
}

/* Exported routines...                                               */

/* The peepholer: */

void peephole_op(PendingOp *cur, bool flush) {
  if (localcg_debug(5)) a_pr_jopcode (cur);
  if (var_cc_private_flags & 8L) {
    show_inst_direct(cur);
  } else {
    pending = peephole_jopcode(-1, pending, cur, YES);
    if (flush)
      flush_pending(0);
    else if (pending == &pendingstack[PendingStackSize-1])
      flush_pending(PeepholeWindowSize+2);
  }
}

void peephole_reinit(void) {
  pendingstack = (PendingOp *)SynAlloc(PendingStackSize * sizeof(PendingOp));
  pending = &pendingstack[0];
  INIT_IC(pending->ic, J_NOOP);
  pending->peep = 0;
  pending->dataflow = 0;
}

void peephole_init(void) {
#ifdef ENABLE_LOCALCG
  int i;
  for (i = 0; i <= PeepholeMax; i++) p_count[i] = 0;
#endif
}

void peephole_tidy(void) {
#ifdef ENABLE_LOCALCG
  if (localcg_debug(1) || debugging(DEBUG_STORE))
  { int i;
    for (i = 0; i <= PeepholeMax; i++)
      if (p_count[i] != 0)
        cc_msg("{%3d}%6ld\n", i+1, p_count[i]);
  }
#endif
}
