/*
 * C compiler file thumb/peephole.c.
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
#include "host.h"
#include "options.h"
#include "mcdep.h"
#include "mcdpriv.h"
#include "aeops.h"
#include "ops.h"
#include "jopcode.h"
#include "store.h"
#include "errors.h"
#include "cg.h"        /* for procflags, greatest_stackdepth */
#include "regalloc.h"  /* regmask */
#include "simplify.h"  /* for MCR_SIZE_MASK */

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
#define p_value_unchanged(u, a)  (!((u)->kill & regbit(a)))
#define p_reg_unused(u, a)       (!(((u)->use | (u)->kill) & regbit(a)))
#define p_psr_unchanged(u)       (!((u)->kill & M_PC))
#define p_regset_unused(u, a)    (!(((u)->use | (u)->kill) & (a)))
#define p_regset_unchanged(u, a) (!((u)->kill & (a)))

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

#define pep_argct_log2 1
#define pep_argct_bit 1
#define pep_argct_bitcount 1
#define pep_argct_lsb 1
#define pep_argct_lsbpair 1
#define pep_argct_shift_p 1
#define pep_argct_shift_k 2

typedef enum PE_Proc {
  pep_bit,
  pep_bitcount,
  pep_lsb,
  pep_lsbpair,
  pep_log2,
  pep_shift_k,
  pep_shift_p,
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

#define peepop_(p) ((p)->n)
#define peepprop_(p) (peepprops[(p)->n])
#define peepsub1_(p) (peepsubs[((p)->n)>>16])
#define peepsub2_(p) (peepsubs[((p)->n)&0xffff])
#define peepset_(p) (peepsets[((p)->n)&0xffff])
#define peepopi_(p) (peepsets[((p)->n)&0xffff])
#define peepmask_(p) (peepsets[((p)->n)>>16])

#define G_ANY 0xff
#define G_STR 1

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
  pcp_config
} PC_Proc;

#define pcp_argct_notcall 1
#define pcp_argct_regset_unused 1
#define pcp_argct_regset_unkilled 1
#define pcp_argct_notleafproc 0
#define pcp_argct_movc_pres_r1r2 2
#define pcp_argct_nostackrefsbelow 2
#define pcp_argct_difficult_constant 1
#define pcp_argct_config 1

typedef int C0Proc(RegisterUsage *);
typedef int C1Proc(RegisterUsage *, IntOrP);
typedef int C2Proc(RegisterUsage *, IntOrP, IntOrP);

#define RETLABV ((int32)RETLAB)

#define p_dead_r1 1
#define p_dead_r2 2
#define p_dead_r3 4

static int32 const dead_bits[] = { /* translation from p_dead_xx */
   0,         J_DEAD_R1,           J_DEAD_R2,           J_DEAD_R1+J_DEAD_R2,
   J_DEAD_R3, J_DEAD_R3+J_DEAD_R1, J_DEAD_R3+J_DEAD_R2, J_DEAD_R3+J_DEAD_R1+J_DEAD_R2
};

#define M_SP regbit(R_SP)

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
  return op == J_MOVC || op == J_CLRC;
}

static void a_pr_jopcode_nodeadbits(PendingOp *p)
{
  J_OPCODE op = p->ic.op;
  if ((op & J_TABLE_BITS) <= J_LAST_JOPCODE)
    print_jopcode_1(&p->ic);
  else {
    char v[20];
    unsigned32 attr = a_attributes(op);
    strcpy(v, a_joptable[(op & J_TABLE_BITS)-J_LAST_JOPCODE-1].name);
    if (op & J_ALIGNMENT)
        strcat(v, ((op & J_ALIGNMENT) >> J_ALIGNPOS)*3 + "a1\0a2\0a4\0a8");
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
  }
  if (uses_r4_field(op))
    cc_msg(", %#lx", (long)p->ic.r4.rr);
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
  cc_msg("%c%c%c", (p->dataflow & J_DEAD_R1 ? '1': '-'),
                   (p->dataflow & J_DEAD_R2 ? '2': '-'),
                   (p->dataflow & J_DEAD_R3 ? '3': '-'));
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
  } else if (a_reads_r1(c) && !(flags & aru_ignore_r1))
    u->use |= regbit(c->ic.r1.rr);

  if (a_loads_r2(c)) {
    if (flags & aru_updatekills)
      u->def |= regbit(c->ic.r2.rr);
  } else if (a_reads_r2(c) && !(flags & aru_ignore_r2)) {
      if (!(c->peep & P_PRE && (flags & aru_ignorepreuse)))
        u->use |= regbit(c->ic.r2.rr);
  }
  if (a_uses_r3(c) && !(flags & aru_ignore_r3))
    u->use |= regbit(c->ic.r3.i);
  if (a_uses_r4(c) && !(flags & aru_ignore_r4))
    u->use |= regbit(c->ic.r4.rr);
  if (sets_psr(&c->ic) || ((c->peep & P_CMPZ)) &&
      (flags & aru_updatekills))
    u->def |= regbit(R_PSR);
  if (reads_psr(&c->ic))
      u->use |= regbit(R_PSR);
  if (corrupts_r1(&c->ic) && (flags & aru_updatekills))
      u->def |= regbit(c->ic.r1.rr);
  if (corrupts_r2(&c->ic) && (flags & aru_updatekills))
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
            op == J_MOVC)) ||
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

/* $$$$$ why are the assumptions this makes valid?? */

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

/* On arm, this produces a shift code which is later used in eg
   add r1, r2 , r3 lsl r4. Thumb doesn't have that flexibility so
   try not to generate them.
*/
static bool p_shift_p(IntOrP *resp, IntOrP r1) {
  syserr("p_shift_p called\n");
  IGNORE(r1);
  resp->i = 0;
  return YES;
}

/* No scaled addressing on thumb.
*/
static bool p_shift_k(IntOrP *resp, IntOrP r1, IntOrP r2) {
  syserr("p_shift_k called\n");
  IGNORE(r1); IGNORE(r2);
  resp->i = 0;
  return YES;
}

static int32 opfield(PendingOp *op, P_Field field, int32 *dead) {
  switch (field) {
  case pf_op:   return op->ic.op;
  case pf_peep: return op->peep;
  case pf_r1:   *dead = a_uses_r1(op) && (op->dataflow & J_DEAD_R1); return op->ic.r1.rr;
  case pf_r2:   *dead = a_uses_r2(op) && (op->dataflow & J_DEAD_R2); return op->ic.r2.rr;
  case pf_r3:   *dead = a_uses_r3(op) && (op->dataflow & J_DEAD_R3); return op->ic.r3.i;
  case pf_r4:   return op->ic.r4.rr;
  case pf_dataflow: return op->dataflow;
  case pf_cond: return op->cond;
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
  case pf_r4:   op->ic.r4.rr = val; break;
  case pf_dataflow: op->dataflow = val; break;
  case pf_cond: op->cond = val; break;
  default:      syserr(syserr_peep_bad_field);
                break;
  }
}

static int32 readsfield(const PendingOp *const p, P_Field field) {
  switch (field) {
  case pf_r1: return a_reads_r1(p);
  case pf_r2: return a_reads_r2(p);
  case pf_r3: return a_uses_r3(p);
  case pf_r4: return a_uses_r4(p);
  default:    return NO;
  }
}

static bool isregfield(PendingOp *op, P_Field field) {
  return (field == pf_r1 && a_loads_r1(op)) ||
         readsfield(op, field);
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


#define DummyReg 31

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
      op->ic.op = J_NOOP;
      op->peep = 0;
      op->dataflow = 0;
      result = Op_Altered;
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
      if (!check_only)
      {
        ValidField(ops, pr_valtype_(r), pr_val_(r), p->instcount-1, &val, &dead);
        AdjustStackRefs(op+1, ops[-1], val.i, p-patterns+1);
        result |= Op_Altered;
      }
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
  IGNORE(u); IGNORE(p);
  /* $$$$$ on ARM, see if a constant will fit into a 12 bit immediate
     field and return it or -1 if it won't */
/*  return eightbits(p.i) < 0; */
  syserr("difficult_constant called");
  return 1;
}

static int config_set(RegisterUsage *u, IntOrP p) {
  IGNORE(u);
  return (config & p.i) == p.i;
}

static C0Proc * const cprocs[] = {
  (C0Proc *)notcall,
  (C0Proc *)regset_unused,
  (C0Proc *)regset_unkilled,
  (C0Proc *)notleafproc,
  (C0Proc *)movc_pres_r1r2,
            0, /* NoStackRefsBelow */
  (C0Proc *)difficult_constant,
  (C0Proc *)config_set
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
  case pot_prop:      return (bool)((peepprop_(p))(op));
  case pot_peep:      opc = op->peep;
  case pot_op:        return opc == peepop_(p);
  case pot_peep_m:    opc = op->peep;
  case pot_op_m:      return (opc & peepmask_(p)) == peepopi_(p);
  case pot_opinset_m: opc &= peepmask_(p);
  case pot_opinset:   { int i;
                        int32 const *setp = &peepset_(p);
                        for (i = 0; i < p->setcount; i++)
                          if (opc == setp[i])
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
                        res |= regbit(op->ic.r4.rr);
                      break;
      case pu_r3mask: res |= op->ic.r3.i;
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

static int FieldsToIgnore(PeepHole const *ph, int inst, int flags) {
  int n = ph->constraintcount;
  PeepConstraint const *pc = ph->constraint;
  for (; --n >= 0; pc++) {
    if (pc->type == pct_eq) {
      if (f1type_(pc) == pft_field && inst_(f1_(pc)) == inst) {
        P_Field field = field_(f1_(pc));
        if (pf_r1 <= field && field <= pf_r4)
          flags |= aru_fieldbit_(field);
      }
      if (f2type_(pc) == pft_field && inst_(f2_(pc)) == inst) {
        P_Field field = field_(f2_(pc));
        if (pf_r1 <= field && field <= pf_r4)
          flags |= aru_fieldbit_(field);
      }
    }
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
    RealRegisterUse(&ops[n]->ic, &reg);
    maynot.def = reg.c_out.map[0];     /* this takes the corrupts with outputs into account */
    for (i = 0; i <= n; i++) {
      PeepOp const *p = &peepops[i];
      PendingOp o; o = *(ops[i]);
      if (UpdateOp(&o, ops, ph, p->replacecount, p->replaceix, u, YES) & Op_Cant) return NO;
      if (tracing(ph)) { cc_msg("updated: "); a_pr_jopcode_nodeadbits(&o); }
      if (i == 0) AccumulateRegisterUse(&ucopy, &o, FieldsToIgnore(ph, 0, aru_ignorepreuse));
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
    pending->ic.op = J_NOOP;
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
#if 0
  bool r;

  cc_msg("%08x: InterferingStore, op = %08x, r1 = %d, r2 = %d, m = %d", codebase + codep, prev->ic.op, prev->ic.r1.rr, prev->ic.r2.rr, prev->ic.m.i);
#endif
  if ((prev->ic.op & ~J_ALIGNMENT) == J_STRK && prev->ic.r2.rr == cur->ic.r2.rr && prev->ic.r3.i != cur->ic.r3.i) {
#if 0
    cc_msg(" -> NO\n");
#endif
    return NO;
  }
#if 0
  r = a_modifies_mem(prev->ic.op) != 0;
  cc_msg(" -> %d\n", r);
  return r;
#else
  return a_modifies_mem(prev) != 0;
#endif
}

static void KillDeadBits(PendingOp **ops, int depth, RealRegister r, int ix) {
  PendingOp *limit = ops[depth];
  PendingOp *p = ops[0];
  for (; p >= limit; p--) {
    if (a_reads_r1(p) && p->ic.r1.rr == r && (p->dataflow & J_DEAD_R1)) {
      p->dataflow &= ~J_DEAD_R1; break;
    }
    if (a_uses_r2(p) && p->ic.r2.rr == r && (p->dataflow & J_DEAD_R2)) {
      p->dataflow &= ~J_DEAD_R2; break;
    }
    if (a_uses_r3(p) && (RealRegister)p->ic.r3.i == r && (p->dataflow & J_DEAD_R3)) {
      p->dataflow &= ~J_DEAD_R3; break;
    }
  }
  if (p >= limit) pr_res(p, ix+1);
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
    PendingOp *prev;
    int matched = 0;
    peepix = peepv[pat];
    curpeep = &patterns[peepix];
    Profiler_Count_Index = peepix;
    peepops = curpeep->insts;
    use.def = use.use = 0;
    depth = 0;
    for (;;) {
      RealRegUse reg;
      if (peepops[depth].p.type == pot_none) {
        if (!toplevel) goto next_pattern;
        ++depth;
        ops[depth] = &dummy[depth-matched-1];
        dummy[depth-matched-1].ic.op = J_NOOP;
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
      } else if (curpeep->gapconstraint == G_ANY ||  /* ops required to be adjacent */
                 ((curpeep->gapconstraint & G_STR) && InterferingStore(cur, prev)) ||
                 (prev->ic.op & ~Q_MASK) == J_CONDEXEC /* allowed only if matched in pattern */)
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
        if (peepops[d].p.maynotkill & pu_r1) KillDeadBits(ops, depth, (ops+1)[d]->ic.r1.rr, peepix);
        if (peepops[d].p.maynotkill & pu_r2) KillDeadBits(ops, depth, (ops+1)[d]->ic.r2.rr, peepix);
        if (peepops[d].p.maynotkill & pu_r3) KillDeadBits(ops, depth, (ops+1)[d]->ic.r3.i, peepix);
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
          op->ic.op = J_NOOP;
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
  if (var_cc_private_flags & 8L) {
#ifdef ACDEBUG2
    print_jopcode(cur->ic);
#endif
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
  pending->ic.op = J_NOOP;
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

