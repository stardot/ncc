/*
 * asmcg.c: code generation for ARM/Thumb inline assembler
 * Copyright (C) Advanced Risc Machines Ltd., 1997. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "globals.h"
#include "cg.h"
#include "store.h"
#include "codebuf.h"
#include "aeops.h"
#include "util.h"
#include "xrefs.h"
#include "jopcode.h"
#include "regalloc.h"
#include "regsets.h"
#include "cse.h"
#include "sr.h"
#include "flowgraf.h"
#include "mcdep.h"
#include "aetree.h"
#include "builtin.h"
#include "sem.h"       /* typeofexpr */
#include "syn.h"
#include "simplify.h"  /* mcrepofexpr */
#include "bind.h"
#include "errors.h"
#include "inline.h"
#include "armops.h"
#include "lex.h"
#include "defs.h"
#include "cgdefs.h"
#include "mcdpriv.h"
#include "arminst.h"

#include "inlnasm.h"






int32 trans_cc_tab [16] = {
    Q_EQ,
    Q_NE,
    Q_HS,
    Q_LO,
    Q_MI,
    Q_PL,
    Q_VS,
    Q_VC,
    Q_HI,
    Q_LS,
    Q_GE,
    Q_LT,
    Q_GT,
    Q_LE,
    Q_AL,
    Q_NOT  /* not used */
};

#ifdef ARM_INLINE_ASSEMBLER

J_OPCODE arm_op_tab [NUM_ARM_OPCODES] = {
    J_ANDK,
    J_EORK,
    J_SUBK,
    J_RSBK,
    J_ADDK,
    J_ADCK,
    J_SBCK,
    J_RSCK,
    J_TSTK,
    J_TEQK,
    J_CMPK,
    J_CMNK,
    J_ORRK,
    J_MOVK,
    J_BICK,
    J_MVNK
};

#endif




J_OPCODE thumb_op_tab [NUM_ARM_OPCODES] = {
    J_ANDR,
    J_EORR,
    J_SUBR,
    J_NOOP,
    J_ADDR,
    J_ADCR,
    J_SBCR,
    J_NOOP,
    J_TSTR,
    J_NOOP,
    J_CMPR,
    J_CMNR,
    J_ORRR,
    J_MOVR,
    J_BICR,
    J_MVNR,

    J_NOOP,
    J_NOOP,

    J_MULR

};





static bool ccflags_altered;
static uint32 cur_cond;
static LabelNumber *cur_label;


static void cg_asm_init(void)
{
    procflags |= PROC_INLNASM;
    ccflags_altered = NO;
    cur_label = NULL;
    cur_cond = CC_AL;
}


static void cg_asm_finish(void)
{
    if (cur_label != NULL)
    {
        emitbranch (J_B, cur_label);
        start_new_basic_block (cur_label);
    }
}

static void asm_err(msg_t msg)
{
    cc_err(msg);
}

static VRegnum expr_reg(Expr *e)
{
    if (e == 0) return GAP;
    if (h0_(e) == s_register) return intval_(e);
    return cg_expr(e);
}

static void free_reg(Expr *e, VRegnum r)
{
    if (e == 0 || h0_(e) == s_register || r == GAP) return;
    bfreeregister(r);
}

static VRegnum get_reg(Expr *e)
{
    if (e == 0) return GAP;
    if (h0_(e) == s_register) return intval_(e);
    return fgetregister(INTREG);
}


static VRegnum expr_loreg(Expr *e)
{   /* Version which only allows low registers */
    if (e == 0) return GAP;
    if (h0_(e) == s_register)
    {   if (intval_(e) >= 8)
        {   cc_err(asm_err_thumb_highreg, intval_(e));
            return 0;
        }
        return intval_(e);
    }
    return cg_expr(e);
}

static VRegnum expr_reg_sp(Expr *e)
{   /* Version which only allows low registers, and SP */
    if (e == 0) return GAP;
    if (h0_(e) == s_register)
    {   if (intval_(e) >= 8 && intval_(e) != R_SP)
        {   cc_err(asm_err_thumb_highreg, intval_(e));
            return 0;
        }
        return intval_(e);
    }
    return cg_expr(e);
}



static void store_reg(Expr *e, VRegnum r)
{   /* Store a register into variable 'e'. */
    if (e == 0 || h0_(e) == s_register)
    {   if (r == R_PC)
            cc_err(asm_err_write_pc);
        return;
    }
    cg_storein(r, NULL, e, s_assign);
    bfreeregister(r);
}

static void store_reg_psr(Expr *e, VRegnum r)
{   /* Version for MRC which may write to PC (PSR part) */
    if (e == 0 || h0_(e) == s_register)
        return;
    cg_storein(r, NULL, e, s_assign);
    bfreeregister(r);
}

static void store_reg_pc(Expr *e, VRegnum r)
{   /* Version for instructions which may write to pc.
     * We don't allow it...
     */
    if (e == 0 || h0_(e) == s_register)
    {   if (r == R_PC)
            cc_err(asm_err_branch_pc);
        return;
    }
    cg_storein(r, NULL, e, s_assign);
    bfreeregister(r);
}

static void store_loreg(Expr *e, VRegnum r)
{   /* Do not allow high registers. */
    if (e == 0 || h0_(e) == s_register)
    {   if (r >= 8)
            cc_err(asm_err_thumb_highreg, r);
        return;
    }
    cg_storein(r, NULL, e, s_assign);
    bfreeregister(r);
}

static void store_reg_sp(Expr *e, VRegnum r)
{   /* Allow SP, but no other high reg. */
    if (e == 0 || h0_(e) == s_register)
    {   if (r >= 8 && r != R_SP)
            cc_err(asm_err_thumb_highreg, r);
        return;
    }
    cg_storein(r, NULL, e, s_assign);
    bfreeregister(r);
}

static void cg_asm_data(const AsmInstr * const a)
{
    J_OPCODE op;
    VRegnum r1, r2, r3, r4;
    int opnd2_type = asmopcode_(a) & RN_OPND_MASK;

    r1 = get_reg(asmopnd1_(a));
    r2 = expr_reg(asmopnd2_(a));

    switch (INSTRCL(asmopcode_(a)))
    {
        case CL_MOV:
            op = J_AMOVK;
            r2 = 0;
            if ((opnd2_type == RN_REG || opnd2_type == RN_CONST) &&
                !(asmopcode_(a) & SET_CC) && OPCODE(asmopcode_(a)) == A_MOV)
            {
                op = J_MOVK;
                r2 = GAP;
            }
            break;
        case CL_CMP:
            op = J_ACMPK;
            r1 = asmopcode_(a) & SET_PSR ? 15 : 0;
            break;
        case CL_BIN:
            op = J_ABINRK;
            break;
    }
    switch (opnd2_type)
    {
        case RN_CONST:
            r3 = intval_(asmopnd3_(a));
            emitreg4(op, asmopcode_(a), r1, r2, r3, GAP);
            free_reg(asmopnd2_(a), r2);
            break;

        case RN_REG:
            r3 = expr_reg(asmopnd3_(a));
            emitreg4(op+1, asmopcode_(a), r1, r2, r3, GAP);
            free_reg(asmopnd3_(a), r3);
            break;

        case RN_SHIFT:
            r3 = expr_reg(asmopnd3_(a));
            emitreg4(op+2, asmopcode_(a), r1, r2, r3, intval_(asmopnd4_(a)));  /* CHECK RANGE!!!! */
            bfreeregister(r3);
            break;

        case RN_SHIFT_REG:
            r3 = expr_reg(asmopnd3_(a));
            r4 = expr_reg(asmopnd4_(a));
            emitreg4(op+3, asmopcode_(a), r1, r2, r3, r4);
            free_reg(asmopnd3_(a), r3);
            free_reg(asmopnd4_(a), r4);
            break;

        default:
            break;
    }
    free_reg(asmopnd2_(a), r2);
    if (asmopcode_(a) & SET_PSR)
        store_reg_psr(asmopnd1_(a), r1);
    else
        store_reg_pc(asmopnd1_(a), r1);
}


static void cg_asm_mul(const AsmInstr * const a)
{
    VRegnum r1, r2, r3, r4;

    switch (OPCODE (asmopcode_(a)))
    {
        case A_MUL:
            r1 = get_reg(asmopnd1_(a));
            r2 = expr_reg(asmopnd2_(a));
            if (h0_(asmopnd3_(a)) == s_integer)
                emitreg4(J_MULK, asmopcode_(a), r1, r2, intval_(asmopnd3_(a)), 0);
            else
            {   r3 = expr_reg(asmopnd3_(a));
                emitreg4(J_MULR, asmopcode_(a), r1, r3, r2, GAP); /* J_MUL has r2 & r3 swapped */
                free_reg(asmopnd3_(a), r3);
                if (r1 == r2)
                    asm_err(asm_err_mul_conflict);
                if (r3 == R_PC)
                    asm_err(asm_err_pc);
            }
            free_reg(asmopnd2_(a), r2);
            store_reg(asmopnd1_(a), r1);
            if (r1 == R_PC || r2 == R_PC)
                asm_err(asm_err_pc);
            break;

        case A_MLA:
            r1 = get_reg(asmopnd1_(a));
            r2 = expr_reg(asmopnd2_(a));
            r3 = expr_reg(asmopnd3_(a));
            r4 = expr_reg(asmopnd4_(a));
            emitreg4(J_MLAR, asmopcode_(a), r1, r3, r2, r4);    /* J_MLA has r2 & r3 swapped */
            free_reg(asmopnd2_(a), r2);
            free_reg(asmopnd3_(a), r3);
            free_reg(asmopnd4_(a), r4);
            store_reg(asmopnd1_(a), r1);
            if (r1 == r2)
                asm_err(asm_err_mul_conflict);
            if (r1 == R_PC || r2 == R_PC || r3 == R_PC || r4 == R_PC)
                asm_err(asm_err_pc);
            break;

        case A_MULL:
        {
            int32 op;
            r1 = get_reg(asmopnd1_(a));
            r2 = get_reg(asmopnd2_(a));
            r3 = expr_reg(asmopnd3_(a));
            r4 = expr_reg(asmopnd4_(a));
            op = J_MULL;
            if (asmopcode_(a) & M_SIGNED) op |= J_SIGNED;
            emitreg4(op, asmopcode_(a), r1, r2, r3, r4);
            free_reg(asmopnd3_(a), r3);
            free_reg(asmopnd4_(a), r4);
            store_reg(asmopnd1_(a), r1);
            store_reg(asmopnd2_(a), r2);
            if (r1 == r2 || r1 == r3 || r2 == r3)
                asm_err(asm_err_mul_conflict);
            if (r1 == R_PC || r2 == R_PC || r3 == R_PC || r4 == R_PC)
                asm_err(asm_err_pc);
            break;
        }

        case A_MLAL:
        {
            int32 op;
            r1 = expr_reg(asmopnd1_(a));
            r2 = expr_reg(asmopnd2_(a));
            r3 = expr_reg(asmopnd3_(a));
            r4 = expr_reg(asmopnd4_(a));
            op = J_MLAL;
            if (asmopcode_(a) & M_SIGNED) op |= J_SIGNED;
            emitreg4(op, asmopcode_(a), r1, r2, r3, r4);
            free_reg(asmopnd3_(a), r3);
            free_reg(asmopnd4_(a), r4);
            store_reg(asmopnd1_(a), r1);
            store_reg(asmopnd2_(a), r2);
            if (r1 == R_PC || r2 == R_PC || r3 == R_PC || r4 == R_PC)
                asm_err(asm_err_pc);
            break;
        }
    }
}


static void cg_asm_ldmstm(const AsmInstr * const a)
{
    VRegnum r1, r3;
    uint32 op;

    op = OPCODE(asmopcode_(a)) == A_LDM ? J_LDM : J_STM;
    if (asmopcode_(a) & M_WB) op += J_LDMW - J_LDM;
    r1 = expr_reg(asmopnd1_(a));
    r3 = intval_(asmopnd3_(a));
    emitreg4(op, asmopcode_(a), r1, GAP, r3, GAP);
    if (r3 == 0)
        asm_err(asm_err_ldm_empty);
    if (r1 == R_PC
    /* WD; temporary - swallis || ((op == J_LDM || op == J_LDMW) && (r3 & regbit(R_PC))) */
    )
        asm_err(asm_err_pc);
    if (asmopcode_(a) & M_WB)
    {   if (r1 < 16 && regbit(r1) & r3)
            asm_err(asm_err_ldm_base_list);
        store_reg(asmopnd1_(a), r1);
    }
    else
        free_reg(asmopnd1_(a), r1);
}

static void cg_asm_ldrstr(const AsmInstr * const a)
{
    VRegnum r1, r2, r3;
    bool is_load = OPCODE(asmopcode_(a)) == A_LDR;
    uint32 op = is_load ? J_ALDRK : J_ASTRK;

    if (is_load)
        r1 = get_reg(asmopnd1_(a));
    else
        r1 = expr_reg(asmopnd1_(a));
    r2 = expr_reg(asmopnd2_(a));
    if (asmopcode_(a) & M_WB)
        op += J_ALDRKW - J_ALDRK;
    switch (asmopcode_(a) & RN_OPND_MASK)
    {
        case RN_CONST:
            r3 = intval_(asmopnd3_(a));
            emitreg4(op, asmopcode_(a), r1, r2, r3, GAP);
            break;

        case RN_REG:
            op = J_KTOR(op);
            r3 = expr_reg(asmopnd3_(a));
            emitreg4(op, asmopcode_(a), r1, r2, r3, GAP);
            free_reg(asmopnd3_(a), r3);
            break;

        case RN_SHIFT:
            op = J_KTOR(op);
            r3 = expr_reg(asmopnd3_(a));
            emitreg4(op, asmopcode_(a), r1, r2, r3, intval_(asmopnd4_(a)));
            free_reg(asmopnd3_(a), r3);
            break;

        default: ;/* Error */
    }
    if (asmopcode_(a) & M_WB)
    {   if (r1 == r2 && r1 < 16)
            asm_err(asm_err_ldr_base_wb);
        store_reg(asmopnd2_(a), r2);
    }
    else
        free_reg(asmopnd2_(a), r2);
    if (is_load)
        store_reg(asmopnd1_(a), r1);
    else
        free_reg(asmopnd1_(a), r1);
    if (r1 == R_PC && asmopcode_(a) & (M_BYTE | M_HALF))
        asm_err(asm_err_pc);
}


static void cg_psr(const AsmInstr *const a)
{
    VRegnum psr = intval_(asmopnd2_(a));

    if (OPCODE(asmopcode_(a)) == A_MRS)
    {
        VRegnum r1 = get_reg(asmopnd1_(a));
        emitreg4(J_MRS, 0, r1, psr, GAP, GAP);
        store_reg(asmopnd1_(a), r1);
    }
    else if ((psr & PSR_FLAGS) == PSR_F && h0_(asmopnd3_(a)) == s_integer &&
        Arm_EightBits(intval_(asmopnd3_(a))) >= 0)      /* MSR PSR_f, #8 bit const */
    {
        emitreg4(J_MSK, 0, GAP, psr, intval_(asmopnd3_(a)), GAP);
    }
    else                                                /* MSR PSR, reg */
    {
        VRegnum r3 = expr_reg(asmopnd3_(a));
        emitreg4(J_MSR, 0, GAP, psr, r3, GAP);
        free_reg(asmopnd3_(a), r3);
    }
}


static void cg_swi(const AsmInstr *const a)
{
    emitreg4(J_SWI, 0, intval_(asmopnd1_(a)), intval_(asmopnd2_(a)),
                       intval_(asmopnd3_(a)), intval_(asmopnd4_(a)));
}


static void cg_swp(const AsmInstr *const a)
{
    VRegnum r1, r2, r3;
    r1 = get_reg(asmopnd1_(a));
    r2 = expr_reg(asmopnd2_(a));
    r3 = expr_reg(asmopnd3_(a));
    emitreg4((asmopcode_(a) & M_BYTE ? J_SWPB : J_SWP), 0, r1, r2, r3, GAP);
    store_reg(asmopnd1_(a), r1);
    free_reg(asmopnd2_(a), r2);
    free_reg(asmopnd3_(a), r3);
    if (r1 == r3 || r2 == r3)
        asm_err(asm_err_swp_conflict);
    if (r1 == R_PC || r2 == R_PC || r3 == R_PC)
        asm_err(asm_err_pc);

}

static void cg_bl(const AsmInstr *const a)
{   /* Branches are already handled */
    if (OPCODE(asmopcode_(a)) == A_BL)
    {
        emitreg4(J_BL, 0, (int) asmopnd1_(a), intval_(asmopnd2_(a)),
                 intval_(asmopnd3_(a)), intval_(asmopnd4_(a)));
    }
}


static void cg_cop(const AsmInstr *const a)
{
    uint32 coproc = intval_(asmopnd4_(a)) |
        intval_(asmopnd3_(a)) | (intval_(asmopnd2_(a)) << 16);
    VRegnum r1;

    switch (OPCODE(asmopcode_(a)))
    {
        case A_CDP:
            coproc |= intval_(asmopnd1_(a)) << 12;
            emitreg4(J_CDP, 0, GAP, GAP, 0, coproc);
            break;
        case A_MRC:
            r1 = get_reg(asmopnd1_(a));
            emitreg4(J_MRC, 0, r1, GAP, 0, coproc);
            store_reg_psr(asmopnd1_(a), r1);
            break;
        case A_MCR:
            r1 = expr_reg(asmopnd1_(a));
            emitreg4(J_MCR, 0, r1, GAP, 0, coproc);
            free_reg(asmopnd1_(a), r1);
            break;
    }
}


static void cg_cmem(const AsmInstr *const a)
{
    uint32 coproc = intval_(asmopnd4_(a)) |
        (intval_(asmopnd1_(a)) << 12);
    uint32 op;
    VRegnum r2;

    op = (OPCODE(asmopcode_(a)) == A_STC) ? J_STC : J_LDC;
    if (asmopcode_(a) & M_WB) op += J_LDCW - J_LDC;
    r2 = expr_reg(asmopnd2_(a));
    emitreg4(op, asmopcode_(a), GAP, r2, intval_(asmopnd3_(a)) >> 2, coproc);
    if (asmopcode_(a) & M_WB)
        store_reg(asmopnd2_(a), r2);
    else
        free_reg(asmopnd2_(a), r2);
}


/* TODO: cleanup branches and labels -> can be a lot simpler! */

void cg_arm_instr(const AsmInstr *const a)
{
    uint32 cond = GETCC(asmopcode_(a));

    if (usrdbg(DBG_LINE))
        emitfl(J_INFOLINE, a->fl);
    curlex.fl = a->fl;
    if (OPCODE(asmopcode_(a)) == A_B)
    {   LabBind *lab = (LabBind*) asmopnd1_(a);
        LabelNumber *l = lab->labinternlab;
        if (l == NULL)
            l = lab->labinternlab = nextlabel();
        if (lab->labuses & l_defined)
        {
            if (cond == cur_cond)
                emitbranch(J_B, l);
        }
        else
            cond = cur_cond;
    }
    if (cond != cur_cond && cur_label != NULL)
    {                                   /* finish current conditional block */
        emitbranch(J_B, cur_label);
        start_new_basic_block(cur_label);
        cur_label = NULL;
        cur_cond = CC_AL;
    }
    if (OPCODE(asmopcode_(a)) == A_LABEL)   /* create a new block with the label */
    {   LabBind *lab = (LabBind*) asmopnd1_(a);
        LabelNumber *l = lab->labinternlab;
        if (l == NULL)
            l = lab->labinternlab = nextlabel();
        emitbranch(J_B, l);
        start_new_basic_block(l);
        cur_cond = cond = CC_AL;
    }
    if (cond != cur_cond && cond != CC_AL)
    {                               /* create a new block if instr is conditional */
        LabelNumber *l;
        if (OPCODE(asmopcode_(a)) == A_B)
        {   uint32 branch_cond;
            LabBind *lab = (LabBind*) asmopnd1_(a);
            l = lab->labinternlab;
            if (l == NULL)
                l = lab->labinternlab = nextlabel();
            if (lab->labuses & l_defined)
            {
                branch_cond = cond;
                emitbranch(J_B + translate_cc(branch_cond), l);
                l = nextlabel();
                cur_label = nextlabel();
                emitbranch(J_B, l);
                start_new_basic_block (l);
                cur_cond = cond = CC_AL;
            }
        }
        else
        {   l = nextlabel();
            cur_label = nextlabel();
            emitbranch(J_B + translate_cc(INVCC(cond)), cur_label);
            emitbranch(J_B, l);
            start_new_basic_block (l);
            cur_cond = cond;
        }
    }
    if (asmopcode_(a) & SET_CC) cur_cond = CC_NOT;

    switch (INSTRCL(asmopcode_(a)))
    {
        case CL_MOV:
        case CL_CMP:
        case CL_BIN:
            cg_asm_data(a);
            break;
        case CL_MUL: cg_asm_mul(a);
            break;
        case CL_LDM: cg_asm_ldmstm(a);
            break;
        case CL_MEM: cg_asm_ldrstr(a);
            break;
        case CL_PSR: cg_psr(a);
            break;
        case CL_SWI: cg_swi(a);
            break;
        case CL_SWP: cg_swp(a);
            break;
        case CL_BR: cg_bl(a);
            break;
        case CL_COP: cg_cop(a);
            break;
        case CL_CMEM: cg_cmem(a);
            break;
        case CL_NOP: emitreg4(J_NULLOP, 0, 0, 0, 0, 0);
            break;
        default:
            break;
    }
}





static void cg_thumb_data(const AsmInstr *a)
{
    J_OPCODE op;
    VRegnum r1, r2, r3;
    bool const_opnd = h0_(asmopnd3_(a)) == s_integer;
    bool hireg_ok;

    if (const_opnd)
        r3 = intval_(asmopnd3_(a));
    switch (INSTRCL(asmopcode_(a)))
    {
        case CL_MOV:
            switch (OPCODE(asmopcode_(a)))
            {
                case T_NEG:
                    if (const_opnd)
                        op = J_MOVR, r3 = -r3;
                    else
                        op = J_NEGR;
                    break;
                case A_MVN:
                    if (const_opnd)
                        op = J_MOVR, r3 = ~r3;
                    else
                        op = J_NOTR;
                    break;
                case A_MOV:
                    op = J_MOVR;
                    break;
            }
            break;

        case CL_SH:
            switch (OPCODE(asmopcode_(a)))
            {
                case T_ASR: op = J_SHRR + J_SIGNED; break;
                case T_LSR: op = J_SHRR + J_UNSIGNED; break;
                case T_LSL: op = J_SHLR + J_SIGNED; break;
                case T_ROR: op = J_RORR + J_UNSIGNED; break;
            }
            break;
        case CL_BIN:
        case CL_CMP:
            op = thumb_op_tab[OPCODE(asmopcode_(a))];
            break;
    }

    if (const_opnd)
        op = J_RTOK(op);
    hireg_ok = (op == J_ADDR || op == J_CMPR || op == J_MOVR);

    r1 = get_reg(asmopnd1_(a));
    if (hireg_ok)
        r2 = expr_reg(asmopnd2_(a));
    else
        r2 = expr_loreg(asmopnd2_(a));
    if (!const_opnd)
    {
        if (hireg_ok)
            r3 = expr_reg(asmopnd3_(a));
        else
            r3 = expr_loreg(asmopnd3_(a));
    }
    emitreg4(op, asmopcode_(a), r1, r2, r3, GAP);
    if (!const_opnd) free_reg(asmopnd3_(a), r3);

    free_reg(asmopnd2_(a), r2);
    if (hireg_ok)
        store_reg_pc(asmopnd1_(a), r1);
    else
        store_loreg(asmopnd1_(a), r1);
}


static void cg_thumb_mul(const AsmInstr *a)
{
    VRegnum r1, r2, r3;

    r1 = get_reg(asmopnd1_(a));
    r2 = expr_loreg(asmopnd2_(a));
    if (h0_(asmopnd3_(a)) == s_integer)
        emitreg4(J_MULK, asmopcode_(a), r1, r2, intval_(asmopnd3_(a)), 0);
    else
    {   r3 = expr_loreg(asmopnd3_(a));
        emitreg4(J_MULR, asmopcode_(a), r1, r2, r3, GAP);
        free_reg(asmopnd3_(a), r3);
        if (r1 == r3)
            cc_err(asm_err_mul_conflict);
    }
    free_reg(asmopnd2_(a), r2);
    store_loreg(asmopnd1_(a), r1);
}


static void cg_thumb_ldmstm(const AsmInstr *a)
{
    VRegnum r1, r3;
    uint32 op;

    op = OPCODE(asmopcode_(a)) == A_LDM ? J_LDM : J_STM;
    if (asmopcode_(a) & M_WB) op += J_LDMW - J_LDM;
    r1 = expr_reg_sp(asmopnd1_(a));
    r3 = intval_(asmopnd3_(a));
    emitreg4(op, asmopcode_(a), r1, GAP, r3, GAP);
    if (r3 == 0)
        cc_err(asm_err_ldm_empty);
    store_reg(asmopnd1_(a), r1);
}

static void cg_thumb_ldrstr(const AsmInstr *a)
{
    VRegnum r1, r2, r3;
    bool is_load = OPCODE(asmopcode_(a)) == A_LDR;
    uint32 op;

    if (asmopcode_(a) & M_BYTE)
        op = J_LDRBR+J_ALIGN1;
    else if (asmopcode_(a) & M_HALF)
        op = J_LDRWR+J_ALIGN2;
    else
        op = J_LDRR+J_ALIGN4;
    if (!is_load) op += J_STRR - J_LDRR;
    if (asmopcode_(a) & M_SIGNED)
        op |= J_SIGNED;
    else if (is_load && op != J_LDRR+J_ALIGN4)
        op |= J_UNSIGNED;

    if (is_load)
        r1 = get_reg(asmopnd1_(a));
    else
        r1 = expr_loreg(asmopnd1_(a));
    r2 = expr_reg_sp(asmopnd2_(a));         /* always allow SP as base */
    if (h0_(asmopnd3_(a)) == s_integer)
    {
        r3 = intval_(asmopnd3_(a));
        emitreg4(J_RTOK(op), asmopcode_(a), r1, r2, r3, GAP);
    }
    else
    {
        r3 = expr_loreg(asmopnd3_(a));
        emitreg4(op, asmopcode_(a), r1, r2, r3, GAP);
        free_reg(asmopnd3_(a), r3);
    }
    free_reg(asmopnd2_(a), r2);
    if (is_load)
        store_loreg(asmopnd1_(a), r1);
    else
        free_reg(asmopnd1_(a), r1);
}








/* TODO: cleanup branches and labels -> can be a lot simpler! */

void cg_thumb_instr(const AsmInstr *a)
{
    uint32 cond = GETCC(asmopcode_(a));

    if (usrdbg(DBG_LINE))
        emitfl(J_INFOLINE, a->fl);
    curlex.fl = a->fl;
    if (OPCODE(asmopcode_(a)) == A_B)
    {   LabBind *lab = (LabBind*) asmopnd1_(a);
        LabelNumber *l = lab->labinternlab;
        if (l == NULL)
            l = lab->labinternlab = nextlabel();
        if (lab->labuses & l_defined)
        {
            if (cond == cur_cond)
                emitbranch(J_B, l);
        }
        else
            cond = cur_cond;
    }
    if (cond != cur_cond && cur_label != NULL)
    {                                   /* finish current conditional block */
        emitbranch(J_B, cur_label);
        start_new_basic_block(cur_label);
        cur_label = NULL;
        cur_cond = CC_AL;
    }
    if (OPCODE(asmopcode_(a)) == A_LABEL)   /* create a new block with the label */
    {   LabBind *lab = (LabBind*) asmopnd1_(a);
        LabelNumber *l = lab->labinternlab;
        if (l == NULL)
            l = lab->labinternlab = nextlabel();
        emitbranch(J_B, l);
        start_new_basic_block(l);
        cur_cond = cond = CC_AL;
    }
    if (cond != cur_cond && cond != CC_AL)
    {                               /* create a new block if instr is conditional */
        LabelNumber *l;
        if (OPCODE(asmopcode_(a)) == A_B)
        {   uint32 branch_cond;
            LabBind *lab = (LabBind*) asmopnd1_(a);
            l = lab->labinternlab;
            if (l == NULL)
                l = lab->labinternlab = nextlabel();
            if (lab->labuses & l_defined)
            {
                branch_cond = cond;
                emitbranch(J_B + translate_cc(branch_cond), l);
                l = nextlabel();
                cur_label = nextlabel();
                emitbranch(J_B, l);
                start_new_basic_block (l);
                cur_cond = cond = CC_AL;
            }
        }
        else
        {   l = nextlabel();
            cur_label = nextlabel();
            emitbranch(J_B + translate_cc(INVCC(cond)), cur_label);
            emitbranch(J_B, l);
            start_new_basic_block (l);
            cur_cond = cond;
        }
    }
    if (asmopcode_(a) & SET_CC) cur_cond = CC_NOT;

    switch (INSTRCL(asmopcode_(a)))
    {
        case CL_MOV:
        case CL_CMP:
        case CL_BIN:
        case CL_SH: cg_thumb_data(a);
            break;
        case CL_MUL: cg_thumb_mul(a);
            break;
        case CL_LDM: cg_thumb_ldmstm(a);
            break;
        case CL_MEM: cg_thumb_ldrstr(a);
            break;
        case CL_SWI: cg_swi(a);
            break;
        case CL_BR: cg_bl(a);
            break;
        case CL_NOP: emitreg4(J_NULLOP, 0, 0, 0, 0, 0);
            break;
        default:
            break;
    }
}


void cg_asm(Cmd * c)
{
    AsmInstr *a = (AsmInstr *) cmd1e_(c);

    cg_asm_init();
    for ( ; a != NULL; a = a->cdr)
    {
#ifdef ARM_INLINE_ASSEMBLER
        cg_arm_instr(a);
#endif
#ifdef THUMB_INLINE_ASSEMBLER
        cg_thumb_instr(a);
#endif
    }
    cg_asm_finish();
}







#ifdef ARM_INLINE_ASSEMBLER

static void gen_data(PendingOp *cur)
{
    J_OPCODE op;
    uint32 shift;

    switch (cur->ic.flags & RN_OPND_MASK)
    {
        case RN_CONST:
            op = arm_op_tab [asminstr(cur->ic.flags)];
            break;

        case RN_REG:
            op = J_KTOR (arm_op_tab [asminstr(cur->ic.flags)]);
            break;

        case RN_SHIFT:
            op = J_KTOR (arm_op_tab [asminstr(cur->ic.flags)]);
            switch (cur->ic.flags & SH_MASK)
            {
                case SH_LSL: shift = cur->ic.r4.i; break;
                case SH_LSR: shift = cur->ic.r4.i | SHIFT_RIGHT; break;
                case SH_ASR: shift = cur->ic.r4.i | SHIFT_RIGHT | SHIFT_ARITH; break;
                case SH_ROR: shift = cur->ic.r4.i | SHIFT_ARITH; break;
                case SH_RRX: shift = SHIFT_ARITH; break; /* RRX = ROR 0 */
            }
            op |= shift << J_SHIFTPOS;
            break;

        case RN_SHIFT_REG:
            op = J_KTOR (arm_op_tab [asminstr(cur->ic.flags)]);
            switch (cur->ic.flags & SH_MASK)
            {
                case SH_LSL: shift = P_LSL; break;
                case SH_LSR: shift = P_LSR; break;
                case SH_ASR: shift = P_ASR; break;
                case SH_ROR: shift = P_ROR; break;
                case SH_RRX: ;/* Error */
            }
            cur->peep |= shift;
            break;
    }
    cur->ic.op = (cur->ic.op & ~J_TABLE_BITS) | op;
    if (cur->ic.flags & SET_CC)
        cur->peep |= P_CMPZ | P_SETCC;
}


static void gen_move(PendingOp *cur)
{
    uint32 shift = (cur->ic.op & J_SHIFTMASK) >> J_SHIFTPOS;

    if ((cur->ic.op & J_TABLE_BITS) == J_MVNR)
    {
        cur->ic.op += J_NOTR - J_MVNR;
        return;
    }

    switch (cur->ic.flags & RN_OPND_MASK)
    {
        case RN_CONST:
            if ((cur->ic.op & J_TABLE_BITS) == J_MVNK)
            {
                cur->ic.op += J_MOVK - J_MVNK;
                cur->ic.r3.i = ~cur->ic.r3.i;
            }
            break;

        case RN_REG:
            break;

        case RN_SHIFT:
            cur->ic.r2.r = cur->ic.r3.r;
            if (cur->dataflow & J_DEAD_R3)
                cur->dataflow ^= J_DEAD_R3 + J_DEAD_R2;
            if (shift & SHIFT_RIGHT)
            {
                if (shift & SHIFT_ARITH)
                    cur->ic.op = J_SHRK+J_SIGNED;
                else
                    cur->ic.op = J_SHRK+J_UNSIGNED;
            }
            else if (shift & SHIFT_ARITH)
                cur->ic.op = J_RORK;
            else
                cur->ic.op = J_SHLK+J_UNSIGNED;
            if ((shift & 31) == 0 &&
                (cur->ic.op == J_SHRK+J_UNSIGNED || cur->ic.op == J_SHRK+J_SIGNED))
                cur->ic.r3.i = 32;
            else
                cur->ic.r3.i = shift & 31;
            break;

        case RN_SHIFT_REG:
            cur->ic.r2.r = cur->ic.r3.r;
            cur->ic.r3.r = cur->ic.r4.r;
            if (cur->dataflow & J_DEAD_R3)
                cur->dataflow ^= J_DEAD_R3 + J_DEAD_R2;
            if (cur->dataflow & J_DEAD_R4)
                cur->dataflow ^= J_DEAD_R4 + J_DEAD_R3;
            switch (cur->peep & P_RSHIFT)
            {
                case P_LSR: cur->ic.op = J_SHRR+J_UNSIGNED; break;
                case P_ASR: cur->ic.op = J_SHRR+J_SIGNED; break;
                case P_LSL: cur->ic.op = J_SHLR+J_UNSIGNED; break;
                case P_ROR: cur->ic.op = J_RORR; break;
                default: ;/* Error */
            }
            cur->peep &= ~(P_LSR | P_LSL | P_ASR | P_ROR);
            break;
    }
}


static void gen_ldrstr(PendingOp *cur)
{
    uint32 op, shift;
    bool is_load = OPCODE(cur->ic.flags) == A_LDR;

    if (cur->ic.flags & M_BYTE)
        op = J_LDRBR+J_ALIGN1;
    else if (cur->ic.flags & M_HALF)
        op = J_LDRWR+J_ALIGN2;
    else
        op = J_LDRR+J_ALIGN4;
    if (!is_load)
        op += J_STRR - J_LDRR;
    if (cur->ic.flags & M_SIGNED)
        op |= J_SIGNED;
    else if (is_load && op != J_LDRR+J_ALIGN4)
        op |= J_UNSIGNED;
    if (cur->ic.flags & M_WB)
        if (cur->ic.flags & M_PREIDX)
            cur->peep |= P_PRE;
        else
            cur->peep |= P_POST;
    if (!cur->ic.flags & M_UPIDX)
        op += J_NEGINDEX;
    if (cur->ic.flags & M_TRANS)
        cur->peep |= P_TRANS;
    switch (cur->ic.flags & RN_OPND_MASK)
    {
        case RN_CONST:
            op = J_RTOK(op); break;
        case RN_REG:
            break;
        case RN_SHIFT:
            switch (cur->ic.flags & SH_MASK)
            {
                case SH_LSL: shift = cur->ic.r4.i; break;
                case SH_LSR: shift = cur->ic.r4.i | SHIFT_RIGHT; break;
                case SH_ASR: shift = cur->ic.r4.i | SHIFT_RIGHT | SHIFT_ARITH; break;
                case SH_ROR: shift = cur->ic.r4.i | SHIFT_ARITH; break;
                case SH_RRX: shift = SHIFT_ARITH; break; /* RRX = ROR 0 */
            }
            op |= shift << J_SHIFTPOS;
            break;

        default: ;/* Error */
    }
    cur->ic.op = op;
}


void translate_asm_instr (PendingOp *cur)
{
    switch (INSTRCL (cur->ic.flags))
    {
        case CL_BIN:
            gen_data(cur);
            break;

        case CL_CMP:
            gen_data(cur);
            if (cur->ic.r1.r == 15)       /* TEQP & friends */
                cur->peep |= P_SETPSR;
            break;

        case CL_MOV:
            gen_data(cur);
            gen_move(cur);  /* extra translation for moves */
            break;

        case CL_MEM:
            gen_ldrstr(cur);
            break;

    }
}


#endif



