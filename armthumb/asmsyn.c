/*
 * asmsyn.c: syntax analysis & parsing of ARM/Thumb inline assembler
 * Copyright (C) Advanced Risc Machines Ltd., 1997. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <string.h>
#include <ctype.h>
#include "globals.h"
#include "syn.h"
#include "pp.h"
#include "lex.h"
#include "simplify.h"
#include "bind.h"
#include "sem.h"
#include "aetree.h"
#include "builtin.h"
#include "vargen.h"
#include "mcdep.h"
#include "store.h"
#include "errors.h"
#include "aeops.h"
#include "armops.h"
#include "cgdefs.h"

#include "inlnasm.h"

#if defined(ARM_INLINE_ASSEMBLER) || defined(THUMB_INLINE_ASSEMBLER)


typedef enum
{   Str_UpperCase = 1, Str_LowerCase = 2, Str_SubString = 4,
    Str_Opcode      = Str_SubString,
    Str_Register    = Str_LowerCase,
    Str_ShiftOp     = Str_LowerCase,
    Str_Coproc      = Str_LowerCase,
    Str_PSR         = Str_SubString,
    Str_Null        = 0
} StrFlags;

typedef char *string;

typedef uint32 ID;
#define NIL_ID 0xffffffffU

#define SizeofArr(arr) (sizeof (arr) / sizeof (arr [0]))

#define R_ARMONLY 16

typedef enum
{
    NO_SHIFT,
    CONST_SHIFT,
    REG_SHIFT
} ShiftType;

typedef enum
{
    LVALUE,
    RVALUE,
    CONSTVALUE,
    OPND_RN
} OpndType;

typedef struct
{
    const char * str;
    ID id;
} IdentDef;


static IdentDef const cc_tab[] =
{   /* increasing order! */
    { "AL", CC_AL },
    { "CC", CC_CC },
    { "CS", CC_CS },
    { "EQ", CC_EQ },
    { "GE", CC_GE },
    { "GT", CC_GT },
    { "HI", CC_HI },
    { "HS", CC_CS },
    { "LE", CC_LE },
    { "LO", CC_CC },
    { "LS", CC_LS },
    { "LT", CC_LT },
    { "MI", CC_MI },
    { "NE", CC_NE },
    { "PL", CC_PL },
    { "VC", CC_VC },
    { "VS", CC_VS }
};


static IdentDef const arm_mm_tab[] =
{   /* increasing order! */
    { "DA", MM_DA },
    { "DB", MM_DB },
    { "EA", MM_DB | MM_STACK },
    { "ED", MM_IB | MM_STACK },
    { "FA", MM_DA | MM_STACK },
    { "FD", MM_IA | MM_STACK },
    { "IA", MM_IA },
    { "IB", MM_IB }
};


static IdentDef const arm_opcode_tab[] =
{   /* increasing order! */
    { "ADC",    MKOP (A_ADC, CL_BIN) },
    { "ADD",    MKOP (A_ADD, CL_BIN) },
    { "AND",    MKOP (A_AND, CL_BIN) },
    { "BIC",    MKOP (A_BIC, CL_BIN) },
    { "CDP",    MKOP (A_CDP, CL_COP) },
    { "CMN",    MKOP (A_CMN, CL_CMP) | SET_CC },
    { "CMP",    MKOP (A_CMP, CL_CMP) | SET_CC },
    { "EOR",    MKOP (A_EOR, CL_BIN) },
    { "LDC",    MKOP (A_LDC, CL_CMEM)},
    { "LDM",    MKOP (A_LDM, CL_LDM) },
    { "LDR",    MKOP (A_LDR, CL_MEM) },
    { "MCR",    MKOP (A_MCR, CL_COP) },
    { "MLA",    MKOP (A_MLA, CL_MUL) },
    { "MOV",    MKOP (A_MOV, CL_MOV) },
    { "MRC",    MKOP (A_MRC, CL_COP) },
    { "MRS",    MKOP (A_MRS, CL_PSR) },
    { "MSR",    MKOP (A_MSR, CL_PSR) },
    { "MUL",    MKOP (A_MUL, CL_MUL) },
    { "MVN",    MKOP (A_MVN, CL_MOV) },
    { "NOP",    MKOP (A_NOP, CL_NOP) },
    { "ORR",    MKOP (A_ORR, CL_BIN) },
    { "RSB",    MKOP (A_RSB, CL_BIN) },
    { "RSC",    MKOP (A_RSC, CL_BIN) },
    { "SBC",    MKOP (A_SBC, CL_BIN) },
    { "SMLAL",  MKOP (A_MLAL,CL_MUL) | M_SIGNED },
    { "SMULL",  MKOP (A_MULL,CL_MUL) | M_SIGNED },
    { "STC",    MKOP (A_STC, CL_CMEM)},
    { "STM",    MKOP (A_STM, CL_LDM) },
    { "STR",    MKOP (A_STR, CL_MEM) },
    { "SUB",    MKOP (A_SUB, CL_BIN) },
    { "SWI",    MKOP (A_SWI, CL_SWI) },
    { "SWP",    MKOP (A_SWP, CL_SWP) },
    { "TEQ",    MKOP (A_TEQ, CL_CMP) | SET_CC },
    { "TST",    MKOP (A_TST, CL_CMP) | SET_CC },
    { "UMLAL",  MKOP (A_MLAL,CL_MUL  ) },
    { "UMULL",  MKOP (A_MULL,CL_MUL  ) },
};


static IdentDef const thumb_opcode_tab[] =
{   /* increasing order! */
    { "ADC",    MKOP (A_ADC, CL_BIN) },
    { "ADD",    MKOP (A_ADD, CL_BIN) },
    { "AND",    MKOP (A_AND, CL_BIN) },
    { "ASL",    MKOP (T_LSL, CL_SH)  },
    { "ASR",    MKOP (T_ASR, CL_SH)  },
    { "BIC",    MKOP (A_BIC, CL_BIN) },
    { "CMN",    MKOP (A_CMN, CL_CMP) },
    { "CMP",    MKOP (A_CMP, CL_CMP) },
    { "EOR",    MKOP (A_EOR, CL_BIN) },
    { "LDMIA",  MKOP (A_LDM, CL_LDM) | MM_IA | M_WB },
    { "LDR",    MKOP (A_LDR, CL_MEM) },
    { "LDRB",   MKOP (A_LDR, CL_MEM) | M_BYTE },
    { "LDRH",   MKOP (A_LDR, CL_MEM) | M_HALF },
    { "LDSB",   MKOP (A_LDR, CL_MEM) | M_BYTE | M_SIGNED },
    { "LDSH",   MKOP (A_LDR, CL_MEM) | M_HALF | M_SIGNED },
    { "LSL",    MKOP (T_LSL, CL_SH)  },
    { "LSR",    MKOP (T_LSR, CL_SH)  },
    { "MOV",    MKOP (A_MOV, CL_MOV) },
    { "MUL",    MKOP (A_MUL, CL_MUL) },
    { "MVN",    MKOP (A_MVN, CL_MOV) },
    { "NEG",    MKOP (T_NEG, CL_MOV) },
    { "NOP",    MKOP (A_NOP, CL_NOP) },
    { "ORR",    MKOP (A_ORR, CL_BIN) },
    { "POP",    MKOP (A_LDM, CL_LDM) | MM_STACK | MM_IA | M_WB },
    { "PUSH",   MKOP (A_STM, CL_LDM) | MM_STACK | MM_DB | M_WB },
    { "ROR",    MKOP (T_ROR, CL_SH)  },
    { "SBC",    MKOP (A_SBC, CL_BIN) },
    { "STMIA",  MKOP (A_STM, CL_LDM) | MM_IA | M_WB },
    { "STR",    MKOP (A_STR, CL_MEM) },
    { "STRB",   MKOP (A_STR, CL_MEM) | M_BYTE },
    { "STRH",   MKOP (A_STR, CL_MEM) | M_HALF },
    { "SUB",    MKOP (A_SUB, CL_BIN) },
    { "SWI",    MKOP (A_SWI, CL_SWI) },
    { "TST",    MKOP (A_TST, CL_CMP) },
};


static IdentDef const arm_shift_tab[] =
{   /* increasing order! */
    { "ASL", SH_LSL },
    { "ASR", SH_ASR },
    { "LSL", SH_LSL },
    { "LSR", SH_LSR },
    { "ROR", SH_ROR },
    { "RRX", SH_RRX }
};


static IdentDef const register_tab[] =
{   /* increasing order! */
    { "A1", R_A1},
    { "A2", R_A2},
    { "A3", R_A3},
    { "A4", R_A4},
    { "FP", R_FP | R_ARMONLY },
    { "IP", R_IP},
    { "LR", R_LR},
    { "PC", R_PC},
    { "PSR",R_PSR | R_ARMONLY },
    { "R0",   0 },
    { "R1",   1 },
    { "R10", 10 },
    { "R11", 11 },
    { "R12", 12 },
    { "R13", 13 },
    { "R14", 14 },
    { "R15", 15 },
    { "R2",   2 },
    { "R3",   3 },
    { "R4",   4 },
    { "R5",   5 },
    { "R6",   6 },
    { "R7",   7 },
    { "R8",   8 },
    { "R9",   9 },
    { "SB", R_SB | R_ARMONLY },
    { "SL", R_SL},
    { "SP", R_SP},
    { "V1", R_V1},
    { "V2", R_V2},
    { "V3", R_V3},
    { "V4", R_V4},
    { "V5", R_V5},
    { "V6", R_V6},
    { "V7", R_SL},
    { "V8", R_FP}
};


static IdentDef const psr_tab[] =
{   /* increasing order! */
    { "CPSR",     CPSR + PSR_CF },
    { "CPSR_",    CPSR          },
    { "SPSR",     SPSR + PSR_CF },
    { "SPSR_",    SPSR          }
};


static IdentDef const coproc_id_tab[] =
{   /* increasing order! */
    { "P0",   0 },
    { "P1",   1 },
    { "P10", 10 },
    { "P11", 11 },
    { "P12", 12 },
    { "P13", 13 },
    { "P14", 14 },
    { "P15", 15 },
    { "P2",   2 },
    { "P3",   3 },
    { "P4",   4 },
    { "P5",   5 },
    { "P6",   6 },
    { "P7",   7 },
    { "P8",   8 },
    { "P9",   9 }
};


static IdentDef const coproc_reg_tab[] =
{   /* increasing order! */
    { "C0",   0 },
    { "C1",   1 },
    { "C10", 10 },
    { "C11", 11 },
    { "C12", 12 },
    { "C13", 13 },
    { "C14", 14 },
    { "C15", 15 },
    { "C2",   2 },
    { "C3",   3 },
    { "C4",   4 },
    { "C5",   5 },
    { "C6",   6 },
    { "C7",   7 },
    { "C8",   8 },
    { "C9",   9 },
};


static const char *prefix_str(const char *str, const char *substr)
{   char ch1, ch2;
    do
    {   ch1 = *str++;
        ch2 = *substr++;
    } while (ch1 == ch2 && ch1 != 0);
    if (ch1 != ch2 && ch2 != 0) return NULL;
    return str-1;
}


static void toupper_str(char *dst, const char *src, int len)
{
    bool is_lower_str;
    const char *p;

    for (p = src; *p != 0; p++)
        if (isalpha(*p) && isupper(*p))
            break;
    is_lower_str = (*p == 0);

    if (is_lower_str)
        while (--len)
            *dst++ = toupper(*src++);
    else
        while (--len)
            *dst++ = *src++;
    if (len == 0)
        dst[-1] = 0;
}


static ID read_substring(char const **str_ptr,
                         IdentDef const ident[],
                         int num_idents,
                         StrFlags flags)
{
    char const *str = *str_ptr;
    char const *id_str;
    uint32 low = 0, high = num_idents;

    while (high > low)
    {   uint32 mid = (low + high) >> 1;
        if (strcmp(str, ident[mid].str) < 0)
            high = mid;
        else
            low = mid + 1;
    }
    if (low == 0)
        return NIL_ID;
    low--;
    id_str = prefix_str(str, ident[low].str);
    if (id_str == NULL || (!(flags & Str_SubString) && *id_str != '\0'))
        return NIL_ID;
    *str_ptr += id_str - str;
    return ident[low].id;
}


static ID read_string(char const *str_src,
                      IdentDef const ident[],
                      int num_idents,
                      StrFlags flags)
{
    char str_arr[32];
    char *str = str_arr;

    if (flags & Str_LowerCase)
        toupper_str(str, str_src, sizeof(str_arr));
    else
        strncpy(str, str_src, sizeof(str_arr));
    return read_substring((char const **)&str, ident, num_idents, flags);
}


static ID read_cc(char const * *str_ptr)
{
    ID cc = read_substring(str_ptr, cc_tab, SizeofArr(cc_tab), Str_Opcode);
    return (cc == NIL_ID) ? CC_AL : cc;
}


static ID arm_opcode(char const *opcode_str)
{
    ID opcode;
    char str_arr[32];
    char *str = str_arr;

    toupper_str(str, opcode_str, sizeof(str_arr));
    opcode = read_substring((char const **)&str, arm_opcode_tab,
                            SizeofArr(arm_opcode_tab), Str_Opcode);

    /* special case for B and BL since BLS/BLO conflicts with BL */
    if (opcode == NIL_ID && str[0] == 'B')
    {   int len = strlen(str);
        if (len == 1 || len == 3)
        {
            opcode = MKOP(A_B,  CL_BR);
            str++;
        }
        else if (str[1] == 'L')
        {
            opcode = MKOP(A_BL, CL_BR);
            str += 2;
        }
    }
    if (opcode == NIL_ID) return opcode;
    opcode |= read_cc((char const **)&str);
    switch (INSTRCL(opcode))
    {
        case CL_PSR:
        case CL_SWI:
        case CL_BR:
        case CL_COP:
            break;

        case CL_CMP:
            if (*str == 'P')
            {
                str++;
                opcode |= SET_PSR;
                if (!(config & CONFIG_26BIT))
                    cc_warn(asm_err_no_teqp);
            }
            break;

        case CL_MOV:
        case CL_BIN:
        case CL_MUL:
        case CL_MULL:
            if (*str == 'S')
            {
                str++;
                opcode |= SET_CC;
            }
            break;

        case CL_LDM:
            opcode |= read_substring((char const **)&str,
                                     arm_mm_tab, SizeofArr(arm_mm_tab),
                                     Str_Opcode);
            if (opcode & MM_STACK)
            {   if (OPCODE(opcode) == A_STM)
                    opcode ^= MM_IB;
                opcode ^= MM_STACK;
            }
            break;

        case CL_MEM:
            if (OPCODE(opcode) == A_LDR && *str == 'S')
            {
                str++;
                opcode |= M_SIGNED;
            }
            if (*str == 'B')
            {
                str++;
                opcode |= M_BYTE;
            }
            else if (*str == 'H')
            {
                str++;
                opcode |= M_HALF;
            }
            if (*str == 'T' && !(opcode & (M_SIGNED | M_HALF)))
            {
                str++;
                opcode |= M_TRANS;
            }
            break;

        case CL_SWP:
            if (*str == 'B')
            {
                str++;
                opcode |= M_BYTE;
            }
            break;

        case CL_CMEM:
            if (*str == 'L')
            {
                str++;
                opcode |= M_LONG;
            }
            break;

        case CL_NOP:
            break;

        default:
            return NIL_ID;
    }
    return (*str == '\0') ? opcode : NIL_ID;
}


static ID thumb_opcode(char const *opcode_str)
{
    ID opcode;
    char str_arr[32];
    char *str = str_arr;

    toupper_str(str, opcode_str, sizeof(str_arr));
    opcode = read_substring((char const **)&str,
                            thumb_opcode_tab, SizeofArr(thumb_opcode_tab),
                            Str_Opcode);

    /* special case for B and BL since BLS/BLO conflicts with BL */
    if (opcode == NIL_ID && str[0] == 'B')
    {   int len = strlen(str);
        if (len == 1 || len == 3)
        {
            opcode = MKOP(A_B,  CL_BR);
            str++;
            opcode |= read_cc((char const **)&str);
        }
        else if (str[1] == 'L')
        {
            opcode = MKOP(A_BL, CL_BR);
            str += 2;
            opcode |= CC_AL;
        }
    }
    else
        opcode |= CC_AL;
    return (*str == '\0') ? opcode : NIL_ID;
}


static void NoteFileLine(FileLine *fl) {
    *fl = curlex.fl;
    fl->p = dbg_notefileline(*fl);
}


static ID rd_arm_phys_reg(void)
{
    ID reg;

    if (curlex.sym != s_identifier)
    {   checkfor_ket(s_identifier);
        return NIL_ID;
    }
    reg = read_string(symname_(curlex.a1.sv), register_tab,
                      SizeofArr(register_tab), Str_Register);
#ifdef THUMB_INLINE_ASSEMBLER
    if (reg & R_ARMONLY)
        reg = NIL_ID;
#endif
    if (reg == NIL_ID) cc_err(asm_err_bad_physreg, symname_(curlex.a1.sv));
    nextsym();
    return reg & 15;
}


static Expr *rd_arm_opnd(OpndType opnd_type)
{   Expr *e;

    if (opnd_type != CONSTVALUE && curlex.sym == s_identifier)     /* TODO: C++ */
    {
        ID reg = read_string(symname_(curlex.a1.sv), register_tab,
                             SizeofArr(register_tab), Str_Register);
#ifdef THUMB_INLINE_ASSEMBLER
        if (reg & R_ARMONLY)
            reg = NIL_ID;
#endif
        if (reg != NIL_ID)
        {   nextsym();
            e = mkintconst(te_uint, reg & 15, 0);
            h0_(e) = s_register;
            return e;
        }
    }
    e = rd_expr(UPTOCOMMA);
    if (e == 0)
        return errornode;
    if (opnd_type == LVALUE)
    {
        e = ensurelvalue(e, s_asm);
        if (h0_(e) == s_error)
            return errornode;
    }
    else
        e = coerceunary(e);
    e = optimise0(e);
    if (e == 0)
        return errornode;
    if (opnd_type == CONSTVALUE && h0_(e) != s_integer)
    {
        moan_nonconst(e, asm_non_const, asm_nonconst1, asm_nonconst2);
        return errornode;
    }
    return e;
}


static Expr *rd_thumb_opnd(OpndType opnd_type)
{
    if (opnd_type == OPND_RN)
    {
        if (curlex.sym == s_hash)
        {
            nextsym();
            opnd_type = CONSTVALUE;
        }
        else
            opnd_type = RVALUE;
    }
    return rd_arm_opnd(opnd_type);
}


static Expr *rd_coproc_reg(void)
{
    ID reg;

    if (curlex.sym != s_identifier)
    {
        checkfor_ket(s_identifier);
        return errornode;
    }
    reg = read_string(symname_(curlex.a1.sv), coproc_reg_tab,
                      SizeofArr(coproc_reg_tab), Str_Register);
    if (reg == NIL_ID)
    {
        cc_err(asm_err_bad_physreg, symname_(curlex.a1.sv));
        nextsym();
        return errornode;
    }
    nextsym();
    return mkintconst(te_uint, reg, 0);
}


static void rd_opnd_rn(AsmInstr *a, ShiftType shift_type)
{
    ID shift;

    if (curlex.sym == s_hash)
    {
        nextsym();
        asmopnd3_(a) = rd_arm_opnd(CONSTVALUE);
        asmopcode_(a) |= RN_CONST;
    }
    else
    {   asmopnd3_(a) = rd_arm_opnd(RVALUE);
        if (shift_type == NO_SHIFT || curlex.sym != s_comma)
        {
            if (h0_(asmopnd3_(a)) == s_integer)
                asmopcode_(a) |= RN_CONST;
            else
                asmopcode_(a) |= RN_REG;
            return;
        }
        nextsym();
        if (curlex.sym != s_identifier)
        {
            cc_err(asm_err_expected_shift);
            asmopnd4_(a) = errornode;
            return;
        }
        shift = read_string(symname_(curlex.a1.sv), arm_shift_tab,
                            SizeofArr(arm_shift_tab), Str_ShiftOp);
        if (shift == NIL_ID)
        {
            cc_err(asm_err_bad_shift, symname_(curlex.a1.sv));
            asmopnd4_(a) = errornode;
            return;
        }
        nextsym();
        if (shift == SH_RRX)
        {
            asmopcode_(a) |= shift;
            asmopnd4_(a) = mkintconst(te_uint, 0, 0);
            asmopcode_(a) |= RN_SHIFT;
            return;
        }
        if (curlex.sym == s_hash)
        {
            nextsym();
            shift_type = CONST_SHIFT;
        }
        asmopnd4_(a) = rd_arm_opnd(shift_type == REG_SHIFT ? RVALUE : CONSTVALUE);
        if (h0_(asmopnd4_(a)) == s_integer)
        {
            int shiftval = intval_(asmopnd4_(a));
            /* Convert LSL, LSR, ASR, ROR #0 to no shift */
            if (shiftval == 0)
            {
                asmopcode_(a) |= RN_REG;
                return;
            }
            /* LSR #32 and ASR #32 allowed, otherwise complain */
            if (shiftval == 32 && (shift == SH_LSR || shift == SH_ASR))
                shiftval = 0;
            if (shiftval < 0 || shiftval >= 32)
            {
                cc_err(asm_err_bad_shiftval, shiftval);
                asmopnd4_(a) = errornode;
            }
            asmopcode_(a) |= shift | RN_SHIFT;
        }
        else
            asmopcode_(a) |= shift | RN_SHIFT_REG;
    }
    return;
}


static void rd_binary_instr(AsmInstr *a)
{
    asmopnd1_(a) = rd_arm_opnd(LVALUE);
    checkfor_ket(s_comma);
    asmopnd2_(a) = rd_arm_opnd(RVALUE);
    checkfor_ket(s_comma);
    rd_opnd_rn(a, REG_SHIFT);
}


static void rd_compare_instr(AsmInstr *a)
{
    asmopnd2_(a) = rd_arm_opnd(RVALUE);
    checkfor_ket(s_comma);
    rd_opnd_rn(a, REG_SHIFT);
}


static void rd_move_instr(AsmInstr *a)
{
    asmopnd1_(a) = rd_arm_opnd(RVALUE);
    checkfor_ket(s_comma);
    rd_opnd_rn(a, REG_SHIFT);
}


static void rd_mem_instr(AsmInstr *a)
{
    bool is_half_or_signed = asmopcode_(a) & (M_SIGNED | M_HALF);

    if (is_half_or_signed && !(config & CONFIG_HALFWORD_SPT))
        cc_err(asm_err_no_halfword);
    asmopnd1_(a) = rd_arm_opnd(OPCODE (asmopcode_(a)) == A_LDR ? LVALUE : RVALUE);
    checkfor_ket(s_comma);
    checkfor_ket(s_lbracket);
    asmopnd2_(a) = rd_arm_opnd(RVALUE);
    if (curlex.sym == s_comma)
    {
        nextsym();
        asmopcode_(a) |= M_PREIDX;
        if (curlex.sym == s_minus)
            nextsym();
        else
            asmopcode_(a) |= M_UPIDX;
        rd_opnd_rn(a, is_half_or_signed ? NO_SHIFT : CONST_SHIFT);
        checkfor_ket(s_rbracket);
        if (curlex.sym == s_boolnot)
        {
            nextsym();
            asmopcode_(a) |= M_WB;
        }
        if (asmopcode_(a) & M_TRANS)
        {
            cc_err(asm_err_ldrt_adrmode);
            asmopnd1_(a) = errornode;
        }
    }
    else
    {   checkfor_ket(s_rbracket);
        if (curlex.sym != s_comma)
        {   /* no RN, so default to LDR r0, [r1, #0] or LDRT r0, [r1], #0 */
            asmopcode_(a) |= RN_CONST;
            asmopnd3_(a) = mkintconst(te_uint, 0, 0);
            if (!(asmopcode_(a) & M_TRANS))
                asmopcode_(a) |= M_PREIDX;
            else
                asmopcode_(a) |= M_WB;
            return;
        }
        asmopcode_(a) |= M_WB;
        nextsym();
        if (curlex.sym == s_minus)
            nextsym();
        else
            asmopcode_(a) |= M_UPIDX;
        rd_opnd_rn(a, is_half_or_signed ? NO_SHIFT : CONST_SHIFT);
    }
}


static uint32 rd_reglist(void)
{
    uint32 regmask = 0;

    checkfor_ket(s_lbrace);
    while (curlex.sym != s_rbrace)
    {
        ID reg1, reg2;
        reg1 = rd_arm_phys_reg();
        if (curlex.sym == s_minus)
        {
            nextsym();
            reg2 = rd_arm_phys_reg();
            if ((reg1 | reg2 | regmask) == NIL_ID)
                regmask = NIL_ID;
            else if (reg2 > reg1)
                regmask |= (2 << reg2) - (1 << reg1);
            else
                regmask |= (2 << reg1) - (1 << reg2);
        }
        else if (reg1 != NIL_ID)
            regmask |= 1 << reg1;
        else
            regmask = NIL_ID;
        if (curlex.sym != s_comma)
            break;
        nextsym();
    }
    checkfor_ket(s_rbrace);
    return regmask;
}


static void rd_ldm_instr(AsmInstr *a)
{
    uint32 regmask;

    asmopnd1_(a) = rd_arm_opnd(LVALUE);
    if (curlex.sym == s_boolnot)
    {   nextsym();
        asmopcode_(a) |= M_WB;
    }
    checkfor_ket(s_comma);
    regmask = rd_reglist();
    if (curlex.sym == s_xor)
    {
        asmopcode_(a) |= SET_CC;    /* is this a good idea? */
        nextsym();
    }
    if (regmask == NIL_ID)
        asmopnd3_(a) = errornode;
    else
        asmopnd3_(a) = mkintconst(te_uint, regmask, 0);
}


static void rd_mul_instr(AsmInstr *a)
{
    uint32 op = OPCODE(asmopcode_(a));

    if (!(config & CONFIG_LONG_MULTIPLY) && (op == A_MULL || op == A_MLAL))
        cc_err(asm_err_no_longmul);
    asmopnd1_(a) = rd_arm_opnd(LVALUE);
    checkfor_ket(s_comma);
    asmopnd2_(a) = rd_arm_opnd(INSTRCL (asmopcode_(a)) == CL_MULL ? LVALUE : RVALUE);
    checkfor_ket(s_comma);
    if (OPCODE(asmopcode_(a)) == A_MUL)
    {
        if (curlex.sym == s_hash)
        {   nextsym();
            asmopnd3_(a) = rd_arm_opnd(CONSTVALUE);
        }
        else
            asmopnd3_(a) = rd_arm_opnd(RVALUE);
    }
    else
    {   asmopnd3_(a) = rd_arm_opnd(RVALUE);
        checkfor_ket(s_comma);
        asmopnd4_(a) = rd_arm_opnd(RVALUE);
    }
}


static void rd_psr_field(AsmInstr *a, bool flags_only)
{
    const char *str = symname_(curlex.a1.sv);
    ID psr;

    if (curlex.sym != s_identifier)
    {
        cc_err(asm_err_expected_psr);
        asmopnd2_(a) = errornode;
        nextsym();
        return;
    }
    psr = read_substring(&str, psr_tab, SizeofArr (psr_tab), Str_PSR);
    if (flags_only && (psr & PSR_FLAGS) != PSR_CF)
        psr = NIL_ID;
    if ((psr & PSR_FLAGS) == 0)           /* read: CSPR_ or SPSR_ */
    {
        if (StrEq(str, "flg"))
        {
            str += 3;
            psr |= PSR_F;
        }
        else if (StrEq(str, "ctl"))
        {
            str += 3;
            psr |= PSR_C;
        }
        else if (StrEq(str, "all"))
        {
            str += 3;
            psr |= PSR_CF;
        }
        else
            while (1)
            {   /* read the cxsf flags in any order */
                if (~psr & PSR_C && *str == 'c')
                {   str++; psr |= PSR_C; continue; }
                if (~psr & PSR_X && *str == 'x')
                {   str++; psr |= PSR_X; continue; }
                if (~psr & PSR_S && *str == 's')
                {   str++, psr |= PSR_S; continue; }
                if (~psr & PSR_F && *str == 'f')
                {   str++; psr |= PSR_F; continue; }
                break;
            }
    }
    if (psr == NIL_ID || *str != '\0')
    {
        cc_err(asm_err_bad_psrfield, symname_(curlex.a1.sv));
        asmopnd2_(a) = errornode;
        return;
    }
    nextsym();
    asmopnd2_(a) = mkintconst(te_uint, psr, 0);
}


static void rd_psr_instr(AsmInstr *a)
{
    if (!(config & CONFIG_32BIT))
        cc_err(asm_err_no_mrs);
    if (OPCODE(asmopcode_(a)) == A_MRS)
    {
        asmopnd1_(a) = rd_arm_opnd(LVALUE);
        checkfor_ket(s_comma);
        rd_psr_field(a, YES);
    }
    else    /* MSR */
    {
        rd_psr_field(a, NO);
        checkfor_ket(s_comma);
        rd_opnd_rn(a, NO_SHIFT);
    }
}


static void rd_swi_instr(AsmInstr *a)
{
    uint32 in = 0, out = 0, corrupt = 0;

    asmopnd1_(a) = rd_arm_opnd(CONSTVALUE);
    if (curlex.sym == s_comma)
    {
        nextsym();
        in = rd_reglist();
    }
    if (curlex.sym == s_comma)
    {
        nextsym();
        out = rd_reglist();
    }
    if (curlex.sym == s_comma)
    {
        nextsym();
        corrupt = rd_reglist();
    }
    if ((in | out | corrupt) == NIL_ID)
    {
        asmopnd1_(a) = errornode;
        return;
    }
    asmopnd2_(a) = mkintconst(te_uint, in, 0);
    asmopnd3_(a) = mkintconst(te_uint, out, 0);
    asmopnd4_(a) = mkintconst(te_uint, corrupt, 0);
    if ((out | corrupt) & regbit(R_PSR))
        asmopcode_(a) |= SET_CC;
}


static void rd_swp_instr(AsmInstr *a)
{
    asmopnd1_(a) = rd_arm_opnd(LVALUE);
    checkfor_ket(s_comma);
    asmopnd3_(a) = rd_arm_opnd(RVALUE);
    checkfor_ket(s_comma);
    checkfor_ket(s_lbracket);
    asmopnd2_(a) = rd_arm_opnd(RVALUE);
    checkfor_ket(s_rbracket);
}


static void rd_branch_instr(AsmInstr *a)
{
    if (OPCODE(asmopcode_(a)) == A_B)
    {
        if (curlex.sym != s_identifier)
        {
            cc_err(syn_err_no_label);
            return;
        }
        asmopnd1_(a) = (Expr*) label_reference(curlex.a1.sv);
        nextsym();
    }
    else
    {
        uint32 in = 0, out = 0, corrupt = reglist(R_A1, 4) | regbit(R_IP);;

        if (curlex.sym != s_identifier)
        {
            cc_err(asm_err_fn_id);
            asmopnd1_(a) = errornode;
        }
        else
        {
            Expr *fn = (Expr *) findbinding(curlex.a1.sv, NULL, ALLSCOPES);
            if (fn != NULL)
            {   /* generate mangled name for C++ if possible */
                fn = mkunary(s_addrof, fn);
                fn = coerceunary(fn);
                asmopnd1_(a) = arg1_(fn);
            }
        }
        if (asmopnd1_(a) == 0)
        {
            cc_err(asm_err_fn_notfound, symname_(curlex.a1.sv));
            asmopnd1_(a) = errornode;
        }
        nextsym();

        if (curlex.sym == s_comma)
        {
            nextsym();
            in = rd_reglist();
        }
        if (curlex.sym == s_comma)
        {
            nextsym();
            out = rd_reglist();
        }
        if (curlex.sym == s_comma)
        {
            nextsym();
            corrupt = rd_reglist();
        }
        if ((in | out | corrupt) == NIL_ID)
        {
            asmopnd1_(a) = errornode;
            return;
        }
        corrupt |= regbit(R_LR);    /* BL always corrupts LR */
        corrupt &= ~out;            /* result registers cannot be corrupted */
        asmopnd2_(a) = mkintconst(te_uint, in, 0);
        asmopnd3_(a) = mkintconst(te_uint, out, 0);
        asmopnd4_(a) = mkintconst(te_uint, corrupt, 0);
        if ((out | corrupt) & regbit(R_PSR))
            asmopcode_(a) |= SET_CC;
    }
}


static int rd_coproc_id(void)
{
    ID coproc;

    if (curlex.sym != s_identifier)
    {
        cc_err(asm_err_expected_coproc);
        nextsym();
        return NIL_ID;
    }
    coproc = read_string(symname_(curlex.a1.sv), coproc_id_tab,
                         SizeofArr (coproc_id_tab), Str_Coproc);
    if (coproc == NIL_ID)
        cc_err(asm_err_unknown_coproc, symname_(curlex.a1.sv));
    else
        coproc <<= 8;
    nextsym();
    return coproc;
}


static int rd_coproc_op(uint32 max)
{
    Expr *op = rd_arm_opnd(CONSTVALUE);
    uint32 opcode;
    if (op == errornode)
        return NIL_ID;
    opcode = intval_(op);
    if (opcode >= max)
    {
        cc_err(asm_err_coproc_op_range, opcode);
        return NIL_ID;
    }
    return opcode;
}


static void rd_coproc_dataop(AsmInstr *a)
{
    uint32 coproc, op;
    coproc = rd_coproc_id();
    checkfor_ket(s_comma);
    if (OPCODE(asmopcode_(a)) == A_CDP)
    {   op = rd_coproc_op(16);
        coproc |= (op == NIL_ID) ? NIL_ID : op << 20;
        checkfor_ket(s_comma);
        asmopnd1_(a) = rd_coproc_reg();
    }
    else
    {   op = rd_coproc_op(8);
        coproc |= (op == NIL_ID) ? NIL_ID : op << 21;
        checkfor_ket(s_comma);
        asmopnd1_(a) = rd_arm_opnd(OPCODE(asmopcode_(a)) == A_MRC ? LVALUE : RVALUE);
    }
    checkfor_ket(s_comma);
    asmopnd2_(a) = rd_coproc_reg();
    checkfor_ket(s_comma);
    asmopnd3_(a) = rd_coproc_reg();
    if (curlex.sym == s_comma)
    {   nextsym();
        op = rd_coproc_op(8);
    }
    else
        op = 0;
    coproc |= (op == NIL_ID) ? NIL_ID : op << 5;
    if (coproc == NIL_ID)
        asmopnd4_(a) = errornode;
    else
        asmopnd4_(a) = mkintconst(te_uint, coproc, 0);
}


static void rd_coproc_memop(AsmInstr *a)
{
    uint32 coproc;
    coproc = rd_coproc_id();
    checkfor_ket(s_comma);
    asmopnd1_(a) = rd_coproc_reg();
    checkfor_ket(s_comma);
    checkfor_ket(s_lbracket);
    asmopcode_(a) |= RN_CONST;
    asmopnd2_(a) = rd_arm_opnd(RVALUE);
    if (curlex.sym == s_comma)
    {   nextsym();
        if (curlex.sym == s_hash) nextsym();
        asmopcode_(a) |= M_PREIDX;
        asmopnd3_(a) = rd_arm_opnd(CONSTVALUE);
        checkfor_ket(s_rbracket);
        if (curlex.sym == s_boolnot)
        {   nextsym();
            asmopcode_(a) |= M_WB;
        }
    }
    else
    {   checkfor_ket(s_rbracket);
        if (curlex.sym != s_comma)      /* no offset, so default to [rx, #0] */
        {   asmopcode_(a) |= M_PREIDX;
            asmopnd3_(a) = mkintconst(te_uint, 0, 0);
        }
        else
        {   asmopcode_(a) |= M_WB;
            nextsym();
            if (curlex.sym == s_hash) nextsym();
            asmopnd3_(a) = rd_arm_opnd(CONSTVALUE);
        }
    }
    if (coproc == NIL_ID)
        asmopnd4_(a) = errornode;
    else
        asmopnd4_(a) = mkintconst(te_uint, coproc, 0);
}


static void rd_thumb_binary(AsmInstr *a)
{
    asmopnd1_(a) = asmopnd2_(a) = rd_thumb_opnd(LVALUE);
    checkfor_ket(s_comma);
    if (curlex.sym == s_hash)
    {   nextsym();
        asmopnd3_(a) = rd_thumb_opnd(CONSTVALUE);
    }
    else
    {   asmopnd3_(a) = rd_thumb_opnd(RVALUE);
        if (curlex.sym == s_comma)
        {   nextsym();
            asmopnd2_(a) = asmopnd3_(a);
            asmopnd3_(a) = rd_thumb_opnd(OPND_RN);
        }
    }
}


static void rd_thumb_move(AsmInstr *a)
{
    asmopnd1_(a) = rd_thumb_opnd(LVALUE);
    checkfor_ket(s_comma);
    asmopnd3_(a) = rd_thumb_opnd(OPND_RN);
}


static void rd_thumb_compare(AsmInstr *a)
{
    asmopnd2_(a) = rd_thumb_opnd(RVALUE);
    checkfor_ket(s_comma);
    asmopnd3_(a) = rd_thumb_opnd(OPND_RN);
}


static void rd_thumb_mem(AsmInstr *a)
{
    asmopnd1_(a) = rd_thumb_opnd(OPCODE(asmopcode_(a)) == A_LDR ? LVALUE : RVALUE);
    checkfor_ket(s_comma);
    checkfor_ket(s_lbracket);
    asmopnd2_(a) = rd_thumb_opnd(RVALUE);
    if (curlex.sym == s_comma)
    {   nextsym();
        asmopnd3_(a) = rd_thumb_opnd(OPND_RN);
    }
    else
        asmopnd3_(a) = mkintconst(te_uint, 0, 0);
    checkfor_ket(s_rbracket);
}


static void rd_thumb_ldm(AsmInstr *a)
{   uint32 regmask;
    if (asmopcode_(a) & MM_STACK)
    {   asmopnd1_(a) = mkintconst(te_uint, R_SP, 0);
        h0_(asmopnd1_(a)) = s_register;
    }
    else
    {   asmopnd1_(a) = rd_thumb_opnd(LVALUE);
        checkfor_ket(s_boolnot);
        checkfor_ket(s_comma);
    }
    regmask = rd_reglist();
    asmopnd3_(a) = errornode;
    if (regmask != NIL_ID)
    {   uint32 mask_allowed = 0xFF;
        if (asmopcode_(a) & MM_STACK)
            mask_allowed |= OPCODE(asmopcode_(a)) == A_LDM ? 0x8000 : 0x4000;
        if (regmask & ~mask_allowed)
            cc_err(asm_err_ldm_badlist);
        else
            asmopnd3_(a) = mkintconst(te_uint, regmask, 0);
    }
    asmopcode_(a) &= ~MM_STACK;
}


static void rd_thumb_swi(AsmInstr *a)
{
    rd_swi_instr(a);
}


static void rd_thumb_branch(AsmInstr *a)
{
    rd_branch_instr(a);
}


static AsmInstr *rd_arm_instr(void)
{
    ID op;
    AsmInstr *a;

    a = mk_asminstr();
    NoteFileLine(&asmfl_(a));
    if (peepsym() == s_colon)       /* found a label */
    {   LabBind *lab = label_define(curlex.a1.sv);
     /*   if (lab != 0) syn_setlab(lab, synscope);  */
        op = A_LABEL + CC_NOT;
        asmopnd1_(a) = (Expr*) lab;
        nextsym();
    }
    else
        op = arm_opcode(symname_(curlex.a1.sv));
    asmopcode_(a) = op;
    if (op == NIL_ID)
    {   cc_err(asm_err_bad_opcode, symname_(curlex.a1.sv));
        nextsym();
        return 0;
    }
    nextsym();
    switch (INSTRCL(op))
    {
        case CL_MOV:
            rd_move_instr(a);
            break;
        case CL_CMP:
            rd_compare_instr(a);
            break;
        case CL_BIN:
            rd_binary_instr(a);
            break;
        case CL_MEM:
            rd_mem_instr(a);
            break;
        case CL_LDM:
            rd_ldm_instr(a);
            break;
        case CL_MUL:
        case CL_MULL:
            rd_mul_instr(a);
            break;
        case CL_PSR:
            rd_psr_instr(a);
            break;
        case CL_SWI:
            rd_swi_instr(a);
            break;
        case CL_SWP:
            rd_swp_instr(a);
            break;
        case CL_BR:
            rd_branch_instr(a);
            break;
        case CL_COP:
            rd_coproc_dataop(a);
            break;
        case CL_CMEM:
            rd_coproc_memop(a);
            break;
        default:
            break;
    }
    if (asmopnd1_(a) == errornode || asmopnd2_(a) == errornode ||
        asmopnd3_(a) == errornode || asmopnd4_(a) == errornode)
        return 0;
    return a;
}


static AsmInstr *rd_thumb_instr(void)
{
    ID op;
    AsmInstr *a;

    a = mk_asminstr();
    NoteFileLine(&asmfl_(a));
    if (peepsym() == s_colon)       /* found a label */
    {   LabBind *lab = label_define(curlex.a1.sv);
     /*   if (lab != 0) syn_setlab(lab, synscope);  */
        op = A_LABEL + CC_NOT;
        asmopnd1_(a) = (Expr*) lab;
        nextsym();
    }
    else
        op = thumb_opcode(symname_(curlex.a1.sv));
    asmopcode_(a) = op;
    if (op == NIL_ID)
    {   cc_err(asm_err_bad_opcode, symname_(curlex.a1.sv));
        nextsym();
        return 0;
    }
    nextsym();
    switch (INSTRCL(op))
    {
        case CL_MOV:
            rd_thumb_move(a);
            break;
        case CL_CMP:
            rd_thumb_compare(a);
            break;
        case CL_BIN:
        case CL_MUL:
        case CL_SH:
            rd_thumb_binary(a);
            break;
        case CL_MEM:
            rd_thumb_mem(a);
            break;
        case CL_LDM:
            rd_thumb_ldm(a);
            break;
        case CL_SWI:
            rd_thumb_swi(a);
            break;
        case CL_BR:
            rd_branch_instr(a);
            break;
        default:
            break;
    }
    if (asmopnd1_(a) == errornode || asmopnd2_(a) == errornode ||
        asmopnd3_(a) == errornode || asmopnd4_(a) == errornode)
        return 0;
    return a;
}


static AsmInstr *rd_asm_instr_list(bool quoted)
{
    AsmInstr *ls = 0, *le = 0, *a;
    AEop endsym = quoted ? s_quote : s_rbrace;

    if (quoted) checkfor_ket(s_quote);
    for (;;)
    {
        if (curlex.sym == s_identifier)
        {
#ifdef ARM_INLINE_ASSEMBLER
            a = rd_arm_instr();
#endif
#ifdef THUMB_INLINE_ASSEMBLER
            a = rd_thumb_instr();
#endif
            if (a != 0)
            {   if (le) le->cdr = a; else ls = a;
                le = a;
                if (OPCODE(asmopcode_(a)) == A_LABEL) continue;
            }
            else  /* syntax error: read till a save point */
                while (curlex.sym != endsym &&
                       curlex.sym != s_semicolon &&
                       curlex.sym != s_eol &&
                       curlex.sym != s_eof)
                    nextsym();
        }
        if (curlex.sym == endsym || curlex.sym == s_eof)
            break;
        if (curlex.sym == s_eol || curlex.sym == s_semicolon)
            nextsym();
        else
            checkfor_2ket(s_semicolon, quoted ? s_quote : s_eol);
    }
    if (quoted) checkfor_ket(s_quote);
    return ls;
}


Cmd *rd_asm_block(void)
{
    FileLine fl;
    AsmInstr *asmlist = 0;

    NoteFileLine(&fl);
    nextsym();
    asm_mode = ASM_BLOCK;
    checkfor_ket(s_lbrace);
    asmlist = rd_asm_instr_list(NO);
    asm_mode = ASM_NONE;
    while (curlex.sym == s_eol) nextsym();
    checkfor_ket(s_rbrace);
    return mk_cmd_e(s_asm, fl, (Expr *)asmlist);
}

#endif

Expr *rd_asm_decl(void)
{
#ifdef TARGET_HAS_INLINE_ASSEMBLER
    FileLine fl;
    AsmInstr *asmlist = 0;

    NoteFileLine(&fl);
    nextsym();
    asm_mode = ASM_STRING;
    checkfor_ket(s_lpar);
    while (curlex.sym == s_eol) nextsym();
    asmlist = rd_asm_instr_list(YES);
    asm_mode = ASM_NONE;
    while (curlex.sym == s_eol) nextsym();
    checkfor_ket(s_rpar);
    checkfor_ket(s_semicolon);
    return (Expr*) mk_cmd_e(s_asm, fl, (Expr *)asmlist);
#else
    {
        Expr *s = 0;
        nextsym();
        checkfor_ket(s_lpar);
        if (curlex.sym == s_string)
            s = rd_ANSIstring();
        else
            checkfor_ket(s_string);
        checkfor_ket(s_rpar);
        checkfor_delimiter_ket(s_semicolon, "", "");
        cc_warn(syn_warn_ineffective_asm_decl);
        return s;
    }
#endif
}
