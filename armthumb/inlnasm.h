/*
 * inlnasm.h: ARM/Thumb inline assembler header
 * Copyright (C) Advanced Risc Machines Ltd., 1997
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _inlnasm_h
#define _inlnasm_h


#ifdef TARGET_HAS_INLINE_ASSEMBLER

#include "cgdefs.h"
#include "jopcode.h"
#include "mcdpriv.h"

#define ASM_NONE   0
#define ASM_STRING 1
#define ASM_BLOCK  2


#define MKOP(op,icl) ((op)|((icl)<<16))
#define OPCODE(opcode) ((opcode)&255)
#define INSTRCL(opcode) (((opcode)>>16)&15)
#define GETCC(opcode) ((opcode)&0xf0000000)
#define GETSHIFT(opcode)((opcode)&0x00007000)
#define INVCC(cc) ((cc)^0x10000000)

#define CC_EQ 0x00000000U
#define CC_NE 0x10000000U
#define CC_CS 0x20000000U
#define CC_CC 0x30000000U
#define CC_MI 0x40000000U
#define CC_PL 0x50000000U
#define CC_VS 0x60000000U
#define CC_VC 0x70000000U
#define CC_HI 0x80000000U
#define CC_LS 0x90000000U
#define CC_GE 0xa0000000U
#define CC_LT 0xb0000000U
#define CC_GT 0xc0000000U
#define CC_LE 0xd0000000U
#define CC_AL 0xe0000000U
#define CC_NOT 0xf0000000U

#define SET_CC 0x08000000
#define SET_PSR 0x04000000

enum {
 M_SIGNED = 0x0100,
 M_BYTE   = 0x0200,
 M_HALF   = 0x0400,
 M_WORD   = 0x0000,
 M_LONG   = 0x0100,     /* for LDC/STC */

 M_PREIDX = 0x1000,
 M_UPIDX  = 0x2000,
 M_WB     = 0x4000,
 M_TRANS  = 0x8000,     /* LDRT/STRT */

 MM_DA    = 0x0000,
 MM_DB    = 0x1000,
 MM_IA    = 0x2000,
 MM_IB    = 0x3000,
 MM_TYPE  = 0x3000,

 MM_STACK = 0x0100      /* temporary flag LDM/STM */
};


enum {
 SH_LSL = 0x00000000,
 SH_LSR = 0x00100000,
 SH_ASR = 0x00200000,
 SH_ROR = 0x00300000,
 SH_RRX = 0x00400000,
 SH_MASK= 0x00700000
};

enum {
 RN_REG         = 0x00000000,
 RN_CONST       = 0x01000000,
 RN_SHIFT       = 0x02000000,
 RN_SHIFT_REG   = 0x03000000,
 RN_OPND_MASK   = 0x03000000
};

enum {
    CPSR = 0,
    SPSR = 16,
    PSR_F = 8,
    PSR_S = 4,
    PSR_X = 2,
    PSR_C = 1,
    PSR_CF = PSR_C + PSR_F,
    PSR_FLAGS = PSR_F+PSR_S+PSR_X+PSR_C
};

/* instruction classes */
enum {
 CL_NONE,
 CL_MOV,
 CL_CMP,
 CL_BIN,
 CL_MEM,
 CL_LDM,
 CL_MUL,
 CL_MULL,
 CL_SWI,
 CL_SWP,
 CL_PSR,
 CL_BR,
 CL_COP,
 CL_CMEM,
 CL_NOP,
 CL_SH
};


enum {
 A_AND = 0x00,
 A_EOR = 0x01,
 A_SUB = 0x02,
 A_RSB = 0x03,
 A_ADD = 0x04,
 A_ADC = 0x05,
 A_SBC = 0x06,
 A_RSC = 0x07,
 A_TST = 0x08,
 A_TEQ = 0x09,
 A_CMP = 0x0a,
 A_CMN = 0x0b,
 A_ORR = 0x0c,
 A_MOV = 0x0d,
 A_BIC = 0x0e,
 A_MVN = 0x0f,

 A_MRS = 0x10,
 A_MSR = 0x11,

 A_MUL = 0x12,
 A_MLA = 0x13,
 A_MULL= 0x14,
 A_MLAL= 0x15,

 A_LDR = 0x16,
 A_STR = 0x17,
 A_LDM = 0x18,
 A_STM = 0x19,

 A_SWP = 0x1a,

 A_SWI = 0x1b,
 A_B   = 0x1c,
 A_BL  = 0x1d,

 A_CDP = 0x1e,
 A_MRC = 0x1f,
 A_MCR = 0x20,
 A_LDC = 0x21,
 A_STC = 0x22,

 A_NOP = 0x23,
 A_LABEL = 0x24,

 NUM_ARM_OPCODES = 0x25,

 T_NEG = 0x25,
 T_ASR = 0x26,
 T_LSL = 0x27,
 T_LSR = 0x28,
 T_ROR = 0x29

};


extern void cg_asm(Cmd *c);
extern void translate_asm_instr(PendingOp *cur);
extern int32 trans_cc_tab [16];

#define translate_cc(asmcc) (trans_cc_tab[GETCC(asmcc)>>28])

#define is_asminstr(op) ((op & J_TABLE_BITS) >= J_FIRST_ASM_JOPCODE && \
                         (op & J_TABLE_BITS) <= J_LAST_ASM_JOPCODE)
#define asminstr(op)   OPCODE(op)

#else

#define SET_CC 0
#define cg_asm (c)
#define translate_asm_instr(c)

#define is_asminstr(op) 0

#endif

extern Cmd *rd_asm_block(void);
extern Expr *rd_asm_decl(void);

#endif


