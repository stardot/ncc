/*
 * C compiler file thumb/ops.h, version 1
 * Copyright (C) Codemist Ltd., 1994
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _thumbops_LOADED
#define _thumbops_LOADED 1

/* ARM opcodes used by Thumb */

#define ARM_AL          0xe0000000L

#define ARM_B           0x0a000000L
#define ARM_BX          0x012fff10L

#define ARM_LDR_IP_PC_0 0x059fc000L

#define ARM_ADDI3       0x02800000L
#define ARM_SUBI3       0x02400000L

/* Thumb opcodes... */

/* Condition fields are defined relative to the default C_ALWAYS value   */

#define C_CC         0x300L
#define C_LO         0x300L
#define C_CS         0x200L
#define C_HS         0x200L
#define C_EQ         0x000L
#define C_GE         0xa00L
#define C_GT         0xc00L
#define C_HI         0x800L
#define C_LE         0xd00L
#define C_LS         0x900L
#define C_LT         0xb00L
#define C_MI         0x400L
#define C_NE         0x100L
#define C_PL         0x500L
#define C_VC         0x700L
#define C_VS         0x600L
#define C_AL         0xe00L

#define C_of_Q(a)   c_of_q(a)

/* F1, F2 and F3 opcodes */

#define F_ADDI3    0x1c00L
#define F_SUBI3    0x1e00L

#define F_ADD3R    0x1800L
#define F_SUB3R    0x1a00L

#define F_STRHADDR  0x5200L
#define F_LDRHADDR  0x5a00L

#define F_LDSBADDR  0x5600L
#define F_LDSHADDR  0x5e00L

#define F_STRHI5    0x8000L
#define F_LDRHI5    0x8800L

/* F4 opcodes */

#define F_LSLK     0x0000L
#define F_LSRK     0x0800L
#define F_ASRK     0x1000L

/* F5 opcodes */

#define F_AND      0x4000L
#define F_EOR      0x4040L
#define F_LSL      0x4080L
#define F_LSR      0x40c0L

#define F_ASR      0x4100L
#define F_ADC      0x4140L
#define F_SBC      0x4180L
#define F_ROR      0x41c0L

#define F_TST      0x4200L
#define F_NEG      0x4240L
#define F_CMP      0x4280L
#define F_CMN      0x42c0L

#define F_OR       0x4300L
#define F_MUL      0x4340L
#define F_BIC      0x4380L
#define F_MVN      0x43c0L

#define IS_DATAPROC(op) (((op) & 0xfc00) == 0x4000)

#define F_ADDHL    0x4440L
#define F_ADDLH    0x4480L
#define F_ADDHH    0x44c0L

#define F_CMPHL    0x4540L
#define F_CMPLH    0x4580L
#define F_CMPHH    0x45c0L

#define F_MOVHL    0x4640L
#define F_MOVLH    0x4680L
#define F_MOVHH    0x46c0L

#define F_BX_L     0x4700L
#define F_BX_H     0x4740L

/* F6 opcodes */

#define F_MOV8     0x2000L
#define F_CMP8     0x2800L
#define F_ADDI8    0x3000L
#define F_SUBI8    0x3800L

/* F12 opcodes */

#define F_LDRLIT   0x4800L    /* address is PC relative */

/* F8 opcodes */

#define F_STRSP    0x9000L
#define F_LDRSP    0x9800L

/* F9 opcodes */

#define F_B        0xe000L

/* F10 opcodes */

#define F_PUSH     0xb400L     /* LR is 0x100 bit */
#define F_POP      0xbc00L     /* PC is 0x100 bit */

/* F13 opcodes (pre-index with register offset) */

#define F_STRADDR  0x5000L
#define F_STRBADDR 0x5400L
#define F_LDRADDR  0x5800L
#define F_LDRBADDR 0x5c00L

/* F14 opcodes */

#define F_STRI5    0x6000L
#define F_LDRI5    0x6800L
#define F_STRBI5   0x7000L
#define F_LDRBI5   0x7800L

/* F15 opcodes */

#define F_STM      0xc000L
#define F_LDM      0xc800L

/* F16 opcodes */

#define F_ADDRPC   0xa000L
#define F_ADDRSP   0xa800L

/* F17 opcodes */

#define F_BC       0xd000L   /* must have branch condition included */

/* F18 opcodes */

#define F_ADDSP    0xb000L   /* Note ... now sign & magnitude */
#define F_SUBSP    0xb080L

/* F19 opcodes */

#define F_SWI      0xdf00L

/* F20, F21 opcodes */

#define F_CALL1    0xf000L   /* prefix for a F_CALL */
#define F_CALL     0xf800L

/*
 * Register names - some defined in target.h.
 */

/* #define R_A1 0x0L      * arg 1 & main result register                 */
#define R_A2    0x1L     /* arg 2                                        */
#define R_A3    0x2L     /* arg 3                                        */
#define R_A4    0x3L     /* arg 4                                        */
/* #define R_V1 0x4L      * register variable 1                          */
#define R_V2    0x5L     /* register variable 2                          */
#define R_V3    0x6L     /* register variable 3                          */
#define R_V4    0x7L     /* register variable 4                          */
#define R_V5    0x8L     /* register variable 5 (not allocated)          */
#define R_V6    0x9L     /* register variable 6 (not allocated)          */
/* define R_IP  0x7L */

#  define R_SB  0x9L
#  define R_FP  0xbL     /* Frame pointer                                */
/* #  define R_SP  0xdL     main stack pointer                           */
#  define R_SL  0xaL     /* stack limit (usually not checked)            */
/* #  define R_LR  0xeL   * link address in function calls + workspace   */
#define R_PC    0xfL     /* program counter                              */
/* #define R_PSR   R_PC     program status register (register allocation)*/

/* Calling a function can disturb r_a1 to r_a4, r_ip, r_lk but must      */
/* preserve r_v1 to r_v3, r_fp and r_sp. r_sl must always be a valid     */
/* stack limit value - it may be changed during a call is a stack        */
/* has its size changed.                                                 */

/* LDM/STM masks for real registers.                                     */
/* **** $$$$$ BEWARE: on arm these are used in general register fields and
   to show reg usage in ldm/stm. In thumb, ldm and stm only allow 8 regs +
   lr or pc. Don't use these in thumb ldm and stm.
*/
#define M_LR            regbit(R_LR)
#define M_PC            regbit(R_PC)
#define M_ARGREGS       (regbit(R_A1+NARGREGS)-regbit(R_A1))
#define M_VARREGS       (regbit(R_V1+NVARREGS)-regbit(R_V1))
#define M_FARGREGS      0
#define M_FVARREGS      0

#define PC_OFFSET 4

#endif

/* end of thumb/ops.h */
