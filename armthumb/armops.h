/*
 * C compiler file armthumb/armops.h, version 3
 * Copyright (C) Codemist Ltd., 1987
 * SPDX-Licence-Identifier: Apache-2.0
 * (Was arm/ops.h)
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _armops_LOADED
#define _armops_LOADED 1

/* AM some lines commented out temporarily and duplicated in mcspec.h */

/* ARM opcodes... */

/* Condition fields are defined relative to the default C_ALWAYS value   */
#define C_ALWAYS     0xe0000000
#define C_CC        (0x30000000^C_ALWAYS)
#define C_LO        (0x30000000^C_ALWAYS)
#define C_CS        (0x20000000^C_ALWAYS)
#define C_HS        (0x20000000^C_ALWAYS)
#define C_EQ        (0x00000000^C_ALWAYS)
#define C_GE        (0xa0000000^C_ALWAYS)
#define C_GT        (0xc0000000^C_ALWAYS)
#define C_HI        (0x80000000^C_ALWAYS)
#define C_LE        (0xd0000000^C_ALWAYS)
#define C_LS        (0x90000000^C_ALWAYS)
#define C_LT        (0xb0000000^C_ALWAYS)
#define C_MI        (0x40000000^C_ALWAYS)
#define C_NE        (0x10000000^C_ALWAYS)
#define C_NEVER     (0xf0000000^C_ALWAYS)
#define C_PL        (0x50000000^C_ALWAYS)
#define C_VC        (0x70000000^C_ALWAYS)
#define C_VS        (0x60000000^C_ALWAYS)

/* Major opcode groups                                                   */

#define OP_RXR      0x00000000L
/*                  0x01000000L     part of RXR                          */

#define OP_MUL      0x00000090L
#define OP_MLA      0x00200090L
#define OP_UMULL    0x00800090L
#define OP_SMULL    0x00C00090L
#define OP_UMLAL    0x00A00090L
#define OP_SMLAL    0x00E00090L

#define OP_RXN      0x02000000L
/*                  0x03000000L     part of RXN                          */

#define OP_POSTN    0x04000000L
#define OP_PREN     0x05000000L
#define OP_POSTR    0x06000000L
#define OP_PRER     0x07000000L

#define OP_H_POSTN  0x00400090L
#define OP_H_PREN   0x01400090L
#define OP_H_POSTR  0x00000090L
#define OP_H_PRER   0x01000090L

#define OP_BX       0x012fff10L

/*                  0x08000000L     part of LDM/STM                      */
/*                  0x09000000L     part of LDM/STM                      */

#define OP_B        0x0a000000L
#define OP_BL       0x0b000000L

#define OP_CPPOST   0x0c000000L     /* needed for floating point         */
#define OP_CPPRE    0x0d000000L
#define OP_CPOP     0x0e000000L

#define OP_SWI      0x0f000000L

#define OP_CLASS    0x0f000000L

/* Subfields for the data processing opcodes                             */

#define F_ADC       (0x5L<<21L)
#define F_ADD       (0x4L<<21L)
#define F_AND       (0x0L<<21L)
#define F_BIC       (0xeL<<21L)
#define F_CMN       (0xbL<<21L)
#define F_CMP       (0xaL<<21L)
#define F_EOR       (0x1L<<21L)
#define F_MOV       (0xdL<<21L)
#define F_MVN       (0xfL<<21L)
#define F_ORR       (0xcL<<21L)
#define F_RSB       (0x3L<<21L)
#define F_RSC       (0x7L<<21L)
#define F_SBC       (0x6L<<21L)
#define F_SUB       (0x2L<<21L)
#define F_TEQ       (0x9L<<21L)
#define F_TST       (0x8L<<21L)

#define F_DPOP      (0xfL<<21)

#define OP_MRS      0x010F0000L
#define OP_MSR      0x0120F000L
#define K_SPSR      (1<<22)                            /* CPSR/SPSR bit */

#define OP_SWP      0x01000090L
#define OP_SWPB     0x01400090L

#define OP_CDP      0x0E000000L
#define OP_MRC      0x0E100010L
#define OP_MCR      0x0E000010L
#define OP_LDC      0x0C100000L
#define OP_STC      0x0C000000L
#define F_WB        (1<<21)
#define F_LONG      (1<<22)
/* #define F_UP        (1<<23) already defined */
#define F_PRE       (1<<24)

#define F_SCC       0x00100000L
#define F_RN(n)     (((int32)(n))<<16L)
#define F_RD(n)     (((int32)(n))<<12L)
#define F_RM(n)     ((int32)(n))

/* The next few are for use with RXR format (and corresponding memory    */
/* access instructions)                                                  */

#define K_NOSHIFT   0L
#define K_LSL(n)    (((int32)(n))<<7L)                  /* 0 to 31       */
#define K_LSR(n)    (((((int32)(n))&0x1fL)<<7L)|0x20L)  /* 1 to 32       */
#define K_ASR(n)    (((((int32)(n))&0x1fL)<<7L)|0x40L)  /* 1 to 32       */
#define K_ROR(n)    ((((int32)(n))<<7L)|0x60L)          /* 1 to 31       */
#define K_RRX       0x60L                  /* one bit with extend        */
#define R_LSL(r)    ((((int32)(r))<<8L)|0x10L)   /* register shift value */
#define R_LSR(r)    ((((int32)(r))<<8L)|0x30L)   /* register shift value */
#define R_ASR(r)    ((((int32)(r))<<8L)|0x50L)   /* register shift value */
#define R_ROR(r)    ((((int32)(r))<<8L)|0x70L)   /* register shift value */


/* subfields for memory reference instructions                           */

#define F_LDR       0x00100000L
#define F_STR       0x00000000L
#define F_LDRSTR_FIELD F_LDR

#define F_WRITEBACK 0x00200000L  /* = T bit in postindexed modes         */

#define F_BYTE      0x00400000L
#define F_WORD      0x00000000L
#define F_BYTEWORD_FIELD F_BYTE

#define F_SBYTE     0x00000040L
#define F_UHALF     0x00000020L
#define F_SHALF     0x00000060L

#define F_UP        0x00800000L
#define F_DOWN      0x00000000L
#define F_UPDOWN_FIELD F_UP

/* Block data subfields                                                  */

#define OP_LDMFA    0x08100000L
#define OP_LDMEA    0x09100000L
#define OP_LDMFD    0x08900000L
#define OP_LDMED    0x09900000L

#define OP_STMFA    0x09800000L
#define OP_STMEA    0x08800000L
#define OP_STMFD    0x09000000L
#define OP_STMED    0x08000000L

/* Versions for use when I am not thinking in stack terms */

#define OP_LDMDA    0x08100000L
#define OP_LDMDB    0x09100000L
#define OP_LDMIA    0x08900000L
#define OP_LDMIB    0x09900000L

#define OP_STMIB    0x09800000L
#define OP_STMIA    0x08800000L
#define OP_STMDB    0x09000000L
#define OP_STMDA    0x08000000L

#define OP_LDR  (OP_PREN|F_LDR|F_WORD)
#define OP_LDRB (OP_PREN|F_LDR|F_BYTE)
#define OP_STR  (OP_PREN|F_STR|F_WORD)
#define OP_STRB (OP_PREN|F_STR|F_BYTE)

#define OP_LDRR  (OP_PRER|F_LDR|F_WORD)
#define OP_LDRBR (OP_PRER|F_LDR|F_BYTE)
#define OP_STRR  (OP_PRER|F_STR|F_WORD)
#define OP_STRBR (OP_PRER|F_STR|F_BYTE)

#define OP_ADDR (OP_RXR | F_ADD)
#define OP_ADCR (OP_RXR | F_ADC)
#define OP_ANDR (OP_RXR | F_AND)
#define OP_BICR (OP_RXR | F_BIC)
#define OP_CMPR (OP_RXR | F_CMP | F_SCC)
#define OP_CMNR (OP_RXR | F_CMN | F_SCC)
#define OP_EORR (OP_RXR | F_EOR)
#define OP_MOVR (OP_RXR | F_MOV)
#define OP_MVNR (OP_RXR | F_MVN)
#define OP_ORRR (OP_RXR | F_ORR)
#define OP_RSBR (OP_RXR | F_RSB)
#define OP_RSCR (OP_RXR | F_RSC)
#define OP_SUBR (OP_RXR | F_SUB)
#define OP_SBCR (OP_RXR | F_SBC)
#define OP_TEQR (OP_RXR | F_TEQ | F_SCC)
#define OP_TSTR (OP_RXR | F_TST | F_SCC)

#define OP_ADDN (OP_RXN | F_ADD)
#define OP_ADCN (OP_RXN | F_ADC)
#define OP_ANDN (OP_RXN | F_AND)
#define OP_BICN (OP_RXN | F_BIC)
#define OP_CMNN (OP_RXN | F_CMN | F_SCC)
#define OP_CMPN (OP_RXN | F_CMP | F_SCC)
#define OP_EORN (OP_RXN | F_EOR)
#define OP_MOVN (OP_RXN | F_MOV)
#define OP_MVNN (OP_RXN | F_MVN)
#define OP_ORRN (OP_RXN | F_ORR)
#define OP_RSBN (OP_RXN | F_RSB)
#define OP_RSCN (OP_RXN | F_RSC)
#define OP_SUBN (OP_RXN | F_SUB)
#define OP_SBCN (OP_RXN | F_SBC)
#define OP_TEQN (OP_RXN | F_TEQ | F_SCC)
#define OP_TSTN (OP_RXN | F_TST | F_SCC)

#define F_PSR       0x00400000L

/* #define regbit(n)   (1<<(n)) */

/* Support here for second version of floating point (FPE2).             */

/* options within LDF/STF CPDT group                                     */
/* ONE OF THESE MUST BE USED WITH ANY FLOATING POINT OPERATION.          */

#define F_SINGLE    0x00000100L
#define F_DOUBLE    0x00008100L
#define F_EXTENDED  0x00400100L
#define F_PACKED    0x00408100L
#define CPDT_FLTOFJ(x) (J_double(x) ? F_DOUBLE : F_SINGLE)

#define F_FM_1      0x00008200L
#define F_FM_2      0x00400200L
#define F_FM_3      0x00408200L
#define F_FM_4      0x00000200L

/* opcodes for CPDO group                                                */

#define CPDO_SINGLE 0x00000100L
#define CPDO_DOUBLE 0x00000180L
#define CPDO_FLTOFJ(x) (J_double(x) ? CPDO_DOUBLE : CPDO_SINGLE)

#define CPDO_RNDUP  0x00000020L  /* Without one of these it rounds to nearest */
#define CPDO_RNDDN  0x00000040L
#define CPDO_RNDZ   0x00000060L

#define F_REGOP     0x00000000L
#define F_CONSTOP   0x00000008L

#define F_ADF       0x00000000L
#define F_MUF       0x00100000L
#define F_SUF       0x00200000L
#define F_RSF       0x00300000L
#define F_DVF       0x00400000L
#define F_RDF       0x00500000L
#define F_POW       0x00600000L
#define F_RPW       0x00700000L
#define F_RMF       0x00800000L
#define F_FML       0x00900000L
#define F_FDV       0x00a00000L
#define F_FRD       0x00b00000L
#define F_POL       0x00c00000L
#define F_XX1       0x00d00000L
#define F_XX2       0x00e00000L
#define F_XX3       0x00f00000L

#define F_MVF       0x00008000L
#define F_MNF       0x00108000L
#define F_ABS       0x00208000L
#define F_RND       0x00308000L
#define F_SQT       0x00408000L
#define F_LOG       0x00508000L
#define F_LGN       0x00608000L
#define F_EXP       0x00708000L
#define F_SIN       0x00808000L
#define F_COS       0x00908000L
#define F_TAN       0x00a08000L
#define F_ASN       0x00b08000L
#define F_ACS       0x00c08000L
#define F_ATN       0x00d08000L
#define F_YY1       0x00e08000L
#define F_YY2       0x00f08000L

/* opcodes for more floating point operations                            */


#define F_FLT   0x00000110L
#define F_FIX   0x00100110L
#define F_WFS   0x00200110L
#define F_RFS   0x00300110L
#define F_WFC   0x00400110L
#define F_RFC   0x00500110L

#define F_CMF   0x0090f110L      /* Use for == tests */
#define F_CNF   0x00b0f110L
#define F_CMFE  0x00d0f110L      /* Use for >, >= etc tests */
#define F_CNFE  0x00f0f110L

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
#define R_V5    0x8L     /* register variable 5                          */
#define R_V6    0x9L     /* register variable 6                          */
#define R_SB    0x9L     /* (in reentrant APCS_3 code)                   */
#define R_FP  0xbL       /* Frame pointer                                */
/* #define R_IP 0xcL      * temp + used in call                          */
/* #define R_SP  0xdL     * main stack pointer                           */
#define R_SL  0xaL       /* stack limit (usually not checked)            */
/* #define R_LR 0xeL      * link address in function calls + workspace   */
#define R_PC    0xfL     /* program counter                              */
/* #define R_PSR R_PC     * program status register                      */

/* Calling a function can disturb r_a1 to r_a4, r_ip, r_lk but must      */
/* preserve r_v1 to r_v6, r_fp and r_sp. r_sl must always be a valid     */
/* stack limit value - it may be changed during a call is a stack        */
/* has its size changed.                                                 */

/* LDM/STM masks for real registers.                                     */
#define M_LR    regbit(R_LR)
#define M_PC    regbit(R_PC)
#define M_PSR   regbit(R_PSR)
#define M_ARGREGS       (regbit(R_A1+NARGREGS)-regbit(R_A1))
#define M_VARREGS       (regbit(R_V1+NVARREGS)-regbit(R_V1))
#define M_FARGREGS      (regbit(R_F0+NFLTARGREGS)-regbit(R_F0))
#define M_FVARREGS      (regbit(R_F0+NFLTVARREGS+NFLTARGREGS) - \
                         regbit(R_F0+NFLTARGREGS))

/* #define R_F0    0x10L   / * 16 added to avoid muddle with integer regs.   */
#define R_F1    0x11L
#define R_F2    0x12L
#define R_F3    0x13L
#define R_F4    0x14L
#define R_F5    0x15L
#define R_F6    0x16L
#define R_F7    0x17L

#endif

/* end of arm/ops.h */
