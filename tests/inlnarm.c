/*
 * ARM C compiler inline assembler test $RCSfile$
 * Copyright (C) 1997 Advanced Risc Machines Ltd. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include "testutil.h"
#include <stdlib.h>
#include <string.h>

int t0(int a, int b, int c)
{   /* Quoted assembler construct can only be used in C++ as a declaration,
       the assembler block can be used as a command.
     */
    __asm("ADD a, b, c ; ADD b, a, c");
    if (a)
        __asm { ADD a, b, c }
    else
        __asm { ADD a, a, 1 }
#ifdef __cplusplus
    asm(" ADD a, a, 1 ");
    asm { ADD a, a, 2 };
#endif
    return a + b + c;
}

int t1(int a, int b, int c)
{
    /* Data processing */
    __asm
    {
        MOV     r0, a
        MOV     r1, b
        MOV     r2, c
        /* none of the instructions below may be removed */
        AND     r0, r1, r2
        EOR     r1, r0, r2
        SUB     r2, r1, r0
        RSB     r0, r1, r2
        ADDS    r1, r0, r2
        ADCS    r2, r0, r1
        SBCS    r0, r1, r2
        RSC     r1, r0, r2
        TST     r0, r1
        TEQ     r0, r1
        CMP     r0, r1
        CMN     r0, r1
        ORR     r2, r0, r1
        MOV     r0, r1
        BIC     r1, r0, r2
        MVN     r0, r2
        /* until here */
        ADD     a, r0, r1, ROR r2
    }
    return a;
}

void t2(int a, int *b, int *c)
{
    /* Load/store instructions */
    __asm
    {
        LDR     a, [b]
        ADD     a, a, #1
        STR     a, [b]

        LDR     a, [b, #4]
        ADD     a, a, #1
        STR     a, [b, #4]

        LDRT    a, [c], #4      /* no preindex possible */
        ADD     a, a, #1
        STRBT   a, [c]

        LDRB    a, [b, #8]
        ADD     a, a, #1
        STRB    a, [b, #9]
#ifdef __TARGET_FEATURE_HALFWORD
        LDRSB   a, [b]
        STRH    a, [b, #12]

        LDRSH   a, [b]
        STR     a, [b, #16]
        LDRH    a, [b]
        STR     a, [b, #20]
#endif
        LDMIA   b!, {r0 - r2}
        ADD     b, b, #12
        STMIA   b!, {r0 - r2}

        MOV     a, #-1
        STR     a, [b]
        LDR     a, [b, #-9*4]
        SWP     c, a, [b]
        SWPB    a, c, [b]
    }
}

int t3(int a, int b)
{   /* multiply */
    int c, d;
    __asm
    {
        MUL     c, a, b
        MLA     c, b, a, c
#ifdef x__TARGET_FEATURE_MULTIPLY
        UMULL   a, d, a, b
        SMLAL   a, b, c, d
#else
        MOV     d, #0
#endif
    }
    return a + b + c + d;
}

void t4(int a, int b)
{   /* not runnable - parsing only */
    /* MRS/TEQP/condition codes */
    __asm
    {
        MRS     a, CPSR
        MRS     b, SPSR
        MSR     SPSR_all, b     /* equals SPSR_cf */
        MSR     CPSR_f, #15
        MSR     SPSR_ctl, b
        MSR     SPSR_sc, a
        MSR     CPSR_cx, b
        MSR     SPSR, a
        MSR     SPSR_fsxc, b
        /* only for 26 bit compatibility */
        TEQP    a, b
        CMPP    a, b
        TEQP    a, #1
        TSTP    a, #1 << 24
        CMPP    b, #1
        CMNP    a, #1

        CMP     a, b            /* mark PSR as live */
        ADDEQ   a, a, b
        ADDNE   a, a, b
        ADDCS   a, a, b ; ADDHS   a, a, b   /* synomyms */
        ADDCC   a, a, b ; ADDLO   a, a, b   /* synomyms */
        ADDMI   a, a, b
        ADDPL   a, a, b
        ADDVS   a, a, b
        ADDVC   a, a, b
        ADDHI   a, a, b
        ADDLS   a, a, b
        ADDGE   a, a, b
        ADDLT   a, a, b
        ADDGT   a, a, b
        ADDLE   a, a, b
        ADDAL   a, a, b ; ADD     a, a, b   /* synomyms */
        /* NOTE: NV not supported */

        ADDS    a, a, b
        SUBNE   b, b, a
        RSBEQ   a, a, b
#if defined(__TARGET_FEATURE_HALFWORD) && defined(__TARGET_FEATURE_MULTIPLY)
        MULVSS      a, a, b
        MLAMIS      a, b, b, a
        SMULLGTS    a, b, a, b
        UMLALNES    a, b, b, a
        LDRLEBT     a, [b]
        LDRVSSH     a, [b, #4]
#endif
        SWPALB      a, b, [a]

        NOP

        SWI     100
        BL      t3, {}, {}, {}
    }
}

void t5(char *a, char *b, int n)
{   /* labels & branches */
    int ch;
    char *end;
    __asm
    {
        ADD     end, a, n
    loop:
        LDRB    ch, [a], #1
        CMP     a, end
        STRB    ch, [b], #1
        CMPNE   ch, #0
        BNE     loop
    }
}

#define ROR(x,y) (((unsigned)(x)>>(y))|((x)<<(32-(y))))

void t6(int x, int y)
{   /* shifts */
    int a, b, c, d;
    a = x; b = y; c = x; d = y;
    __asm
    {
        MOV     a, a, LSL #0        /* no shift */
        MOV     b, b, ASL #2        /* synonym of LSL */
        MOV     c, c, LSL #31
    }
    EQI(a, x);
    EQI(b, y << 2);
    EQI(c, x << 31);
    a = x; b = y; c = x; d = y;
    __asm
    {
        MOV     a, a, LSR #0        /* no shift */
        MOV     b, b, LSR #1
        MOVS    c, c, LSR #32       /* preserve LSR #32 */
        ADDCS   c, c, #1
        MOV     d, d, LSR #32       /* might translate to MOV a, #0 */
    }
    EQI(a, x);
    EQI(b, (unsigned) y >> 1);
    EQI(c, x & (1U << 31) ? 1 : 0);
    EQI(d, 0);
    a = x; b = y; c = x; d = y;
    __asm
    {
        MOV     a, a, ASR #0        /* no shift */
        MOV     b, b, ASR #1
        MOVS    c, c, ASR #32       /* preserve ASR #32 */
        ADDCS   c, c, #2
        MOV     d, d, ASR #32       /* preserve ASR #32 */
    }
    EQI(a, x);
    EQI(b, y >> 1);
    EQI(c, (x < 0) ? -1+2 : 0);
    EQI(d, (y < 0) ? -1 : 0);
    a = x; b = y; c = x; d = y;
    __asm
    {
        MOV a, a, ROR #0            /* no shift */
        MOV b, b, ROR #1
        MOV c, c, ROR #31
        CMP d, #1                   /* set carry if (us)d >= 1 */
        MOV d, d, RRX
    }
    EQI(a, x);
    EQI(b, ROR(y,1));
    EQI(c, ROR(x,31));
    EQI(d, (d != 0 ? (1U << 31) : 0) + ((unsigned) y >> 1));
    a = x; b = y; c = x; d = y;
    __asm
    {
        MOV a, a, LSL a
        MOV b, b, LSR b
        MOV c, c, ASR c
        MOV d, d, ROR d
    }
    EQI(a, (x & 255 >= 32) ? 0 : x << x);
    EQI(b, (y & 255 >= 32) ? 0 : (unsigned) y >> y);
    EQI(c, (x & 255 >= 32) ? (x < 0 ? -1 : 0) : x >> x);
    EQI(d, ROR(y, y&31));
}

void test(void)
{
    int arr[10] = { 0x81818181, 0x22334455, 0xffffffff, 0, 0xffffffff, 0 };
    char str1[20] = "Inline assembler...";
    char str2[20] = "                   ";


#ifdef __cplusplus
    EQI(t0(1, 2, 3), 25);
#else
    EQI(t0(1, 2, 3), 22);
#endif
    EQI(t1(0x11223344, 0x55667788, 0x99aabbcc), 0xAC004458);
    EQI(t1(0xFFEEDDCC, 0xBBAA9988, 0x77665544), 0x8841EEBB);

    t2(0, arr, arr+1);
#ifndef __BIG_ENDIAN
    EQI(arr[0], 0x81818182);
    EQI(arr[1], 0x22334456);
    EQI(arr[2], 0xffff5857);
#ifdef __TARGET_FEATURE_HALFWORD
    EQI(arr[3], 0x0000ff82);
    EQI(arr[4], 0xffff8182);
    EQI(arr[5], 0x00008182);
#endif
    EQI(arr[6], arr[0]);
    EQI(arr[7], arr[1]);
    EQI(arr[8], arr[2]);
    EQI(arr[9], arr[0] | 0xff);
#endif
#ifdef x__TARGET_FEATURE_MULTIPLY
    EQI(t3(0x11223344, 0x55667788), 0x164B7F0D);
    EQI(t3(0xffeeddcc, 0xbbaa9988), 0x86ECEA9F);
#else
    EQI(t3(0xffeeddcc, 0xbbaa9988), 0xC972F814);
    EQI(t3(0x11223344, 0x55667788), 0x68B85B0C);
#endif
    /* t4 not runnable */
    t5(str1, str2, 16);
    EQI(0, strcmp(str2, "Inline assembler   "));

    t6(-1, -2);
    t6(100, 1000);
    t6(0,0);
    t6(0xFEDCBA98, 0x76543210);
    t6(0x76543210, 0xFEDCBA98);
}

/******************* test *************************/

typedef unsigned int uint;

unsigned char bitrev_tab[256];

uint bitrev_1(uint n)
{
    char *tab;
    uint res, t;
    __asm
    {
        MOV     tab, bitrev_tab
        LDRB    res, [tab, n, LSR #24]
        MOV         n, n, LSL #8
        LDRB    t, [tab, n, LSR #24]
        MOV         n, n, LSL #8
        ORR         res, res, t, LSL #8
        LDRB    t, [tab, n, LSR #24]
        MOV         n, n, LSL #8
        LDRB    n, [tab, n, LSR #24]
        ORR         res, res, t, LSL #16
        ORR         res, res, n, LSL #24
    }
    return res;
}

uint bitrev_2(uint n)
{
    uint c0, c1, c2, t;
    __asm
    {
        MOV     c2, #0x0F0F0F0F
        EOR     c1, c2, c2, LSL #2  // 0x33333333
        EOR     c0, c1, c1, LSL #1  // 0x55555555
        AND     t, c0, n
        AND     n, c0, n, LSR #1
        ORR     n, n, t, LSL #1
        AND     t, c1, n
        AND     n, c1, n, LSR #2
        ORR     n, n, t, LSL #2
        AND     t, c2, n
        AND     n, c2, n, LSR #4
        ORR     n, n, t, LSL #4
        MOV     t, n, LSR #8        // DCBA
        BIC     t, t, #255 << 8     // 0D0B
        EOR     n, t, n, ROR #8     // A0C0
        ORR     n, n, t, ROR #16    // ABCD
    }
    return n;
}

uint bitreverse(uint n)
{
    uint res = 0, i;
    for (i = 32; i > 0; i--)
    {
        res = (res << 1) | (n & 1);
        n >>= 1;
    }
    return res;
}

void bitreverse_test(void)
{
    int i;
    for (i = 0; i < 256; i++)
        bitrev_tab[i] = bitreverse(i) >> 24;

    for (i = 0; i < 1000; i++)
    {
        uint r = rand();
        uint r1 = bitrev_1(r);
        uint r2 = bitrev_2(r);
        uint r3 = bitreverse(r);
        EQI(r1, r3);
        EQI(r2, r3);
    }
}

/**************************************************/

__inline int mul48(int a, int b)
{
    int tmp, res;
    __asm
    {
        BIC     a, a, #255 << 24
        BIC     b, b, #255 << 24
        AND     tmp, b, #255
        MUL     res, a, tmp
        MOV     tmp, b, LSR #8
        AND     tmp, tmp, #255
        TST     res, #255
        MOV     res, res, LSR #8
        MLA     res, a, tmp, res
        MOV     tmp, b, LSR #16
        TSTEQ   res, #255
        MOV     res, res, LSR #8
        MLA     res, a, tmp, res    // 24 bit result in bit 8..31 of res, bit 7 = round bit
        ORRNE   res, res, #1        // bit 0..5 are the sticky bits, bit 6 = guard bit
    }
    return res;
}

int fmul(int a, int b)
{
    int mask, expa, expb, exp, tmp, res;
    __asm
    {
        MOV     mask, #255 << 16        // first check for denorms, infinites and NaNs
        ANDS    expa, mask, a, LSR #7
        ANDNES  expb, mask, b, LSR #7
        CMPNE   expa, #255 << 16
        CMPNE   expb, #255 << 16
        BEQ     fmul_uncommon
        TEQ     a, b
        ADDMI   exp, exp, #1 << 8       // insert result sign into exponent

        ORR     a, a, #1 << 23          // do the 24*24 -> 48 bit multiply
        ORR     b, b, #1 << 23
        ADD     exp, expa, expb
        SUB     exp, exp, #128 << 16    // subtract bias+1 - 0..253 normal (anti-interlock)

        MOV     res, mul48(a, b)

        CMP     exp, #252 << 16         // 0..251 can never overflow
        BHS     fmul_check_overflow
    fmul_round:
        MOVS    a, res, LSL #1          // CS -> high bit set
        MOVCC   res, res, LSL #1        // CC : shift 1 left - sets bit 23
        ADC     exp, exp, exp, LSR #16  // recombine sign and exponent
        MOVS    a, res, LSR #8          // CS -> round
        TSTCS   res, #127               // EQ -> round to even
        ADC     a, a, exp, LSL #23      // add fraction, and round
        BICEQ   a, a, #1                // round to even
    }
    return a;

    __asm
    {
    fmul_check_overflow:
        BMI     fmul_underflow          // was really underflow
        MOV     tmp, exp, LSR #16       // test whether there is overflow
        TST     res, #1 << 31
        ADDNE   tmp, tmp, #1
        CMP     tmp, #253
        CMPEQ   res, #0xFFFFFF80        // rounding overflow possible?
        BLO     fmul_round              // will not overflow after all
    fmul_overflow:                      // exception handling not yet implemented...
        AND     a, a, #1 << 31          // create signed infinite
        ORR     a, a, #255 << 23
    }
    return a;

    __asm
    {
    fmul_underflow:                     // underflow -> denormalise (not implemented)
        MOV     expa, #0
        MOV     res, #0
        B   fmul_round

    fmul_uncommon:                      // a or b denorm/NaN/inf
        AND     expb, mask, b, LSR #7
        CMP     expa, #255 << 16
        CMPNE   expb, #255 << 16
        BEQ     fmul_uncommon1          // a or b NaN/inf
        // denorms not implemented - act as if it was a zero

        EOR     a, a, b
        AND     a, a, #1 << 31
    }
    return a;

    __asm
    {
    fmul_uncommon1:
        CMP expb, #255 << 16
        MOVEQ   a, b
    }
    return a;
}


typedef union
{   int i;
    float f;
}   intfloat;


void fmul_test(void)
{
    intfloat a, b, c, d;
    int i;

    a.f = 1.00001;
    b.f = 0.99999;

    c = a;
    for (i = 0; i < 10000; i++)
        c.i = fmul(c.i, b.i);
    d = a;
    for (i = 0; i < 10000; i++)
        d.f = d.f * b.f;
    EQI(1, c.i == d.i);     /* results should be bit identical */
}


/******************* fail *************************/

/* ldm peepholer should not combine assembler LDRs? */

void f_0(void)
{
    int arr[2] = { 0x11223344, 0 };
    char *p = (char*)(&arr[0]) + 2;
    int t0, t1;
    __asm
    {
        LDR  r0, [p, #0]    /* unaligned access - rotate */
        LDR  r1, [p, #4]
        EOR  t0, r0, r1
        LDMIA p, {r0, r1}
        EOR  t1, r0, r1
    }
    EQI(t0, 0x33441122);
    EQI(t1, 0x11223344);
}

/* register allocator does not handle dead physical
   registers correctly - p is allocated to r1...
 */

void f_1(void)
{
    int arr[4] = { 1, 2 };
    int *p = arr;
    __asm
    {
        LDMIA   p!, {r0 - r1}
        STMDB   p, {r0}
    }
    EQI(arr[0], 1);
    EQI(arr[1], 1);
}

/**************************************************/

int main ()
{
  BeginTest();
  test();
  bitreverse_test();
  fmul_test();
  f_0();
  f_1();
  EndTest();
  return 0;
}


