/*
 * Thumb C compiler inline assembler test $RCSfile$
 * Copyright (C) 1997 Advanced Risc Machines Ltd. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include "testutil.h"

#ifdef __thumb

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
        SUB     r0, r2, r1      /* no RSB in Thumb */
        ADD     r1, r0, r2
        ADD     r2, r0, r1      /* ADC / SBC UNSAFE */
        SUB     r0, r1, r2
        SUB     r1, r2, r0      /* no RSC in Thumb */
        TST     r0, r1
                                /* no TEQ in Thumb */
        CMP     r0, r1
        CMN     r0, r1
        ORR     r2, r0, r1
        MOV     r0, r1
        BIC     r1, r0, r2
        MVN     r0, r2
        /* until here */
        ROR     r1, r2
        ADD     a, r0, r1
    }
    return a;
}

int t2(int a, int b, int c)
{   int d;
    /* Data processing */
    __asm
    {   /* each binary op comes in 4 forms */
        ADD     a, #1
        ADD     a, b
        ADD     b, a, #1
        ADD     c, a, c

        SUB     a, #1
        SUB     a, b
        SUB     b, a, #1
        SUB     c, a, c

        ADC     a, b
        ADC     a, #1
        ADC     b, a, #1
        ADC     c, a, c

        SBC     a, b
        SBC     a, #1
        SBC     b, a, #1
        SBC     c, a, c

        AND     a, #1
        AND     a, b
        AND     b, a, #1
        AND     c, a, c

        BIC     a, #1
        BIC     a, b
        BIC     b, a, #1
        BIC     c, a, c

        ORR     a, #1
        ORR     a, b
        ORR     b, a, #1
        ORR     c, a, c

        EOR     a, #1
        EOR     a, b
        EOR     b, a, #1
        EOR     c, a, c

        ASR     a, #1
        ASR     a, b
        ASR     b, a, #1
        ASR     c, a, c

        LSR     a, #1
        LSR     a, b
        LSR     b, a, #1
        LSR     c, a, c

        LSL     a, #1
        LSL     a, b
        LSL     b, a, #1
        LSL     c, a, c
        ASL     a, #3       /* ASL is synonym for LSL */

        ROR     a, #1
        ROR     a, b
        ROR     b, a, #1
        ROR     c, a, c

        MUL     a, #5
        MUL     a, b
        MUL     b, a, #5
        MUL     c, a, c

        /* each unary op comes in 2 forms */

        MOV     a, b
        MOV     d, #1
        ADC     c, d

        MVN     a, a
        MVN     d, #1
        ADC     c, d

        NEG     a, a
        NEG     d, #1
        ADC     c, d

        /* compares come in 2 forms */

        CMP     a, b
        ADC     a, b
        CMP     a, #1

        TST     a, b
        TST     a, #1

        CMN     a, b
        CMN     a, #1

        NOP         /* translates to MOV r8, r8 - doesn't set condition codes */
    }
    return a + b + c + d;
}

void t3(int a, int *b, int *c)
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

        LDRB    a, [b, #8]
        ADD     a, a, #1
        STRB    a, [b, #9]

        LDSB    a, [b]
        STRH    a, [b, #12]

        LDSH    a, [b]
        STR     a, [b, #16]
        LDRH    a, [b]
        STR     a, [b, #20]

        LDMIA   b!, {r0 - r2}   /* r0-r5 BROKEN-regalloc */
        ADD     b, #4*3
        STMIA   b!, {r0 - r2}
    }
}


void t4(char *a, char *b, int n)
{   /* labels & branches */
    int ch;
    char *end;
    __asm
    {
        ADD     end, a, n
    loop:
        LDRB    ch, [a]
        ADD     a, #1
        STRB    ch, [b]
        CMP     a, end
        BEQ     ret
        ADD     b, #1
        CMP     ch, #0
        BNE     loop
    ret:
    }
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
    EQI(t1(0x11223344, 0x55667788, 0x99aabbcc), 0xAC004453);
    EQI(t1(0xFFEEDDCC, 0xBBAA9988, 0x77665544), 0x8841EEBB);

    t3(0, arr, arr);
#ifndef __BIG_ENDIAN
    EQI(arr[0], 0x81818182);
    EQI(arr[1], 0x22334456);
    EQI(arr[2], 0xffff00ff);
    EQI(arr[3], 0x0000ff82);
    EQI(arr[4], 0xffff8182);
    EQI(arr[5], 0x00008182);
    EQI(arr[6], arr[0]);
    EQI(arr[7], arr[1]);
    EQI(arr[8], arr[2]);
#endif
    t4(str1, str2, 16);
    EQI(0, strcmp(str2, "Inline assembler   "));

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
        LSR     t, n, #24
        LDRB    res, [tab, t]
        LSL     n, #8
        LSR     t, n, #24
        LDRB    t, [tab, t]
        LSL     t, #8
        ORR     res, t
        LSL     n, #8
        LSR     t, n, #24
        LDRB    t, [tab, t]
        LSL     t, #16
        ORR     res, t
        LSL     n, #8
        LSR     t, n, #24
        LDRB    t, [tab, t]
        LSL     t, #24
        ORR     res, t
    }
    return res;
}

uint bitrev_2(uint n)
{
    uint c0, c1, c2, t, sh;
    __asm
    {
        MOV     c2, #0x0F0F0F0F
        LSL     sh, c2, #2
        EOR     c1, c2, sh          // 0x33333333
        LSL     sh, c1, #1
        EOR     c0, c1, sh          // 0x55555555
        AND     t, c0, n
        LSR     sh, n, #1
        AND     n, c0, sh
        LSL     sh, t, #1
        ORR     n, n, sh
        AND     t, c1, n
        LSR     sh, n, #2
        AND     n, c1, sh
        LSL     sh, t, #2
        ORR     n, n, sh
        AND     t, c2, n
        LSR     sh, n, #4
        AND     n, c2, sh
        LSL     sh, t, #4
        ORR     n, n, sh
        LSR     sh, n, #8
        MOV     t, sh           // DCBA
        BIC     t, t, #255 << 8 // 0D0B
        ROR     sh, n, #8
        EOR     n, t, sh        // A0C0
        ROR     sh, t, #16
        ORR     n, n, sh        // ABCD
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


/******************* fail *************************/

/**************************************************/

int main ()
{
  BeginTest();
  test();
  bitreverse_test();
  EndTest();
  return 0;
}

#else

int main ()
{
  BeginTest();
  EndTest();
  return 0;
}

#endif
