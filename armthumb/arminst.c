/*
 * armthumb/arminst.c
 * Copyright (C) Codemist Ltd., 1987-1993
 * Copyright (C) Advanced RISC Machines Limited., 1990-1997
 * SPDX-Licence-Identifier: Apache-2.0
 *
 * Functions to generate sequences of ARM instructions required by
 * both ARM and Thumb backends
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include "globals.h"
#include "armops.h"
#include "arminst.h"

#define ROR(x, n) (((x)<<(32-(n))) | ((((uint32)x)>>(n)) & ((1L<<(32-(n)))-1L)))

int32 Arm_EightBits(int32 n)
{
/* If n fits in an ARM immediate field this function returns a 12-bit    */
/* quantity suitable for use there. Otherwise it returns -1              */
    int32 shift;
    for (shift = 0; shift<0x1000; shift += 0x100)
    {   if ((n&0xffffff00)==0) return(shift|n);
        n = ROR(n, 30);
    }
    return(-1);
}

/* This is the ACN second attempt to produce sensible code to load       */
/* integer values into ARM registers.                                    */
/* /* There are too many different ways below to break up an integer into*/
/* 8-bit chunks.  Rationalisation needed.                                */

int32 Arm_SplitForAdd(RealRegister r, int32 n, int32 *op) {
    int32 tail = 0x3, mask = 0xff;
    while ((n & tail)==0) tail = tail << 2, mask = mask << 2;
    tail <<= 8;
    if (r != R_SP && (n & mask) != mask) {
        int32 bitsabove = (n & tail) == tail ? tail : 0;
        int32 mask1 = mask, tail1 = tail;
        for (; ; tail1 >>= 2, mask1 = ROR(mask1, 2)) {
            bitsabove >>= 2;
            if ((n & (tail1 >> 2)) != bitsabove) break;
        }
        if ((n & mask1 & 0xff000000) != 0)
            mask = mask1, tail = tail1;
    }
/*
 * If the destination register is the stack pointer I decide on using
 * an ADD or a SUB based on the sign of n.  The effect should be to ensure
 * that the stack pointer moves in a monotonic way, which is necessary if
 * code is to be secure against asynchronous interrupts.  For other
 * destination registers it will sometimes be possible to generate
 * tighter code by (e.g.) treating a + 0x1fffffff as a - 1 + 0x20000000.
 */
    if ((r == R_SP && n < 0) ||
        (r != R_SP && (n & tail) == tail)) {
      *op = OP_SUBN;
      return mask & -n;
    } else {
      *op = OP_ADDN;
      return mask & n;
    }
}

int32 *arm_add_integer(
    RealRegister r1, RealRegister r2, int32 n, int32 scc,
    RegisterContaining *cachelookup,
    int32 *v)
{ /* Generate code for r1 = r2 + n.                                      */
    int32 packed;

    if (n==0 && scc == 0) {
        if (r1 != r2)
            *v++ = (OP_MOVR | scc | F_RD(r1) | r2);
        return v;
    }
    packed = Arm_EightBits(n);
    if (packed >= 0)
    {   *v++ = (OP_ADDN | scc | F_RD(r1) | F_RN(r2) | packed);
        return v;
    }
    packed = Arm_EightBits(-n);
    if (packed >= 0)
    {   *v++ = (OP_SUBN | scc |  F_RD(r1) | F_RN(r2) | packed);
        return v;
    }
    if (cachelookup != NULL)
    {   ValueDesc vd;
        int rc = cachelookup(n, V_Negated+V_AnyShift, &vd);
        if (rc != 0) {
            int32 w = F_RD(r1) | F_RN(r2) | scc | vd.r | vd.op3.shift;
            if (rc == V_Negated)
                w |= OP_SUBR;
            else
                w |= OP_ADDR;
            *v++ = w;
            return v;
        }
    }
/*
 * here it will take at least two instructions...
 */
    {   int32 op;
        int32 n1 = Arm_SplitForAdd(r1, n, &op);
        if (op == OP_SUBN)
            n += n1;
        else
            n -= n1;
        *v++ = (op | F_RD(r1) | F_RN(r2) | Arm_EightBits(n1));
        return arm_add_integer(r1, r1, n, scc, cachelookup, v);
    }
}

/* End of armthumb/arminst.c */
