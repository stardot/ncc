/*
 * armthumb/arminst.h
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

#include "cgdefs.h"

int32 Arm_EightBits(int32 n);

int32 Arm_SplitForAdd(RealRegister r, int32 n, int32 *opp);

#define V_Exact    0x001
#define V_Negated  0x002
#define V_Inverted 0x004
#define V_Orred    0x008
#define V_KAdded   0x010
#define V_RegPair  0x020
#define V_ASR      0x040
#define V_LSR      0x080
#define V_LSL      0x100
#define V_ROR      0x200
#define V_AnyShift (V_ASR|V_LSR|V_LSL|V_ROR)

typedef union {
    int32 shift;  /* The decription of a constant shift, suitable    */
                  /* for orring into an ARM instruction              */
    struct {
        int32 op; /* J_ADDK or J_SUBK                                */
        int32 k;  /* a representation of the constant, suitable for  */
                  /* orring into an ARM instruction                  */
    } add;
    struct {
        int32 op;
        int32 rm;
    } rpair;
} ValueDescOp3;

typedef struct { RealRegister r; ValueDescOp3 op3; } ValueDesc;

typedef int RegisterContaining(uint32 n, int flags, ValueDesc *vp);

int32 *arm_add_integer(
     RealRegister r1, RealRegister r2, int32 n, int32 scc,
     RegisterContaining *cachelookup,
     int32 *v);
/* Generates instructions into the buffer v to add the constant n    */
/* to the contents of r2, placing the result in r1. The condition    */
/* field of the generated instructions is 0; scc is orred into the   */
/* last instruction generated (expected to be the S bit or 0). The   */
/* return value is a pointer to the next free word in v.             */
/* If cachelookup is non-null, it is used to determine whether the   */
/* addition can be performed via a RR-instruction with a register    */
/* whose value is known                                              */

/* end of armthumb/arminst.h */
