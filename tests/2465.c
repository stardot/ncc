/*
 * ARM C compiler regression test $RCSfile$
 * Copyright (C) 1997 Advanced Risc Machines Ltd. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */


#include "testutil.h"

/********************* 2465 ***********************/

/* no executable test needed */

typedef void (P_2465)(int a, int b);

int f_2465(int a)
{
    extern P_2465 p_2465;
    return a;
}

typedef int A_2465[];
void g_2465() { extern A_2465 a; }

/*********************  ***********************/


int main(void)
{
    BeginTest();
    EQI(0,0);
    EndTest();
    return 0;
}
