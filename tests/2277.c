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

/********************* 2277 ***********************/

/* may syserr */

__inline double f1_2277(double x) { return x + 1; }

void g1_2277() { f1_2277(1); }
double h1_2277() { return f1_2277(1); }

__inline float f2_2277(float x) { return x + 1; }

void g2_2277() { f2_2277(1); }
float h2_2277() { return f2_2277(1); }

void t_2277(void)
{
    EQD(h1_2277(), 2.0);
    EQD(h2_2277(), 2.0);
}

/*********************  ***********************/


int main(void)
{
    BeginTest();
    t_2277();
    EndTest();
    return 0;
}
