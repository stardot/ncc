/*
 * ARM C compiler regression test $RCSfile$
 * Copyright (C) 1995 Advanced Risc Machines Ltd. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include "testutil.h"

/********************* 1929 ***********************/

int __global_reg(1) x0;
int __global_reg(2) x1;
int __global_reg(3) x2;
int __global_reg(4) x3;
int __global_reg(5) x4;
int __global_reg(6) x5;


typedef struct
{   int a[1000];
    signed char b;
} S_1929;

int f_1929(S_1929 *p, int a, int b, int c)
{
    int corrupted;
    corrupted = p->a[0];
    a += p->b;
    return corrupted + a + b + c;
}

void t_1929(void)
{
    S_1929 s;
    s.b = -4;
    s.a[0] = 98;
    EQI(f_1929(&s, 1, 2, 3), 100);
}

/********************* main ***********************/

int main(void)
{
    BeginTest();
    t_1929();
    EndTest();
}