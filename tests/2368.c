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

/********************* 2368 ***********************/

/* no executable test needed */

#include <math.h>

double t1_2368(double a)
{
    return a;
}

void t_2368(void)
{
    double a, b, c;
    a = t1_2368(1.1);
    b = t1_2368(1.2);
    c = t1_2368(a / b);
}

/*********************  ***********************/


int main(void)
{
    BeginTest();

    EndTest();
    return 0;
}
