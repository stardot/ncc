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

/********************* 2099 ***********************/

void GLPen_TPtoCA (float x1, float y1, float x2, float y2, float x3, float y3,
                   float *xc, float *yc, float *sweep)
{
    float a1, b1, c1, a2, b2, c2, xx1, yy1, denom;

    xx1 = x1*x1;
    yy1 = y1*y1;
    a1 = 2.0F * (x2 - x1);
    b1 = 2.0F * (y2 - y1);
    c1 = xx1 - x2*x2 + yy1 - y2*y2;
    a2 = 2.0F * (x3 - x1);
    b2 = 2.0F * (y3 - y1);
    c2 = xx1 - x3*x3 + yy1 - y3*y3;
    denom = (a1*b2 - a2*b1);
    *yc = (a2*c1 - a1*c2) / denom;
    *xc = (b1*c2 - b2*c1) / denom;
    return;
}

/* no executable test */

/*********************  ***********************/


int main(void)
{
    BeginTest();
    EQI(0,0);
    EndTest();
    return 0;
}
