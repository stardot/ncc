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

/********************* 2232 ***********************/

/* may syserr */

__inline float f_2232() { return 0; }

float g_2232() { return f_2232(); }

void t_2232()
{
    EQD(g_2232(), 0.0);
}

/*********************  ***********************/


int main(void)
{
    BeginTest();
    t_2232();
    EndTest();
    return 0;
}
