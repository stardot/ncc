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

__int64 ll_2672;
double d_2672;
float f_2672;

t_2672()
{
    f_2672 = ll_2672;
    d_2672 = ll_2672;  /* convert long long to a double */
}

/*********************  ***********************/


int main(void)
{
    BeginTest();
    EQI(0,0);
    EndTest();
    return 0;
}
