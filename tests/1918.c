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

/********************* 1918 ***********************/

  const int t0_1918[2];
  const int t0_1918[] = { 1,2 };

  /* no executable test */

/*********************  ***********************/


int main(void)
{
    BeginTest();
    EQI(0,0);
    EndTest();
    return 0;
}
