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

/*********************  795 ***********************/

void t_795() {
  /* Actually, if the fault in thread 795 is present, compiling
     this will cause the compiler to crash.
   */
  char b[4];
  strcpy(b, "");
  EQI(b[0], 0);
}

/*********************  ***********************/


int main(void)
{
    BeginTest();
    t_795();
    EndTest();
    return 0;
}
