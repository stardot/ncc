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

/********************* 2256 ***********************/

/* must be compiled with -zpq8 */

int t0_2256(int argc, char **argv)
{   return argc;    }

int t1_2256(int argc, char **argv)
{
   int count, retval;
   if (argc < 3)
      return argc;
   for (count = 0; count > 0; count--)
      retval = t0_2256 (argc, argv);
   return retval;
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
