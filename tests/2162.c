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


#include <string.h>
#include "testutil.h"

/********************* 2162 ***********************/

/* may syserr */

unsigned short cnt;
unsigned short min;

void t0_2162(void)
{
    if(min>=cnt)
        cnt = 0;
    else
        cnt--;
}

int t1_2162(void)
{   int t;
    if(min>=cnt)
        cnt = 0, t = 1;
    else
        cnt--, t = 2;
    return t;
}

void t_2162(void)
{
    cnt = 3;
    min = 1;
    t0_2162();
    EQI(cnt, 2);
    EQI(t1_2162(), 2);
    EQI(cnt, 1);
}


int main(void)
{
    BeginTest();
    t_2162();
    EndTest();
    return 0;
}

