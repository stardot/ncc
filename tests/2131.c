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

/********************* 2131 ***********************/

/* may syserr - must be compiled with -g -apcs /reent */

unsigned char const arr[2] [1] = { {1}, {1} };

#define m(s) { int *r; r = (int *)(s); }

int f_2131 ;
int *p_2131;

void t_2131()
{  int i ;

   switch (i) {
      case 1 :
         if ( f_2131 == 1 )
             f_2131 =  1 ;
         else
             f_2131 = -1 ;
         break ;
      case 2 :
         m(p_2131[1]);
         t_2131();
         f_2131 = 1 ;
         f_2131 = 1 ^ f_2131 ;
         break ;
      case 3 :
      case 4 :
         m(p_2131[1]);
         f_2131 = 3 ; f_2131 = 3 ; f_2131 = 3 ; f_2131 = 3 ;
         f_2131 = 1 ;
         f_2131 = 1 ^ f_2131 ;
         break;
      case 5 :
         m(arr[1]);
         break;
   }
}

/* no executable test */


int main(void)
{
    BeginTest();
    t_2131();
    EndTest();
    return 0;
}

