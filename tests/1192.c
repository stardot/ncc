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

#include <stdio.h>
#include "testutil.h"

#define BUFFER_SECS 2
#define SAMPLERATE  500
#define DATAMUX     12
#define BUFFER_SAMPLES ( BUFFER_SECS* SAMPLERATE* DATAMUX)

#define FDS1 8      /* forward shift #1 (anticipate) @ 250 sps   */
#define FDS2 16     /* forward shift #2 (anticipate) @ 250 sps   */
#define BDS1 8      /* backward shift #1 (delay)     @ 250 sps   */

#define FWDEL1  (DATAMUX*FDS1*(SAMPLERATE/250))
#define FWDEL2  (DATAMUX*FDS2*(SAMPLERATE/250))
#define BKDEL1  (DATAMUX*BDS1*(SAMPLERATE/250))


short DataBuf[BUFFER_SAMPLES];
short FilterSum( short *pdata, short bndflt[]);

short FilterSum( short *pdata, short bndflt[]) {
   unsigned short leads, ix;
   short fil_sig, fil_sig_part1, fil_sig_part2,sumdtr;

   leads = 0x72;            /* set bits for the 4 channels to use  */
   ix = 0;
   sumdtr = 0;

/* NOTE: adding any function call in this function will eliminate the
 * problem
 */

   for (; leads != 0; leads = leads << 1) {

      if (leads & 0x80) {
         fil_sig_part1 = *pdata;
         fil_sig = fil_sig_part1 << 1;

         fil_sig = -fil_sig;
         fil_sig_part2 = *(pdata + FWDEL1);
         fil_sig += fil_sig_part2;
         fil_sig += *(pdata - BKDEL1);
         fil_sig += bndflt[ ix];
         fil_sig_part2 *= 2;
         fil_sig_part1 -= fil_sig_part2;
         fil_sig_part1 += *(pdata + FWDEL2);
         fil_sig_part1 += bndflt[ ix+4];
         bndflt[ ix+4] = fil_sig_part1;
         bndflt[ ix++] = fil_sig;
         fil_sig_part1 -= fil_sig;
         if (fil_sig_part1 < 0)
            fil_sig_part1 = -fil_sig_part1;
         fil_sig_part1 = fil_sig_part1 >> 2;
         sumdtr += fil_sig_part1;
      }
      pdata++;
   }
   sumdtr = sumdtr >> 1;

   return( sumdtr);
}

int main(int argc, char *argv[] ) {
    short sum,bndfltr[8];
    int i;

    BeginTest();
/*  fill data buffer with dummy data */
    for ( i = 0; i < BUFFER_SAMPLES; i++ )
        DataBuf[i] = (short)((i*17)&0xfff);

    for ( i = 0; i < 8; i++ )
        bndfltr[i] = 0;

    sum = FilterSum( &DataBuf[BUFFER_SAMPLES/2], bndfltr);

    EQI(sum, 2048);
    EQI(bndfltr[0], -4096);
    EQI(bndfltr[1], -4096);
    EQI(bndfltr[2], -4096);
    EQI(bndfltr[3], -4096);
    EQI(bndfltr[4], 0);
    EQI(bndfltr[5], 0);
    EQI(bndfltr[6], 0);
    EQI(bndfltr[7], 0);
    EndTest();
    return(0);
}
