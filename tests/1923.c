/* ARM C library test */
/* Copyright (C) Advanced RISC Machines, 1997. All Rights Reserved */
/* SPDX-Licence-Identifier: Apache-2.0 */

/* THIS TEST HAS BEEN SUPERCEDED BY 2581.c */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <stdlib.h>
#include <limits.h>
#include "testutil.h"

double dd(double x)
{
  /* return a number bigger than the argument */
  return x*2;
}

float df(float x)
{
  /* return a number bigger than the argument */
  return x*2;
}

int main(void)
{
  double d, f;
  long l;
  unsigned long u;

  /* disable the IVO exception */
  __fp_status(__fpsr_IOE, 0);

  BeginTest();

  /* double -> long  [overflow] */
  d = (double)INT_MAX;
  l = (long)d;                  /* does not overflow */
  EQI(l, INT_MAX);              /* should return INT_MAX */

  d = dd(d);
  l = (long)d;                  /* overflows */
  EQI(l, INT_MAX);              /* should return INT_MAX */

  d = dd(d);
  l = (long)d;                  /* overflows */
  EQI(l, INT_MAX);              /* should return INT_MAX */

  /* double -> unsigned long  [-ve] */
  d = (double)-1;
  u = (unsigned long)d;         /* should fail - invalid */
  EQI(u, 0);

  /* double -> unsigned long  [overflow] */
  d = (double)UINT_MAX;
  u = (unsigned long)d;         /* should not overflow */
  EQI(u, UINT_MAX);

  d = dd(d);
  u = (unsigned long)d;         /* overflows */
  EQI(u, UINT_MAX);

  d = dd(d);
  u = (unsigned long)d;         /* overflows */
  EQI(u, UINT_MAX);

  /* float -> long [overflow] */
  f = (float)INT_MAX;
  l = (long)f;                  /* overflows */
  EQI(l, INT_MAX);              /* should return INT_MAX */

  f = df(f);
  l = (long)f;                  /* overflows */
  EQI(l, INT_MAX);              /* should return INT_MAX */

  f = df(f);
  l = (long)f;                  /* overflows */
  EQI(l, INT_MAX);              /* should return INT_MAX */

  /* float -> unsigned long  [-ve] */
  f = (float)-1;
  u = (unsigned long)f;         /* should fail - invalid */
  EQI(u, 0);

  /* float -> unsigned long  [overflow] */
  f = (float)UINT_MAX;
  u = (unsigned long)f;         /* should not overflow */
  EQI(u, UINT_MAX);        /*  /* Value is unlikely to be UINT_MAX: floats */
                                /* don't have sufficient precision */

  f = df(f);
  u = (unsigned long)f;         /* overflows */
  EQI(u, UINT_MAX);

  f = df(f);
  u = (unsigned long)f;         /* overflows */
  EQI(u, UINT_MAX);


  EndTest();
  return 0;
}
