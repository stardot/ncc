/* ARM C library test $RCSfile$. */
/* (Fault this tests for is /softfp only) */
/* Copyright (C) Advanced RISC Machines, 1997. All Rights Reserved */
/* SPDX-Licence-Identifier: Apache-2.0 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <stdio.h>
#include "testutil.h"
#include "mathtest.h"

#include "mathtest.c"

static int count;
#define do_something (count++)

/* double-precision tests */

static void t2483a(void)
{
  /* Test (QNaN == QNaN) -> false */
  double_ints x;

  dset_to_qnan(&x);

  if (x.f == x.f)
    EQI(1, 0);
  return;
}

static void t2483b(void)
{
  /* Test (QNaN != QNaN) -> true */
  double_ints x;

  dset_to_qnan(&x);

  if (x.f != x.f)
    do_something;
  else
    EQI(1, 0);
  return;
}

static void t2483c(void)
{
  /* Test (Inf == Inf) -> true */
  double_ints x;

  dset_to_inf(&x);

  if (x.f == x.f)
    do_something;
  else
    EQI(1, 0);
  return;
}

static void t2483d(void)
{
  /* Test (Inf != Inf) -> false */
  double_ints x;

  dset_to_inf(&x);

  if (x.f != x.f)
    EQI(1, 0);
  return;
}

static void t2483e(void)
{
  /* Test (1.0 == 1.0) -> true */
  double_ints x;

  dset_to_one(&x);

  if (x.f == x.f)
    do_something;
  else
    EQI(1, 0);
  return;
}

static void t2483f(void)
{
  /* Test (1.0 != 1.0) -> false */
  double_ints x;

  dset_to_one(&x);

  if (x.f != x.f)
    EQI(1, 0);
  return;
}

/* single-precision tests */

static void t2483g(void)
{
  /* Test (QNaN == QNaN) -> false */
  float_int x;

  fset_to_qnan(&x);

  if (x.f == x.f)
    EQI(1, 0);
}

static void t2483h(void)
{
  /* Test (QNaN != QNaN) -> true */
  float_int x;

  fset_to_qnan(&x);

  if (x.f != x.f)
    do_something;
  else
    EQI(1, 0);
}

static void t2483i(void)
{
  /* Test (Inf == Inf) -> true */
  float_int x;

  fset_to_inf(&x);

  if (x.f == x.f)
    do_something;
  else
    EQI(1, 0);
}

static void t2483j(void)
{
  /* Test (Inf != Inf) -> false */
  float_int x;

  fset_to_inf(&x);

  if (x.f != x.f)
    EQI(1, 0);
}

static void t2483k(void)
{
  /* Test (1.0 == 1.0) -> true */
  float_int x;

  fset_to_one(&x);

  if (x.f == x.f)
    do_something;
  else
    EQI(1,0);
}

static void t2483l(void)
{
  /* Test (1.0 != 1.0) -> false */
  float_int x;

  fset_to_one(&x);

  if (x.f != x.f)
    EQI(1,0);
}

int main(void)
{
  BeginTest();
  t2483a();                     /* these two are the actual bug */
  t2483b();                     /*   "    "   "   "    "     "  */
  t2483c();                     /* these are just misc tests to check */
  t2483d();                     /* the fix doesn't break anything */
  t2483e();
  t2483f();

  t2483g();
  t2483h();
  t2483i();
  t2483j();
  t2483k();
  t2483l();
  EndTest();

  return 0;
}
