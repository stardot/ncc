/* Test for what sqrt() should return */
/* Copyright (C) 1997, Advanced RISC Machines, All Rights Reserved. */
/* SPDX-Licence-Identifier: Apache-2.0 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include "testutil.h"
#include "mathtest.h"

#include "mathtest.c"

#define EQDI(x, y) (EQI((x).i.se_hi, (y).i.se_hi), EQI((x).i.lo, (y).i.lo))

void t2597_zero(void)
{
  double_ints d, r;

  get_errno();                  /* reset errno */

  d.f = 0.0;

  r.f = sqrt(d.f);
  EQDI(r, d);
  EQI(get_errno(), 0);

  dneg(&d);

  r.f = sqrt(d.f);
  EQDI(r, d);
  EQI(get_errno(), 0);
}

void t2597_one(void)
{
  double_ints d, r;

  get_errno();                  /* reset errno */

  dset_to_one(&d);

  r.f = sqrt(d.f);
  EQDI(r, d);
  EQI(get_errno(), 0);

  dneg(&d);

  r.f = sqrt(d.f);
  d.f = -HUGE_VAL;
  EQDI(r, d);
  EQI(get_errno(), EDOM);
}

void t2597_inf(void)
{
  double_ints d, r;

  get_errno();                  /* reset errno */

  dset_to_inf(&d);

  r.f = sqrt(d.f);
  EQDI(r, d);
  EQI(get_errno(), 0);

  dneg(&d);
  r.f = sqrt(d.f);
  d.f = -HUGE_VAL;
  EQDI(r, d);
  EQI(get_errno(), EDOM);
}

void t2597_min(void)
{
  double_ints d, r;

  get_errno();                  /* reset errno */

  d.f = 1.4916681482400413E-154*1.4916681482400413E-154;

  r.f = sqrt(d.f);
  d.f = 1.4916681482400413E-154;
  EQDI(r, d);
  EQI(get_errno(), 0);

  dneg(&d);
  r.f = sqrt(d.f);
  d.f = -HUGE_VAL;
  EQDI(r, d);
  EQI(get_errno(), EDOM);
}

void t2597_max(void)
{
  double_ints d, r;

  get_errno();                  /* reset errno */

  d.f = 1.3407807929942596E154 * 1.3407807929942596E154;

  r.f = sqrt(d.f);
  d.f = 1.3407807929942596E154;
  EQDI(r, d);
  EQI(get_errno(), 0);

  dneg(&d);
  r.f = sqrt(d.f);
  d.f = -HUGE_VAL;
  EQDI(r, d);
  EQI(get_errno(), EDOM);
}

void t2597_nan(void)
{
  double_ints d;

  get_errno();

  dset_to_qnan(&d);

  d.f = sqrt(d.f);
  EQU(disnan(&d), TRUE);
  EQI(get_errno(), 0);
}

void t2597_104(void)
{
  double_ints d, r;

  get_errno();                  /* reset errno */

  d.f = 10.4;

  r.f = sqrt(d.f);
  d.f = 3.2249030993194200967;
  EQDI(r, d);
  EQI(get_errno(), 0);

  dneg(&d);
  r.f = sqrt(d.f);
  d.f = -HUGE_VAL;
  EQDI(r, d);
  EQI(get_errno(), EDOM);
}

void t2597_106(void)
{
  double_ints d, r;

  get_errno();                  /* reset errno */

  d.f = 10.6;

  r.f = sqrt(d.f);
  d.f = 3.2557641192199411329;
  EQDI(r, d);
  EQI(get_errno(), 0);

  dneg(&d);
  r.f = sqrt(d.f);
  d.f = -HUGE_VAL;
  EQDI(r, d);
  EQI(get_errno(), EDOM);
}


void t2597_105(void)
{
  double_ints d, r;

  get_errno();                  /* reset errno */

  d.f = 10.5;

  r.f = sqrt(d.f);
  d.f = 3.24037034920393;
  EQDI(r, d);
  EQI(get_errno(), 0);

  dneg(&d);
  r.f = sqrt(d.f);
  d.f = -HUGE_VAL;
  EQDI(r, d);
  EQI(get_errno(), EDOM);
}

void t2597_110(void)
{
  double_ints d, r;

  get_errno();                  /* reset errno */

  d.f = 11.0;

  r.f = sqrt(d.f);
  d.f = 3.3166247903554;
  EQDI(r, d);
  EQI(get_errno(), 0);

  dneg(&d);
  r.f = sqrt(d.f);
  d.f = -HUGE_VAL;
  EQDI(r, d);
  EQI(get_errno(), EDOM);
}

void t2597_1000(void)
{
  double_ints d, r;

  get_errno();                  /* reset errno */

  d.f = 100.0;

  r.f = sqrt(d.f);
  d.f = 10.0;
  EQDI(r, d);
  EQI(get_errno(), 0);

  dneg(&d);
  r.f = sqrt(d.f);
  d.f = -HUGE_VAL;
  EQDI(r, d);
  EQI(get_errno(), EDOM);
}

int main(void)
{
  /* disable the IVO exception */
  __fp_status(__fpsr_IOE+__fpsr_IOC, 0);

  get_errno();                  /* reset errno */

  BeginTest();

  t2597_zero();
  t2597_one();
  t2597_min();
  t2597_max();
  t2597_inf();
  t2597_nan();
  t2597_104();
  t2597_106();
  t2597_105();
  t2597_110();
  t2597_1000();

  EndTest();
  return 0;
}

