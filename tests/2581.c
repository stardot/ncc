/* ARM C library test */
/* Copyright (C) Advanced RISC Machines, 1997. All Rights Reserved */
/* SPDX-Licence-Identifier: Apache-2.0 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#define LONG_LONG

#include <stdlib.h>
#include <limits.h>
#include "testutil.h"
#include "mathtest.h"

#include "mathtest.c"

/* double precision tests */

static void t2581_dnan(void)
{
  double_ints d;
  long l;
  unsigned long u;

  ivo();                        /* clear IVO flag */

  dset_to_qnan(&d);
  l = (long)d.f;
  EQI(l, 0);
  EQI(ivo(), TRUE);

  u = (unsigned long)d.f;
  EQU(u, 0);
  EQU(ivo(), TRUE);

  dneg(&d);
  l = (long)d.f;
  EQI(l, 0);
  EQU(ivo(), TRUE);

  u = (unsigned long)d.f;
  EQU(u, 0);
  EQU(ivo(), TRUE);
}

static void t2581_dnan_ll(void)
{
  double_ints d;
#ifdef LONG_LONG
  long long ll;
  unsigned long long ull;

  ivo();                        /* clear IVO flag */

  dset_to_qnan(&d);
  ll = (long long)d.f;
  EQLL(ll, 0);
  EQU(ivo(), TRUE);

  ull = (unsigned long long)d.f;
  EQUU(ull, 0);
  EQU(ivo(), TRUE);

  dneg(&d);
  ll = (long long)d.f;
  EQLL(ll, 0);
  EQU(ivo(), TRUE);

  ull = (unsigned long long)d.f;
  EQUU(ull, 0);
  EQU(ivo(), TRUE);
#endif
}

#ifdef LONG_LONG
#define LL_MAX 0x7fffffffffffffffLL
#define ULL_MAX 0xffffffffffffffffULL
#define LL_MIN (~LL_MAX)
#define ULL_MIN 0
#endif

static void t2581_dsigned(void)
{
  double_ints d;
  long l;
  ivo();                        /* clear IVO flag */

                                /* long */
                                /* double -> long  [overflow] */
  d.f = (double)(INT_MAX/2+1);
  l = (long)d.f;                /* does not overflow */
  EQI(l, INT_MAX/2 + 1);        /* should return INT_MAX/2 + 1 */
  EQU(ivo(), FALSE);

  d.f *= 2.0;                   /* should send overflowing */
  l = (long)d.f;
  EQI(l, INT_MAX);              /* overflowed - returns INT_MAX */
  EQU(ivo(), TRUE);

  d.f = (double)INT_MAX;        /* exact */
  l = (long)d.f;
  EQI(l, INT_MAX);
  EQU(ivo(), FALSE);

  dsucc(&d);
  l = (long)d.f;                /* overflows */
  EQI(l, INT_MAX);              /* should return INT_MAX */
  EQU(ivo(), TRUE);

  l = INT_MIN;
  d.f = (double)l;              /* exact */
  l = (long)d.f;
  EQU(ivo(), FALSE);

  dpred(&d);                    /* into overflow */
  l = (long)d.f;
  EQI(l, INT_MIN);
  EQU(ivo(), TRUE);
}

static void t2581_dsigned_ll(void)
{
#ifdef LONG_LONG
  double_ints d;
  long long ll;

  ivo();                        /* clear IVO flag */

  ll = LL_MAX;
  d.f = (double)ll;             /* rounded up */
  dpred(&d);
  ll = (long long)d.f;          /* truncated */
  EQLL(ll, (LL_MAX & ~((1ull << (11-1))-1ull)));
  EQU(ivo(), FALSE);

  dsucc(&d);
  ll = (long long)d.f;          /* overflows */
  EQLL(ll, LL_MAX);
  EQU(ivo(), TRUE);

  ll = LL_MIN;                  /* exact */
  d.f = (double)ll;
  ll = (long long)d.f;
  EQLL(ll, LL_MIN);
  EQU(ivo(), FALSE);

  dpred(&d);                    /* overflows now */
  ll = (long long)d.f;
  EQLL(ll, LL_MIN);
  EQU(ivo(), TRUE);
#endif
}

static void t2581_dunsigned(void)
{
  double_ints d;
  unsigned long u;

  ivo();                        /* clear IVO flag */

                                /* unsigned */
                                /* double -> unsigned long  [-ve] */
  dset_to_mone(&d);
  u = (unsigned long)d.f;       /* should fail - invalid */
  EQU(u, 0);
  EQU(ivo(), TRUE);

                                /* double -> unsigned long  [overflow] */
  d.f = UINT_MAX/2;
  dsucc(&d);                    /* big */
  u = (unsigned long)d.f;       /* should not overflow */
  EQU(u, UINT_MAX/2+1);
  EQU(ivo(), FALSE);

                                /* double -> unsigned long  [overflow] */
  d.f = (double)UINT_MAX;
  u = (unsigned long)d.f;       /* should not overflow */
  EQU(u, UINT_MAX);
  EQU(ivo(), FALSE);

  dsucc(&d);
  u = (unsigned long)d.f;       /* overflows */
  EQU(u, UINT_MAX);
  EQU(ivo(), TRUE);
}

static void t2581_dunsigned_ll(void)
{
#ifdef LONG_LONG
  double_ints d;
  unsigned long long ull;

  ivo();                        /* clear IVO flag */

  dset_to_mone(&d);
  ull = (unsigned long long)d.f; /* not representable */
  EQUU(ull, 0);
  EQU(ivo(), TRUE);

  ull = ULL_MAX;
  d.f = (double)ull;            /* doesn't quite fit - rounded up */
  dpred(&d);
  ull = (unsigned long long)d.f; /* truncated */
  EQUU(ull, ULL_MAX & ~((1ull << 11) - 1ull));
  EQU(ivo(), FALSE);

  dsucc(&d);
  ull = (unsigned long long)d.f; /* overflows */
  EQUU(ull, ULL_MAX);           /* should return ull_max */
  EQU(ivo(), TRUE);
#endif
}

static void t2581_dinf(void)
{
  double_ints d;
  long l;
  unsigned long u;

  ivo();                        /* clear IVO flag */

  dset_to_inf(&d);
  l = (long)d.f;
  EQI(l, INT_MAX);
  EQU(ivo(), TRUE);

  u = (unsigned long)d.f;
  EQU(u, UINT_MAX);
  EQU(ivo(), TRUE);

  dneg(&d);
  l = (long)d.f;
  EQI(l, INT_MIN);
  EQU(ivo(), TRUE);

  u = (unsigned long)d.f;
  EQU(u, 0);
  EQU(ivo(), TRUE);
}

static void t2581_dinf_ll(void)
{
#ifdef LONG_LONG
  double_ints d;
  long long ll;
  unsigned long long ull;

  ivo();                        /* clear IVO flag */

  dset_to_inf(&d);
  ll = (long long)d.f;
  EQLL(ll, LL_MAX);
  EQU(ivo(), TRUE);

  ull = (unsigned long long)d.f;
  EQUU(ull, ULL_MAX);
  EQU(ivo(), TRUE);

  dneg(&d);
  ll = (long long)d.f;
  EQLL(ll, LL_MIN);
  EQU(ivo(), TRUE);

  ull = (unsigned long long)d.f;
  EQUU(ull, 0);
  EQU(ivo(), TRUE);
#endif
}


/* single precision tests */

static void t2581_fsigned(void)
{
  float_int f;
  long l;

  ivo();                        /* clear IVO flag */

  fset_to_one(&f);
  l = (long)f.f;
  EQI(l, 1);
  EQU(ivo(), FALSE);

  l = INT_MAX;
  f.f = (float)l;               /* does not fit - rounded up */
  fpred(&f);
  l = (long)f.f;
  EQI(l, INT_MAX & ~((1ul << (8-1)) - 1));
  EQU(ivo(), FALSE);

  fsucc(&f);
  l = (long)f.f;                /* overflows */
  EQI(l, INT_MAX);              /* should return INT_MAX */
  EQU(ivo(), TRUE);

  l = INT_MIN;
  f.f = (float)l;               /* fits exactly */
  l = (long)f.f;
  EQI(l, INT_MIN);              /* with no error */
  EQU(ivo(), FALSE);

  fpred(&f);                    /* take into underflow */
  l = (long)f.f;
  EQI(l, INT_MIN);
  EQU(ivo(), TRUE);
}

static void t2581_fsigned_ll(void)
{
#ifdef LONG_LONG
  float_int f;
  long long ll;

  ivo();                        /* clear IVO flag */

  ll = LL_MAX;
  f.f = (float)ll;              /* doesn't quite fit - rounded up */
  fpred(&f);                    /* bring into range */
  ll = (long long)f.f;          /* truncated */
  EQLL(ll, LL_MAX & ~((1ull << (32+8-1)) - 1ull));
  EQU(ivo(), FALSE);

  fsucc(&f);
  ll = (long long)f.f;          /* overflows */
  EQLL(ll, LL_MAX);
  EQU(ivo(), TRUE);

  ll = LL_MIN;                  /* exact */
  f.f = (float)ll;
  ll = (long long)f.f;
  EQLL(ll, LL_MIN);
  EQU(ivo(), FALSE);

  fpred(&f);                    /* take into overflow */
  ll = (long long)f.f;
  EQLL(ll, LL_MIN);
  EQU(ivo(), TRUE);
#endif
}

static void t2581_funsigned(void)
{
  float_int f;
  unsigned long u;

  ivo();                        /* clear IVO flag */

                                /* float -> unsigned long  [-ve] */
  fset_to_mone(&f);
  u = (unsigned long)f.f;       /* should fail - invalid */
  EQU(u, 0);
  EQU(ivo(), TRUE);

                                /* float -> unsigned long  [overflow] */
  f.f =(float)(UINT_MAX/2);     /* not >UINT_MAX, but large */
  fsucc(&f);
  u = (unsigned long)f.f;       /* should not overflow */
                                /* since float's don't have sufficient precision, this won't return
   * UINT_MAX/2+1, but UINT_MAX/2 + 1<<(length_of_float_exponent+1)+1
   */
  EQU(u, UINT_MAX/2 + (1<<8) + 1);
  EQU(ivo(), FALSE);

  f.f *= 2.0;                   /* causes overflow */
  u = (unsigned long)f.f;       /* overflows */
  EQU(u, UINT_MAX);
  EQU(ivo(), TRUE);

  u = UINT_MAX;
  f.f= (float)UINT_MAX;         /* rounds up */
  fpred(&f);
  u = (unsigned long)f.f;       /* just fits */
  EQU(u, UINT_MAX & ~((1ul << 8)-1ul));
  EQU(ivo(), FALSE);

  fsucc(&f);
  u = (unsigned long)f.f;       /* overflows */
  EQU(u, UINT_MAX);
  EQU(ivo(), TRUE);
}

static void t2581_funsigned_ll(void)
{
#ifdef LONG_LONG
  float_int f;
  unsigned long long ull;

  ivo();                        /* clear IVO flag */

  fset_to_mone(&f);
  ull = (unsigned long long)f.f; /* not representable */
  EQUU(ull, 0);
  EQU(ivo(), TRUE);

  ull = ULL_MAX;
  f.f = (float)ull;             /* doesn't quite fit - rounded up */
  fpred(&f);
  ull = (unsigned long long)f.f; /* truncated */
  EQUU(ull, (ULL_MAX & ~((1ull << (32+8))-1ull)));
  EQU(ivo(), FALSE);

  fsucc(&f);
  ull = (unsigned long long)f.f; /* overflows */
  EQUU(ull, ULL_MAX);           /* should return ull_max */
  EQU(ivo(), TRUE);
#endif
}

static void t2581_fnan(void)
{
  float_int f;
  long l;
  unsigned long u;

  ivo();                        /* clear IVO flag */

  fset_to_qnan(&f);
  l = (long)f.f;
  EQI(l, 0);
  EQU(ivo(), TRUE);

  u = (unsigned long)f.f;
  EQU(u, 0);
  EQU(ivo(), TRUE);

  fneg(&f);
  l = (long)f.f;
  EQI(l, 0);
  EQU(ivo(), TRUE);

  u = (unsigned long)f.f;
  EQU(u, 0);
  EQU(ivo(), TRUE);
}

static void t2581_fnan_ll(void)
{
#ifdef LONG_LONG
  float_int f;
  long long ll;
  unsigned long long ull;

  fset_to_qnan(&f);
  ll = (long long)f.f;
  EQLL(ll, 0);
  EQU(ivo(), TRUE);

  ull = (unsigned long long)f.f;
  EQUU(ull, 0);
  EQU(ivo(), TRUE);

  fneg(&f);
  ll = (long long)f.f;
  EQLL(ll, 0);
  EQU(ivo(), TRUE);

  ull = (unsigned long long)f.f;
  EQUU(ull, 0);
  EQU(ivo(), TRUE);
#endif
}

static void t2581_finf(void)
{
  float_int f;
  long l;
  unsigned long u;

  ivo();                        /* clear IVO flag */

  fset_to_inf(&f);
  l = (long)f.f;
  EQI(l, INT_MAX);
  EQU(ivo(), TRUE);

  u = (unsigned long)f.f;
  EQU(u, UINT_MAX);
  EQU(ivo(), TRUE);

  fneg(&f);
  l = (long)f.f;
  EQI(l, INT_MIN);
  EQU(ivo(), TRUE);

  u = (unsigned long)f.f;
  EQU(u, 0);
  EQU(ivo(), TRUE);
}

static void t2581_finf_ll(void)
{
#ifdef LONG_LONG
  float_int f;
  long long ll;
  unsigned long long ull;

  ivo();                        /* clear IVO flag */

  fset_to_inf(&f);
  ll = (long long)f.f;
  EQLL(ll, LL_MAX);
  EQU(ivo(), TRUE);

  ull = (unsigned long long)f.f;
  EQUU(ull, ULL_MAX);
  EQU(ivo(), TRUE);

  fneg(&f);
  ll = (long long)f.f;
  EQLL(ll, LL_MIN);
  EQU(ivo(), TRUE);

  ull = (unsigned long long)f.f;
  EQUU(ull, 0);
  EQU(ivo(), TRUE);
#endif
}

int main(void)
{
  /* disable the IVO exception */
  __fp_status(__fpsr_IOE+__fpsr_IOC, 0);

  BeginTest();

  /*
   * double tests
   */

  t2581_dnan();
  t2581_dinf();
  t2581_dsigned();
  t2581_dunsigned();

  t2581_dnan_ll();
  t2581_dinf_ll();
  t2581_dsigned_ll();
  t2581_dunsigned_ll();

  /*
   * float tests
   */
  t2581_fnan();
  t2581_finf();
  t2581_fsigned();
  t2581_funsigned();

  t2581_fnan_ll();
  t2581_finf_ll();
  t2581_fsigned_ll();
  t2581_funsigned_ll();

  EndTest();
  return 0;
}
