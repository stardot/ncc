/* Library routines for testing math functions with odd values */
/* Copyright (C) Advanced RISC Machines, 1997. All Rights Reserved */
/* SPDX-Licence-Identifier: Apache-2.0 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include "mathtest.h"

#define SIGN_BIT           0x80000000ul

#define DINFNAN_INT_SE_HI  0x7FF00000ul
#define DINFNAN_INT_LO     0x00000000ul

#define DQNAN_BIT          0x00080000ul

unsigned long dnan_low = 1, dnan_high = 0, fnan = 1;

void dset_to_qnan(double_ints *x)
{
  if ((dnan_low == 0ul && dnan_high == 0ul) ||
      (dnan_high & (DINFNAN_INT_SE_HI+DQNAN_BIT)) != 0ul ||
      (dnan_low & DINFNAN_INT_LO) != 0ul) {
    fprintf(stderr, "Test error - dset_to_qnan [%08lx/%08lx]\n",
            dnan_high, dnan_low);
    exit(1);
  }
  x->i.se_hi = DINFNAN_INT_SE_HI + DQNAN_BIT + dnan_high;
  x->i.lo    = DINFNAN_INT_LO + dnan_low;
  return;
}

void dset_to_snan(double_ints *x)
{
  if ((dnan_low == 0ul && dnan_high == 0ul) ||
      (dnan_high & (DINFNAN_INT_SE_HI+DQNAN_BIT)) != 0ul ||
      (dnan_low & DINFNAN_INT_LO) != 0ul) {
    fprintf(stderr, "Test error - dset_to_snan [%08lx/%08lx]\n",
            dnan_high, dnan_low);
    exit(1);
  }
  x->i.se_hi = DINFNAN_INT_SE_HI + dnan_high;
  x->i.lo    = DINFNAN_INT_LO + dnan_low;
  return;
}

unsigned int disnan(double_ints *x)
{
  return ( ((x->i.se_hi & DINFNAN_INT_SE_HI) == DINFNAN_INT_SE_HI) &&
           ( (x->i.se_hi & ~(SIGN_BIT+DINFNAN_INT_SE_HI)) != 0 ||
             x->i.lo != 0) );
}

void dset_to_inf(double_ints *x)
{
  x->i.se_hi = DINFNAN_INT_SE_HI;
  x->i.lo    = DINFNAN_INT_LO;
  return;
}

unsigned int disinf(double_ints *x)
{
  return ( ((x->i.se_hi & DINFNAN_INT_SE_HI) == DINFNAN_INT_SE_HI) &&
           (x->i.se_hi & ~(SIGN_BIT+DINFNAN_INT_SE_HI)) == 0 &&
           x->i.lo == 0 );
}

void dset_to_one(double_ints *x)
{
  x->f = 1.0;
  return;
}

void dset_to_mone(double_ints *x)
{
  x->f = -1.0;
}

void dsucc(double_ints *a)
{
  /* return a number at least 1 bigger than the argument */
  double x = a->f;
  a->f += 1.0;
  if (a->f == x) {
    unsigned lo = a->i.lo;
    if (a->i.se_hi & SIGN_BIT) {
      a->i.lo--;
      if (lo < a->i.lo) a->i.se_hi--;
    } else {
      a->i.lo++;
      if (lo > a->i.lo) a->i.se_hi++;
    }
  }
}

void dpred(double_ints *a)
{
  double x = a->f;
  a->f -= 1.0;
  if (a->f == x) {
    unsigned lo = a->i.lo;
    if (a->i.se_hi & SIGN_BIT) {
      a->i.lo++;
      if (lo > a->i.lo) a->i.se_hi++;
    } else {
      a->i.lo--;
      if (lo < a->i.lo) a->i.se_hi--;
    }
  }
}

void dneg(double_ints *a)
{
  a->i.se_hi |= SIGN_BIT;
}

#define FINFNAN_INT_SEM  0x7f800000ul
#define FQNAN_BIT        0x00400000ul

void fset_to_qnan(float_int *x)
{
  if (fnan == 0ul ||
      (fnan & (FINFNAN_INT_SEM+FQNAN_BIT)) != 0ul) {
    fprintf(stderr, "Test error - fset_to_qnan [%08lx]\n", fnan);
    exit(1);
  }
  x->sem = FINFNAN_INT_SEM + FQNAN_BIT + fnan;
  return;
}

void fset_to_snan(float_int *x)
{
  if (fnan == 0ul ||
      (fnan & (FINFNAN_INT_SEM+FQNAN_BIT)) != 0ul) {
    fprintf(stderr, "Test error - fset_to_qnan [%08lx]\n", fnan);
    exit(1);
  }
  x->sem = FINFNAN_INT_SEM + fnan;
  return;
}

unsigned int fisnan(float_int *x)
{
  return ( (x->sem & FINFNAN_INT_SEM) == FINFNAN_INT_SEM &&
           (x->sem &~(SIGN_BIT+FINFNAN_INT_SEM)) != 0);
}

void fset_to_inf(float_int *x)
{
  x->sem = FINFNAN_INT_SEM;
  return;
}

unsigned int fisinf(float_int *x)
{
  return ( (x->sem & FINFNAN_INT_SEM) == FINFNAN_INT_SEM &&
           (x->sem &~(SIGN_BIT+FINFNAN_INT_SEM)) == 0);
}

void fset_to_one(float_int *x)
{
  x->f = 1.0;
  return;
}

void fset_to_mone(float_int *x)
{
  x->f = -1.0;
}

void fsucc(float_int *a)
{
  /* return a number bigger than the argument */
  float x = a->f;
  a->f += 1.0;
  if (a->f == x) {
    if (a->sem & SIGN_BIT)
      a->sem--;
    else
      a->sem++;
  }
}

void fpred(float_int *a)
{
  float x = a->f;
  a->f -= 1.0;
  if (a->f == x) {
    if (a->sem & SIGN_BIT)
      a->sem++;
    else
      a->sem--;
  }
}

void fneg(float_int *a)
{
  a->sem |= SIGN_BIT;
}


/* return TRUE if there's been an IVO, and clear the flag */
unsigned int ivo(void)
{
  unsigned long flags = __fp_status(__fpsr_IOC, 0);
  return (flags & __fpsr_IOC) != 0;
}

int get_errno(void)
{
  int e = errno;
  errno = 0;
  return e;
}

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

