/* Library routines for testing math functions with odd values */
/* Copyright (C) Advanced RISC Machines, 1997. All Rights Reserved */
/* SPDX-Licence-Identifier: Apache-2.0 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef mathtest_h
#define mathtest_h

typedef union double_ints
{
  struct {long se_hi; unsigned long lo;} i;
  double f;
} double_ints;


typedef union float_int
{
  long sem;
  float f;
} float_int;

#define EQFI(x, y) EQI((x).sem, (y).sem)

/* Bits to make up a NaN - set to anything non-zero */
extern unsigned long dnan_low, dnan_high, fnan;

extern void dset_to_qnan(double_ints *x);
extern void dset_to_snan(double_ints *x);
extern unsigned int disnan(double_ints *x);
extern void dset_to_inf(double_ints *x);
extern unsigned int disinf(double_ints *x);
extern void dset_to_one(double_ints *x);
extern void dset_to_mone(double_ints *x);
extern void dsucc(double_ints *a);
extern void dpred(double_ints *a);
extern void dneg(double_ints *a);

extern void fset_to_qnan(float_int *x);
extern void fset_to_snan(float_int *x);
extern unsigned int fisnan(float_int *x);
extern void fset_to_inf(float_int *x);
extern unsigned int fisinf(float_int *x);
extern void fset_to_one(float_int *x);
extern void fset_to_mone(float_int *x);
extern void fsucc(float_int *a);
extern void fpred(float_int *a);
extern void fneg(float_int *a);

/* return TRUE if there's been an IVO, and clear the flag */
extern unsigned int ivo(void);

/* Check for errno errors */
extern int get_errno(void);

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#endif
