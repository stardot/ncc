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

#include "testutil.h"
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <signal.h>

typedef int bool;
#define T 1
#define F 0

/********************* xxxx ***********************/

#include "testutili.h"


float finf, fnan, fsnan, fone = 1.0;
double dinf, dnan, dsnan, done = 1.0;

jmp_buf jmpbuf;
bool exception;

#define set_jmp() (exception = 0, setjmp(jmpbuf))

void fpe_exception(int x)
{
    if (exception != 0)
    {
        fprintf(stdout, "Uncaught FP execption\n");
        test_fail();
        EndTest();
        abort();
    }
    exception = 1;
    signal(SIGFPE, fpe_exception);
    longjmp(jmpbuf,1);
}


int test_cmpd(int lineno, double x, double y, char *cmp, bool res1, bool res2)
{   test_execute();
    if (res1 == res2) {
        if (test_isverbose())
            fprintf(stdout, "\"%s\", line %d: OK\n", test_name(), lineno);
        return 0;
    }
    test_fail();
    fprintf(stdout, "\"%s\", line %d: %g %s %g failed\n", test_name(), lineno, x, cmp, y);
    return 1;
}

#define FCMP(a, b, cmp, r) test_cmpd(__LINE__, a, b, #cmp, (a cmp b), r)
#define FCMPE(a, b, cmp, r, exec) if (set_jmp() == 0) test_cmpd(__LINE__, a, b, #cmp, (a cmp b), r); else EQI(exception, exec);



void fcmp(float a, float b, bool eq, bool lt)
{
    FCMP(a, b, ==, eq);
    FCMP(a, b, !=, !eq);
    FCMP(a, b, < , lt);
    FCMP(a, b, <=, lt | eq);
    FCMP(a, b, > , !lt & !eq);
    FCMP(a, b, >=, !lt | eq);
}

void fcmpnan(float *a, float *b, bool snan)
{   /* one of a or b is a NaN - use ptrs to avoid conversion */
    FCMPE(*a, *b, ==, F, snan);
    FCMPE(*a, *b, !=, T, snan);
    FCMPE(*a, *b, < , F, T);
    FCMPE(*a, *b, <=, F, T);
    FCMPE(*a, *b, > , F, T);
    FCMPE(*a, *b, >=, F, T);
}

void dcmp(double a, double b, bool eq, bool lt)
{
    FCMP(a, b, ==, eq);
    FCMP(a, b, !=, !eq);
    FCMP(a, b, < , lt);
    FCMP(a, b, <=, lt | eq);
    FCMP(a, b, > , !lt & !eq);
    FCMP(a, b, >=, !lt | eq);
}

void dcmpnan(double *a, double *b, bool snan)
{   /* one of a or b is a NaN */
    FCMPE(*a, *b, ==, F, snan);
    FCMPE(*a, *b, !=, T, snan);
    FCMPE(*a, *b, < , F, T);
    FCMPE(*a, *b, <=, F, T);
    FCMPE(*a, *b, > , F, T);
    FCMPE(*a, *b, >=, F, T);
}

void t_cmp(void)
{
    /* enable the IVO exception */
    __fp_status(__fpsr_IOE, __fpsr_IOE);

    fcmp( 0.0,  0.0, T, F);
    fcmp(-0.0,  0.0, T, F);
    fcmp(-0.0, -0.0, T, F);
    fcmp( 0.0, -0.0, T, F);

    fcmp( 1.0,  1.0, T, F);
    fcmp(-1.0,  1.0, F, T);
    fcmp(-1.0, -1.0, T, F);
    fcmp( 1.0, -1.0, F, F);

    fcmp( 1.0,  2.0, F, T);
    fcmp(-1.0,  2.0, F, T);
    fcmp( 1.0, -2.0, F, F);
    fcmp(-1.0, -2.0, F, F);

    fcmp( finf, finf, T, F);
    fcmp(-finf, finf, F, T);
    fcmp( finf,-finf, F, F);
    fcmp(-finf,-finf, T, F);

    dcmp( 0.0,  0.0, T, F);
    dcmp(-0.0,  0.0, T, F);
    dcmp(-0.0, -0.0, T, F);
    dcmp( 0.0, -0.0, T, F);

    dcmp( 1.0,  1.0, T, F);
    dcmp(-1.0,  1.0, F, T);
    dcmp(-1.0, -1.0, T, F);
    dcmp( 1.0, -1.0, F, F);

    dcmp( 1.0,  2.0, F, T);
    dcmp(-1.0,  2.0, F, T);
    dcmp( 1.0, -2.0, F, F);
    dcmp(-1.0, -2.0, F, F);

    dcmp( dinf, dinf, T, F);
    dcmp(-dinf, dinf, F, T);
    dcmp( dinf,-dinf, F, F);
    dcmp(-dinf,-dinf, T, F);

    dcmp( 1.0000000001,  1.0, F, F);
    dcmp( 1.0000000001, -1.0, F, F);
    dcmp(-1.0000000001,  1.0, F, T);
    dcmp(-1.0000000001, -1.0, F, T);

    dcmp( 0.9999999999,  1.0, F, T);
    dcmp( 0.9999999999, -1.0, F, F);
    dcmp(-0.9999999999,  1.0, F, T);
    dcmp(-0.9999999999, -1.0, F, F);

    fcmpnan(&fnan, &finf, F);
    fcmpnan(&fnan, &fone, F);
    fcmpnan(&finf, &fnan, F);
    fcmpnan(&fone, &fnan, F);
    fcmpnan(&finf, &fsnan,T);
    fcmpnan(&fsnan, &fnan,T);
    fcmpnan(&fone, &fsnan,T);
    fcmpnan(&fsnan, &finf,T);

    dcmpnan(&dnan, &dinf, F);
    dcmpnan(&dnan, &done, F);
    dcmpnan(&dinf, &dnan, F);
    dcmpnan(&done, &dnan, F);
    dcmpnan(&dinf, &dsnan, T);
    dcmpnan(&dsnan, &dnan, T);
    dcmpnan(&done, &dsnan, T);
    dcmpnan(&dsnan, &dnan, T);

    __fp_status(__fpsr_IOE, 0);     /* IVO disabled -> signalling NaNs behave like normal NaNs */

    fcmpnan(&finf, &fsnan,T);
    fcmpnan(&fsnan, &fnan,T);
    fcmpnan(&fone, &fsnan,T);
    fcmpnan(&fsnan, &finf,T);

    dcmpnan(&dinf, &dsnan, T);
    dcmpnan(&dsnan, &dnan, T);
    dcmpnan(&done, &dsnan, T);
    dcmpnan(&dsnan, &dnan, T);

    __fp_status(__fpsr_IOE, __fpsr_IOE);
}


void t_init(void)
{
    int *p;

    p = (int*) &finf;
    p[0] = 0x7F800000;
    p = (int*) &fnan;
    p[0] = 0x7FC00000;
    p = (int*) &fsnan;
    p[0] = 0x7F800001;
    p = (int*) &dinf;
    p[0] = 0x7FF00000;
    p[1] = 0;
    p = (int*) &dnan;
    p[0] = 0x7FF80000;
    p[1] = 1;
    p = (int*) &dsnan;
    p[0] = 0x7FF00000;
    p[1] = 1;           /* tricky NaN - high word looks like an infinite */

    signal(SIGFPE, fpe_exception);
}


/********************* main ***********************/

int main(void)
{
  BeginTest();
  t_init();
  t_cmp();
  EndTest();

  return 0;
}
