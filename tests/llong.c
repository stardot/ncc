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

/* The tests below are designed to exercise the compiler's    */
/* internal constant folding (expressions involving constant  */
/* operands), the compiler's internal constant folding after  */
/* dataflow analysis (expressions involving local non-address */
/* taken variables and constants) and the run-time support    */
/* (expressions involving statics, one function call removed  */
/* from the point of assignment to the static).               */

static long long l, l1;
static unsigned long long ul, ul1;
static unsigned u, u1;
static int i, i1;
static int a[1LL];
static int b = 123LL;
double d;
typedef struct {
  long long a: 55;
  long long b: 12;
  long long c: 40;
} LL;

void set_bf(LL *p) {
  p->a = 0x123456789abcd;
  p->b = 33;
  p->c = 0xa987654321;
}

void t_bf(void) {
  LL ll;
  set_bf(&ll);
  EQLL(ll.a, 0x123456789abcd);
  EQLL(ll.b, 33);
  EQLL(ll.c, 0xa987654321);
}

void t_sw(void) {
  l = 0;
  a[0LL] = 1;
  switch(u) {
  case 1LL: break;
  default:  EQI(u, 1); break;
  }
  switch(l) {
  case 1:   break;
  default:  EQI(u, 1); break;
  }
  EQI(a[0], 1);
  EQI(a[l], 1);
}

long long l_7(void) { return 7; }
unsigned long long u_7(void) { return 7; }

void t_add(void) {
  long long a, b;
  EQLL(1+2, 3);
  EQLL(1+0xffffffff, 0);
  EQLL(1+0xffffffffLL, 0x100000000);
  EQLL(5LL+7, 12);
  a = 3; b = 9;
  l = 5; l1 = 7; u = 0xffffffff; u1 = 4;
  EQLL(a+b, 12);
  EQLL(a+b, l+l1);
  EQLL(u+u1, 3);
  EQLL(u+(long long)u1, 0x100000003);
  a = -1; b = 3;
  l = -3;
  EQLL(a+b, 2);
  EQLL(a+5, 4);
  EQLL(l+b, 0);
  EQLL(l+2, a);
}

void t_sub(void) {
  long long a, b;
  EQLL(1-2, -1);
  EQLL(7-5LL, 2);
  a = 3; b = 9;
  l = 5; l1 = 7;
  EQLL(b-a, 6);
  EQLL(l1-l, l-a);
  a = -1; b = 3;
  l = -3;
  EQLL(a-b, -4);
  EQLL(a-(b-1), -3);
  EQLL(b-a, 4);
  EQLL(a-5, -6);
  EQLL(b-l, 6);
  EQLL(l-a, -2);
}

void t_mul(void) {
  long long a, b;
  EQLL(2*3, 6);
  EQLL(0x100000*0x300000LL, 0x30000000000);
  a = 3; b = 9;
  l = 5; l1 = 7;
  EQLL(b*a, 27);
  EQLL(l1*l, 35);
  a = -2; b = 3;
  l = -3; l1 = u = 0x123456;
  EQLL(a*b, -6);
  EQLL(a*5, -10);
  EQLL(b*l, -9);
  EQLL(l*a, 6);
  EQLL(0x123456*0x123456LL, 0x14b66cb0ce4);
  EQLL(l1*l1, 0x14b66cb0ce4);
  EQLL(u*u, 0x66cb0ce4);
}

void t_div(void) {
  l = 0x123456; i = -5; i1 = -3;
  EQLL(0x14b66cb0ce4 / l, l);
  EQLL(0x14b66cb0ce4 / l, l);
  EQLL(-5 / -3, 1);
  EQLL(i / i1, 1);
  EQLL(i / 2, -2);
  EQLL(40 / l_7(), 5);
  EQLL(40 % u_7(), 5);
  l = 0x8000000000000000;
  EQLL(l / -1, l);
  EQLL(l / -1, l);
  EQLL(l / -1, l);
  EQLL(l / -1, l);
}

void t_rem(void) {
  l = 0x123456; i = -5; i1 = -3;
  EQLL(0x14b66cb0ce4 % l, 0);
  EQLL(0x14b66cb0ce4 % l, 0);
  EQLL(-5 % -3, -2);
  EQLL(i % i1, -2);
  EQLL(i % 2, -1);
  EQLL(40 % u_7(), 5);
  EQLL(40 % l_7(), 5);
}

void t_neg(void) {
  long long ll = l1 = 0x123456789;
  l = 1;
  EQLL(-ll, -0x123456789);
  EQLL(-l1, -0x123456789);
  l1 = -1;
  EQLL(-l, -1);
  EQLL(-l1, 1);
}

void t_cmp(void) {
  long long ll = -1;
  unsigned long long lul = -1;
  ul = -1; l = -1;
  EQI(-1LL > 0, 0);
  EQI((unsigned long long)-1 > 0, 1);
  EQI(ll > 0, 0);
  EQI(lul > 0, 1);
  EQI(l > 0, 0);
  EQI(ul > 0, 1);
  EQI(l >= 0, 0);
  EQI(ul >= 0, 1);
  EQI(l < 0, 1);
  EQI(ul < 0, 0);
  EQI(l <= 0, 1);
  EQI(ul <= 0, 0);
  EQI(0x123456789ab > 0x123456789ac, 0);
  EQI(0x223456789ab > 0x123456789aa, 1);
}

void t_and(void) {
  unsigned long long lul;
  ul = lul = 0x1234567890123456;
  EQLL(0x1234567890123456 & 0xff00ff00ff00ff00, 0x1200560090003400);
  EQLL(0x1234567890123456 & 0xf0f0f0f00f0f0f0f, 0x1030507000020406);
  EQLL(lul & 0xff00ff00ff00ff00, 0x1200560090003400);
  EQLL(lul & 0xf0f0f0f00f0f0f0f, 0x1030507000020406);
  EQLL(ul & 0xff00ff00ff00ff00, 0x1200560090003400);
  EQLL(ul & 0xf0f0f0f00f0f0f0f, 0x1030507000020406);
}

void t_or(void) {
  unsigned long long lul;
  ul = lul = 0x1234567890123456;
  EQLL(0x1234567890123456 | 0xff00ff00ff00ff00, 0xff34ff78ff12ff56);
  EQLL(0x1234567890123456 | 0xf0f0f0f00f0f0f0f, 0xf2f4f6f89f1f3f5f);
  EQLL(lul | 0xff00ff00ff00ff00, 0xff34ff78ff12ff56);
  EQLL(lul | 0xf0f0f0f00f0f0f0f, 0xf2f4f6f89f1f3f5f);
  EQLL(ul | 0xff00ff00ff00ff00, 0xff34ff78ff12ff56);
  EQLL(ul | 0xf0f0f0f00f0f0f0f, 0xf2f4f6f89f1f3f5f);
}

void t_eor(void) {
  unsigned long long lul;
  ul = lul = 0x1234567890123456;
  EQLL(0x1234567890123456 ^ 0xff00ff00ff00ff00, 0xed34a9786f12cb56);
  EQLL(0x1234567890123456 | 0xf0f0f0f00f0f0f0f, 0xe2c4a6889f1d3b59);
  EQLL(lul | 0xff00ff00ff00ff00, 0xed34a9786f12cb56);
  EQLL(lul | 0xf0f0f0f00f0f0f0f, 0xe2c4a6889f1d3b59);
  EQLL(ul | 0xff00ff00ff00ff00, 0xed34a9786f12cb56);
  EQLL(ul | 0xf0f0f0f00f0f0f0f, 0xe2c4a6889f1d3b59);
}

void t_shift(void) {
  long long ll = -1;
  unsigned long long lul = -1;
  l = -1; ul = -1;
  EQLL(0xffffffffffffffff >> 5, 0x07ffffffffffffff);
  EQLL(-1LL >> 5, -1);
  EQLL(0xffffffffffffffff >> 34, 0x3fffffff);
  EQLL(-1LL >> 34, -1);
  EQLL(lul >> 5, 0x07ffffffffffffff);
  EQLL(ll >> 5, -1);
  EQLL(lul >> 34, 0x3fffffff);
  EQLL(ll >> 34, -1);
  EQLL(ul >> 5, 0x07ffffffffffffff);
  EQLL(l >> 5, -1);
  EQLL(ul >> 34, 0x3fffffff);
  EQLL(l >> 34, -1);
  l = ul = ll = lul = 0x9876543210234567;
  EQLL(0x9876543210234567 >> 40, 0x987654);
  EQLL((long long)0x9876543210234567 >> 40, 0xffffffffff987654);
  EQLL(0x9876543210234567 >> 20, 0x98765432102);
  EQLL((long long)0x9876543210234567 >> 20, 0xfffff98765432102);
  EQLL(lul >> 40, 0x987654);
  EQLL(ll >> 40, 0xffffffffff987654);
  EQLL(lul >> 20, 0x98765432102);
  EQLL(ll >> 20, 0xfffff98765432102);
  EQLL(ul >> 40, 0x987654);
  EQLL(l >> 40, 0xffffffffff987654);
  EQLL(ul >> 20, 0x98765432102);
  EQLL(l >> 20, 0xfffff98765432102);
  EQLL(0x9876543210234567 << 12, 0x6543210234567000);
  EQLL(0x9876543210234567 << 40, 0x2345670000000000);
  EQLL(lul << 12, 0x6543210234567000);
  EQLL(lul << 40, 0x2345670000000000);
  EQLL(l << 12, 0x6543210234567000);
  EQLL(l << 40, 0x2345670000000000);
  l = 1; i = 3;
  EQI(i << l, 6);
  EQI(i << l, 6);
  EQLL(i << l, 6);
}

void t_not(void) {
}

void t_pr(void) {
  char b[32];
  sprintf(b, "%llx", 0x9876543210234567);
  EQS(b, "9876543210234567");
  sscanf(b, "%llx", &ul);
  EQLL(ul, 0x9876543210234567);
  sprintf(b, "%lld", 0x1234567890);
  EQS(b, "78187493520");
  sscanf(b, "%lld", &l);
  EQLL(l, 0x1234567890);
  sprintf(b, "%lld", -0x1234567890);
  EQS(b, "-78187493520");
  sscanf(b, "%lld", &l);
  EQLL(l, -0x1234567890);
}

int main(void) {
  BeginTest();

  EQI(sizeof(long long), 8);
  t_add();
  t_sub();
  t_mul();
  t_div();
  t_rem();
  t_cmp();
  t_neg();
  t_and();
  t_or();
  t_not();
  t_shift();
  l = 1; u = 1;
  t_sw();
  t_bf();
  t_pr();

  EndTest();
  return 0;
}
