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

typedef __packed struct {
  int (*f)(int, int, int, int);
} S_667;

int g_667(int a, int b, int c, int d) {
  return a*b + c*d;
}

int f_667(S_667 *s) {
  return s->f(1,2,3,4);
}

void t_667(void) {
  S_667 s; s.f = g_667;
  EQI(f_667(&s), 14);
}

typedef __packed struct {
   unsigned short p1;
   unsigned short p2;
   unsigned short p3;
} S_931;

static void f_931(S_931 s, int a, int b, int c) {
  EQI(s.p1, a);
  EQI(s.p2, b);
  EQI(s.p3, c);
}

static S_931 ss = { 1, 2, 3 };

void t_931(void)
{
   S_931 s = { 0, 0, 0 };
   f_931(s, 0, 0, 0);
   s = ss;
   f_931(ss, 1, 2, 3);
}

typedef __packed struct {
  int a;
  char pad1;
  int b;
  char pad2;
  int c;
  char pad3;
  int d;
  char pad4;

  short e;
  char pad5;
  short f;
  char pad6[3];
} S_xxx;

void test1_xxx_unaligned(S_xxx *t) {
  EQI(t->a, 0x12345678); /* Not BaseAligned: Offset 0 */
  EQI(t->b, 0x12345678); /* Not BaseAligned: Offset 1 */
  EQI(t->c, 0x12345678); /* Not BaseAligned: Offset 2 */
  EQI(t->d, 0x12345678); /* Not BaseAligned: Offset 3 */
  EQI(t->e, 0x1234);     /* Not BaseAligned (halfword): Offset 0 */
  EQI(t->f, 0x1234);     /* Not BaseAligned (halfword): Offset 1 */
}

void test1_xxx_aligned(S_xxx t) {
  EQI(t.a, 0x12345678);  /* BaseAligned: Offset 0 */
  EQI(t.b, 0x12345678);  /* BaseAligned: Offset 1 */
  EQI(t.c, 0x12345678);  /* BaseAligned: Offset 2 */
  EQI(t.d, 0x12345678);  /* BaseAligned: Offset 3 */
  EQI(t.e, 0x1234);      /* BaseAligned (halfword): Offset 0 */
  EQI(t.f, 0x1234);      /* BaseAligned (halfword): Offset 1 */
}

void test_xxx_args(int a, int b, int c, int d) {
  EQI(a, 0x12345678);
  EQI(b, 0x12345678);
  EQI(c, 0x12345678);
  EQI(d, 0x12345678);
}

void test2_xxx_unaligned(S_xxx *t) {
  test_xxx_args(t->a, t->b, t->c, t->d); /* Not BaseAligned: fnargs */
}

void test2_xxx_aligned(S_xxx t) {
  test_xxx_args(t.a, t.b, t.c, t.d);  /* BaseAligned: fnargs */
}

S_xxx s1_xxx = {
  0x12345678,
  0,
  0x12345678,
  0,
  0x12345678,
  0,
  0x12345678,
  0,
  0x1234,
  0,
  0x1234,
};

S_xxx s2_xxx;

typedef __packed struct {
    char c;
    unsigned short l, r, t, b;
    unsigned short tl, tr, tt, tb;
} S_1093;

int f_1093(S_1093 *a, S_1093 *b)
{
    return
    (a->tt >= b->tt && a->tt < b->tb) ||
    (a->tb <= b->tb && a->tb > b->tt) ||
    (b->tt >= a->tt && b->tt < a->tb) ||
    (b->tb <= a->tb && b->tb > a->tt);
}
void t_1093() {
  S_1093 a = {0, 0,0,0,0, 0,0,260,261},
         b = {0, 0,0,0,0, 0,512,256,257};
  EQI(f_1093(&a, &b), 0);

}

/********************* 1574 ***********************/

void f_1574(char *a, int b, int c, char *d, int e, int f, int g) {
  EQI(b, 0);
  EQI(c, 3);
  EQI(e, 5);
  EQI(f, 0);
  EQI(g, 1);
}

void g_1574(int __packed *p) {
  f_1574("xxx", 0, 3, "yyy", *p, 0, 1);
}

__packed struct { char a; int b; } s_1574;

void t_1574(void) {
  s_1574.b = 5;
  g_1574(&s_1574.b);
}

/********************* 1590 ***********************/

__packed struct { char x; short y; int z; } px_1590;

void f_1590(char const *s, int a, int b, int c) {
  EQI(a, 0);
  EQI(b, 1);
  EQI(c, 2);
}

void t_1590(void) {
  px_1590.x = 0;
  px_1590.y = 1;
  px_1590.z = 2;
  f_1590("s", px_1590.x, px_1590.y, px_1590.z);
}

/********************* 1603 ***********************/

__packed struct { char x; short y; int z; } s_1603 = {0, 1, 2};

void f_1603(char const *s, int a, int b, int c) {
  EQI(a, 0);
  EQI(b, 1);
  EQI(c, 2);
}

void t_1603(void) {
  f_1603("s", s_1603.x, s_1603.y, s_1603.z);
}

/********************* 1982 ***********************/

/* failed or gave compiletime error */

typedef struct
{
    int (*f)(int, int, int, int);
}   t0_1982_vtable;

typedef __packed struct
{
    t0_1982_vtable *q;
} t0_1982_mystr;

int t1_1982(t0_1982_mystr *p)
{
    p->q->f(0, 1, 2, 3);
}

int t2_1982(int a, int b, int c, int d)
{
    return a + b + c + d;
}

void t_1982()
{
    t0_1982_vtable v;
    t0_1982_mystr str;
    str.q = &v;
    v.f = t2_1982;
    EQI(t1_1982(&str), 6);
}

/********************* 2297 ***********************/

int (*fv)(int, int, int, int);

typedef __packed struct { int x; } S;

int t0_2297(S *s)
{
    return fv(3, 0, 1, s->x);
}

int t1_2297(int a, int b, int c, int d)
{
    return a + b + c + d;
}

void t_2297()
{
    S s = { 2 };
    fv = t1_2297;
    EQI(t0_2297(&s), 0+1+3+2);
}

/*********************  ***********************/

void t_xxx(void) {
  /* Performing tests on static initialised packed struct */
  test1_xxx_aligned(s1_xxx);
  test1_xxx_unaligned(&s1_xxx);
  test2_xxx_aligned(s1_xxx);
  test2_xxx_unaligned(&s1_xxx);
  /* Performing tests on dynamically initialised packed struct */
  s2_xxx.a = 0x12345678;
  s2_xxx.b = 0x12345678;
  s2_xxx.c = 0x12345678;
  s2_xxx.d = 0x12345678;
  s2_xxx.e = 0x1234;
  s2_xxx.f = 0x1234;
  test1_xxx_aligned(s2_xxx);
  test1_xxx_unaligned(&s2_xxx);
  test2_xxx_aligned(s2_xxx);
  test2_xxx_unaligned(&s2_xxx);
}

int main() {
  BeginTest();
  t_667();
  t_931();
  t_xxx();
  t_1093();
  t_1574();
  t_1590();
  t_1603();
  t_1982();
  t_2297();
  EndTest();
  return 0;
}
