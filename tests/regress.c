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

#include <setjmp.h>
#include <stdio.h>
#include <string.h>
#include "testutil.h"

/*********************      ***********************/

typedef struct { int a; short x; } X_xxx;

unsigned char f_xxx(X_xxx *x, int y) {
  return x->x > (y & 0x3fff);
}

void t_xxx() {
  X_xxx x = { 2, 3 };
  EQI(f_xxx(&x, 1), 1);
}

/*********************  536 ***********************/

jmp_buf buf;

int h_536(int x) { EQI(x, 1); longjmp(buf, 1); return 0; }

int g_536(int x) { if (x != 1) return h_536(x); else return x+1; }

int t_536() {
  struct { int a[1030]; int (* f)(int); } xx; xx.f = g_536;
  if (setjmp(buf) == 0) {
    EQI(xx.f(1), 2);
    return 0;
  } else
    return 0;
}

/*********************  591 ***********************/

void t_591() {
  char a[4];
  strcpy(a, "xxx");
  strcpy(a, "a\0b");
  EQI(a[2], 'x');
}

/*********************  593 ***********************/

typedef struct { int sig; } S_593;

static int x_593[1000];

int sub_593(int *ap) {
  S_593 *uap = (S_593 *)ap;
  int ret;
  ret = x_593[uap->sig + 1];
  return(ret);
}

void t_593(void) {
  int n = 33;
  x_593[34] = 7;
  EQI(sub_593(&n), 7);
}

/*********************  628 ***********************/

int f_628(int x) {
  int a = (x>0?10:20)/7+3;
  int b = (x>0?0:1)/7+3;
  return a+b;
}

void t_628() {
  EQI(f_628(1), 7);
}

/*********************  639 ***********************/

void g_639(unsigned short *p, int i, int j, int k, int l) {
  *p = i;
}

int f_639() {
  unsigned short p[6];
  p[0] = 0;
  g_639(p, 1, 2, 3, 4);
  return *p == 0;
}

void t_639(void) {
  EQI(f_639(), 0);
}

/*********************  678 ***********************/

typedef struct S_678 S_678;

struct S_678 {
  void (*f)(S_678 *, int);
  int x;
};

void g_678(S_678 *p, int n) {
  p->x += n;
}

void f_678(S_678 *p)
{
  if (p->x == -1)
   p->f(p, 1);
  else
   p->f(p, 5);
}

void t_678(void) {
  S_678 s;
  s.f = g_678; s.x = -1;
  f_678(&s);
  EQI(s.x, 0);
  f_678(&s);
  EQI(s.x, 5);
}

/*********************  683 ***********************/

int f_683(unsigned x, unsigned y) {
  x = (x & 0xff00ffff) | ((x & (y << 16)) & 0xff0000);
  return x;
}

void t_683() {
  EQI(f_683(0x11ff1111, 0x34), 0x11341111);
}

/*********************  701 ***********************/

typedef struct T_701 T_701;
typedef struct E_701 E_701;
struct E_701 {
  int n;
  T_701 *t;
  E_701 *a;
};

static E_701 e2_701, e1_701 = {74, 0, &e2_701};

E_701 *g_701(int a, T_701 *t, E_701 *e) {
  T_701 *tt = NULL;
  EQI((int)t, (int)tt);
  EQI((int)e, (int)&e2_701);
  return e;
}

E_701 *h_701(T_701 *t, E_701 *e, int x) {
  return e;
}

static E_701 *f_701(E_701 *x, T_701 *t) {
    return (x->n == 74) ?
        g_701(74, t, x->a) :
        h_701(t, x->a, 0);
}

int t_701(void) {
    f_701(&e1_701, 0);
    return 0;
}

/*********************  724 ***********************/

typedef union { int : 20; int : 20; double d; int i; } U_724;
U_724 u_724 = { 1.23 },
      v_724 = { 2.34 };

void t_724(void) {
    U_724 *p = &u_724;
    EQI(sizeof(u_724), sizeof(double));
    EQI(v_724.i, p[1].i);
}

/*********************  725 ***********************/

typedef struct S_725 {
  int i;
} S_725;

int f_725(S_725 s) {
  switch (s.i) {
  case 2:break;
  default:return 7;
  }
  return 5;
}

/* With faulty compilers, case 2: generates a spurious error:
   cast to non-equal 'S' illegal.
 */
void t_725(void) {
  S_725 s;
  s.i = 7;
  EQI(f_725(s), 7);
}

/*********************  727 ***********************/

typedef struct {
    int size;
    int put_ix;
    int get_ix;
    int cnt;
    unsigned char *buff;
} S_727;

unsigned char f_727(S_727 *p)
{
    p->cnt--;
    return p->get_ix == p->size-1 ?
        p->buff[(p->get_ix = 0, p->size-1)] :
        p->buff[p->get_ix++];
}

void t_727(void) {
    S_727 s;
    unsigned char b[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
    s.buff = b; s.size = 16;
    s.get_ix = 3;
    s.cnt = 7;
    EQI(f_727(&s), 4);
    EQI(s.get_ix, 4);
}

/*********************  742 ***********************/

int f_742(int x, int len, int code)
{
    int i;
    if (code != 1) return len;
    for (i = 0; i < len; i++)
        if (x == i) i++;
    return i;
}

void t_742() {
    EQI(f_742(1, 6, 2), 6);
}

/*********************  770 ***********************/

int f_770(int i) { return ((i>>31) & 2) != 0; }

void t_770() {
    EQI(f_770(0x80000000), 1);
}

/*********************  776 ***********************/

typedef struct {
    int c;
    int xx[150];
} S_776;

static S_776 s_776 = { 0 };

S_776 f_776(void) { S_776 x; x=s_776; x.c=1; return x; }

void g_776(int i, int j) { }

void t_776() {
    int i = 7, j = 8, k = 9, l = 10, m = 22, n = 23, n1 = 14, n2 = 25;
    {
        S_776 xx0, xx1, xx2, xx3, xx4, xx5, xx6, xx7;
        g_776(i >>= (xx0 = f_776()).c, 3);
        g_776(j >>= (xx1 = f_776()).c, 4);
        g_776(k >>= (xx2 = f_776()).c, 4);
        l >>= (xx3 = f_776()).c;
        g_776(l, 5);
        g_776(m >>= (xx4 = f_776()).c, 11);
        n >>= (xx5 = f_776()).c;
        g_776(n, 11);
        n1 >>= (xx6 = f_776()).c;
        EQI(n1, 7);
        n2 >>= (xx7 = f_776()).c;
        EQI(n2, 12);
    }
}

/*********************  778 ***********************/

typedef struct { unsigned int hi, lo; } S_778;

typedef struct { unsigned int a, b, c, d, e, f, g, h, i, j, k; } S_778a;

static S_778 s_778;
static S_778a s_778a;

void f_778(void) {
  S_778 x = {0x3ff00000, 0};
  x.lo = 1;
  s_778 = x;
}

void f_778a(void) {
  S_778a x = {10,11,12,13,14,15,16,17,18,19,20};
  x.j = 1;
  s_778a = x;
}

void t_778() {
  f_778();
  EQI(s_778.lo, 1);
  EQI(s_778.hi, 0x3ff00000);
  f_778a();
  EQI(s_778a.i, 18);
  EQI(s_778a.j, 1);
}

/*********************  825 ***********************/

typedef struct { int a, b, c; } S_825;

typedef struct { int a; char b, c; S_825 s; char d, e, f, g; } S1_825;

static S_825 MkS_825(void) { S_825 s; s.a = s.b = s.c = 0; return s; }

static void f_825(S1_825 * s1, int x)
{ s1->a = 0;
  s1->b = 6 + x;
  s1->c = 3;
  s1->s = MkS_825();
  s1->d = 0;
  s1->e = 5;
  s1->f = 1;
  s1->g = 0;
}

void t_825() {
  S1_825 s;
  f_825(&s, 3);
  EQI(s.d, 0);
  EQI(s.e, 5);
  EQI(s.f, 1);
  EQI(s.g, 0);
}

/*********************  919 ***********************/

typedef struct {
  int x, y, z;
  int a[1024], b[1024];
} S_919;

void f_919(S_919 *s) {
  int i;
  for (i = 0; i < 1024; i++) { s->a[i] = -1; s->b[i] = i; }
}

void t_919() {
  S_919 s;
  memset(&s, 0, sizeof(S_919));
  f_919(&s);
  EQI(s.a[3], -1);
  EQI(s.b[5], 5);
}

/*********************  922 ***********************/

typedef struct { unsigned n; unsigned p; } S_922;

void f_922(unsigned p) {}

void g_922(S_922 *s)
{
  if (s->n == 0) {
    f_922(0x1234); s->p = 0x1234;
  } else {
    f_922(0x1234); s->p = 0x1234;
  }
}

void t_922(void) {
  S_922 s = { 15, 23};
  g_922(&s);
  EQI(s.p, 0x1234);
}

/********************* 1049 ***********************/

int a_1049;
int b_1049[2] = {1, 2};

int f_1049(void) {
    return *((int*) (a_1049 + 4));
}


void t_1049(void) {
    a_1049 = (int)b_1049;
    EQI(f_1049(), 2);
}

/********************* 1075 ***********************/

typedef unsigned uint;

static char *inf;

typedef int F(void);

static int f1(void);
static int *f2(void);
static int f3(int x);
static int f4(char *p);

/* pf needs to be spilt, but not because it's address-taken, for this
 * test to fail (If it's address-taken, there will be no tailcall).
 * Accordingly, it's a bit delicate, and probably in this form is OK
 * with apcs /noswst even with faulty compilers.
 */
void f(F *pf, uint ns) {
  int i;
  int format = f1();
  for (i = 1; i <= ns; i++) {
    int *p = f2();
    if (format == 0)
      *p = f3(3);
    else {
      int s = f4(inf);
      int n = f4(inf);
      if (s == 0) *p = 0;
    }
  }
  pf();
}

static int s;

static int f1() { return 0; }
static int *f2() { return &s; }
static int f3(int x) { return x+3; }
static int f4(char *p) { return 0; }

void t_1075() {
  int v[32];
  int i;
  for (i = 0; i < 32; i++) v[i] = 0;
  /* That tries to ensure that, when pf is loaded from the wrong place
   * inside f, the value loaded will cause a fault when used.
   */
  f(f1, 3);
  EQI(s, 6);
}

/********************* 1152 ***********************/

typedef struct { void *a, *b, *c, *d; } S_1152;
S_1152 f_1152(S_1152 *p) { return *p; }
void t_1152(void) {
    S_1152 a, b, c;
    c.a = c.b = c.c = c.d = 0;
    b.a = b.b = b.c = b.d = &c;
    a = f_1152(&b);
    EQI((int)a.a, (int)b.a);
    EQI((int)a.b, (int)b.b);
    EQI((int)a.c, (int)b.c);
    EQI((int)a.d, (int)b.d);
    EQI((int)c.a, 0);
    EQI((int)c.b, 0);
    EQI((int)c.c, 0);
    EQI((int)c.d, 0);
}

/********************* 1193 ***********************/

void g_1193(int *x, int *y, int *z) { *x = *y = 0; }
void h_1193(int a, int b) {
  EQI(a, 1);
  EQI(b, 2);
}
void f_1193(short *s, int i, int x) {
  int a, b;
  int v[1024];
  g_1193(&a, &b, v);
  a += 1;
  s[i] = x;
  b += 2;
  h_1193(a, b);
}
void t_1193() {
  short s[4];
  f_1193(s, 1, 53);
  EQI(s[1], 53);
}

/********************* 1280 ***********************/

typedef struct { int a:32; } S_1280;

void f_1280(S_1280 *s) { s->a = 0; }
void t_1280() {
  union {
    int i;
    S_1280 s;
  } s;
  s.i = -1;
  f_1280(&s.s);
  EQI(s.i, 0);
}

/********************* 1281 ***********************/

void f1_1281(int *p) { *p = 33; }
void f2_1281(void)   { }
void f3_1281(int *p) { *p = 57; }

void f_1281(int *a, int b, int *c) {
  if (b)
    f1_1281(c);
  else {
    f2_1281();
    f3_1281(a);
  }
}

void t_1281(void) {
  int a = 0, b = 0;
  f_1281(&a, 1, &b);
  EQI(a, 0);
  EQI(b, 33);
}

/********************* 1282 ***********************/

typedef struct { int a, b, c, d, e, f; } S_1282;

int f_1282(int *p, int n) {
  *p = n;
  return 1;
}

void f1_1282(S_1282 *s, int n) {
  s->d = f_1282(&s->a, n) & 0;
  s->e = f_1282(&s->b, n) | -1;
  s->f = f_1282(&s->c, n) * 0;

}

void f2_1282(S_1282 *s, int n) {
  s->d = (s->a = n) & 0;
  s->e = (s->b = n) | -1;
  s->f = (s->c = n) * 0;
}

void f3_1282(S_1282 *s, int n) {
  s->d = (s->a++) & 0;
  s->e = (s->b++) | -1;
  s->f = (s->c++) * 0;
}

void f4_1282(S_1282 *s, int n) {
  s->d = (++s->a) & 0;
  s->e = (++s->b) | -1;
  s->f = (++s->c) * 0;
}

void t_1282(void) {
  S_1282 s, s1 = {0,0,0,3,3,3};
  s = s1;
  f1_1282(&s, 5);
  EQI(s.a, 5);
  EQI(s.b, 5);
  EQI(s.c, 5);
  EQI(s.d, 0);
  EQI(s.e, -1);
  EQI(s.f, 0);
  s = s1;
  f2_1282(&s, 7);
  EQI(s.a, 7);
  EQI(s.b, 7);
  EQI(s.c, 7);
  EQI(s.d, 0);
  EQI(s.e, -1);
  EQI(s.f, 0);
  s = s1;
  f3_1282(&s, 3);
  EQI(s.a, 1);
  EQI(s.b, 1);
  EQI(s.c, 1);
  EQI(s.d, 0);
  EQI(s.e, -1);
  EQI(s.f, 0);
  s = s1;
  f4_1282(&s, 13);
  EQI(s.a, 1);
  EQI(s.b, 1);
  EQI(s.c, 1);
  EQI(s.d, 0);
  EQI(s.e, -1);
  EQI(s.f, 0);
}

/********************* 1360 ***********************/

int f_1360(int a) {
  return a;
}

int g_1360(int a, int b, int c, int d) {
  int e;
  int arr[1000];
  a = f_1360(a);
  e = d + 100;
  arr[100] = a;
  arr[101] = b;
  arr[102] = c;
  arr[103] = d;
  return arr[e];
}

void t_1360(void) {
  EQI(g_1360(10, 11, 12, 0),10);
  EQI(g_1360(13, 14, 15, 1),14);
  EQI(g_1360(16, 17, 18, 2),18);
  EQI(g_1360(19, 20, 21, 3), 3);
}

/********************* 1404 ***********************/

#include <string.h>
char b_1404[13];
char c_1404[13];

void t_1404()
{
  memcpy( c_1404,"cccccc",6 );
  memcpy( b_1404,"bbbbbbbbbbbbbeeee",13 );
  EQI(0, memcmp(c_1404, "cccccc", 6));
  EQI(0, memcmp(b_1404, "bbbbbbbbbbbbbeeee", 13));
}

/********************* 1550 ***********************/

typedef struct { char a; char b:4; } S_1550;
S_1550 s_1550 = {1, 2};

typedef struct { char a; int b:4; } S_1550_1;
S_1550_1 s_1550_1 = {1, 2};

void t_1550(void) {
  EQI(s_1550.a, 1);
  EQI(s_1550.b, 2);
  EQI(s_1550_1.a, 1);
  EQI(s_1550_1.b, 2);
}

/********************* 1556 ***********************/

typedef struct { char a:4; char b; } S_1556;
S_1556 s_1556 = {1, 2};

typedef struct { int a:4; char b; } S_1556_1;
S_1556_1 s_1556_1 = {1, 2};

void t_1556(void) {
  EQI(s_1556.a, 1);
  EQI(s_1556.b, 2);
  EQI(s_1556_1.a, 1);
  EQI(s_1556_1.b, 2);
}

/********************* 1681 ***********************/

int i_1681;

void f_1681(void) {
  while (i_1681 == 0) { continue; }
}

void t_1681() {
  i_1681 = 1;
  f_1681();
  EQI(i_1681, 1);
}

/********************* 1755 ***********************/

typedef int F_1755(void);
int f1_1755(void) { return 1; }
int f2_1755(void) { return 2; }

int f_1755(F_1755 **fp) {
  F_1755 *f = *fp;
  *fp = f2_1755;
  return f();
}

void t_1755(void) {
  F_1755 *f = f1_1755;
  EQI(f_1755(&f), 1);
  EQI(f(), 2);
}

/********************* 1780 ***********************/

typedef struct { double mt[2]; double t; } t1_1780;
typedef struct { unsigned b; } t2_1780;
typedef struct { float value; } t3_1780;
typedef struct { unsigned b; } t4_1780;

extern t1_1780 v1_1780;
extern t2_1780 v2_1780;
extern t3_1780 v3_1780;
extern t4_1780 v4_1780;
extern int count_1780;
extern void f2_1780(void);

void f0_1780(int i) { }

float f1_1780(void)
{
  float f, g;
  f = v1_1780.mt[0] - v1_1780.mt[1];
  if (!f || v3_1780.value || v2_1780.b || v4_1780.b) {
    v1_1780.t = v1_1780.mt[0];
    return 1;
  }
  if (f > 0.1) {
    v1_1780.mt[1] = v1_1780.mt[0] - 0.1;
    f = 0.1;
  }
  g = (v1_1780.t - v1_1780.mt[1]) / f;
  if (g < 0) {
    if (g < -0.01) {
      f0_1780(1);
      v1_1780.t = v1_1780.mt[1];
    }
    g = 0;
  } else if (g > 1) {
    if (g > 1.01) {
      f0_1780(2);
      v1_1780.t = v1_1780.mt[0];
    }
    g = 1;
  }
  f0_1780(0);
  if (count_1780++ != 0) f2_1780();
  return g;
}

static jmp_buf exitbuf_1780;
int count_1780;
t1_1780 v1_1780;
t2_1780 v2_1780;
t3_1780 v3_1780;
t4_1780 v4_1780;

void f2_1780() { EQI(1, 0); longjmp(exitbuf_1780, 1); }

void t_1780() {
  count_1780 = 0;
  v2_1780.b = v4_1780.b = 0;
  v3_1780.value = 0;
  v1_1780.mt[0] = 1.1; v1_1780.mt[1] = 1.0; v1_1780.t = 1.0;
  if (setjmp(exitbuf_1780) == 0) f1_1780();
}

/********************* 1782 ***********************/

int t0_1782(int a, int b)
{
    if (a / b == 2) return 1;  /* returns after one recursive call */
    if (++b > a) return 0;     /* faulty code exits here */
    return t0_1782(a / 3, b);
}

void t_1782(void)
{
    EQI(t0_1782(12,1), 1);
}

/********************* 2125 ***********************/

/* must be compiled with /hardfp/nofp */

void t0_2125(int *p, int *q);
void t1_2125(double);

double t2_2125(int a, int b)
{
   double c = 3.1;
   t0_2125(&a, &b);
   return c;
}

void t0_2125(int *p, int *q)
{   }

double dval(double x)
{
    return x;
}

void t_2125(void)
{
    double x = dval(1.3);
    double y = dval(2.3);
    EQI(t2_2125(1, 2), 3);
    EQI((int)x, 1);
    EQI((int)y, 2);
}


/********************* 2292 ***********************/

struct { int x : 32; } X_2290 = 1;

void t_2290(void)
{
    EQI(X_2290.x, 1);
}

/********************* 2292 ***********************/

int s0_2292[200];
int s1_2292[200];

void t0_2292(int* in,int* out,int in_len,int* coef,int coef_len,int scale)
{
    EQI((int)in, (int)s0_2292);
    EQI(in_len, 700);
    EQI((int)coef, (int)s1_2292);
    EQI(coef_len, 35);
    EQI(scale, 285);
}

void t1_2292(int **a, int **b)
{
    EQI((int)*a, (int)s0_2292);
    EQI((int)*b, (int)s1_2292);
}

void t_2292()
{   int *input0 = s0_2292;
    int *input1 = s1_2292;
    int output[720];

    t1_2292(&input0, &input1);
    t0_2292(input0,output,700,input1,35,285);
}

/********************* 2311 ***********************/

typedef int (*F_2311)(int, int, int);

int f_2311(int a, int b, int c) { return a+b+c; }

int g_2311(void) { return 2; }

F_2311 ff_2311(void) { return f_2311; }

void t_2311(void) {
    EQI(ff_2311()(1, g_2311(), g_2311()), 5);
}

/********************* 2516 ***********************/

void t_2516(void) {
  char buf[sizeof(double)];
  double obj = 1.234;
  memcpy(buf, &obj, sizeof(double));
  obj = 999.;
  memcpy(&obj, buf, sizeof(double));
  EQD(obj, 1.234);
}

/********************* main ***********************/

int main() {
  BeginTest();
  t_xxx();
  t_536();
  t_591();
  t_593();
  t_628();
  t_639();
  t_678();
  t_683();
  t_701();
  t_724();
  t_725();
  t_727();
  t_742();
  t_770();
  t_776();
  t_778();
  t_825();
  t_919();
  t_922();
  t_1049();
  t_1075();
  t_1152();
  t_1193();
  t_1280();
  t_1281();
  t_1282();
  t_1360();
  t_1404();
  t_1550();
  t_1556();
  t_1681();
  t_1755();
  t_1780();
  t_1782();
  t_2125();
  t_2290();
  t_2292();
  t_2311();
  t_2516();
  EndTest();
  return 0;
}
