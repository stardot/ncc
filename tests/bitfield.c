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

typedef unsigned long uint32;
typedef unsigned short uint16;
typedef unsigned char uint8;


typedef struct {
  uint32 queue1;
  uint16 queue2;
  uint8 grp_evt;
  uint8 type : 7;
  uint8 ou_et : 1;
} S_2275;

const S_2275 s_2275[2] = {
  {0,4321,0,0x7f,1},
  {0,0,0,1,1}
};

static void t_2275(void) {
  uint16 val160 = 1234;
  uint16 val161 = s_2275[0].queue2;
  uint8 val80 = s_2275[0].type;
  uint8 val81 = s_2275[0].ou_et;
  EQI(val80, 0x7f);
  EQI(val81, 1);
}

typedef struct {
  uint32 a;
  uint16 b;
  uint8  c;
  uint32 d:32;
  uint32 e:32;
  uint32 f:31;
  uint32 g:1;
  uint8  h:8;
  uint8  i:8;
  uint8  j:8;
  uint8  k:8;
  uint8  l:8;
} S_2290;

S_2290 s_2290 = {
  0x11223344, 0x5566, 0x77, 0x8899aabb, 0xccddeeff,
  0x7f112233, 1, 2, 3, 4, 5, 6
};

void t_2290(void) {
  EQI(s_2290.a, 0x11223344);
  EQI(s_2290.b, 0x5566);
  EQI(s_2290.c, 0x77);
  EQI(s_2290.d, 0x8899aabb);
  EQI(s_2290.e, 0xccddeeff);
  EQI(s_2290.f, 0x7f112233);
  EQI(s_2290.g, 1);
  EQI(s_2290.h, 2);
  EQI(s_2290.i, 3);
  EQI(s_2290.j, 4);
  EQI(s_2290.k, 5);
  EQI(s_2290.l, 6);
  s_2290.d = 0xbbaa9988;
  s_2290.l = 3;
  EQI(s_2290.a, 0x11223344);
  EQI(s_2290.b, 0x5566);
  EQI(s_2290.c, 0x77);
  EQI(s_2290.d, 0xbbaa9988);
  EQI(s_2290.e, 0xccddeeff);
  EQI(s_2290.f, 0x7f112233);
  EQI(s_2290.g, 1);
  EQI(s_2290.h, 2);
  EQI(s_2290.i, 3);
  EQI(s_2290.j, 4);
  EQI(s_2290.k, 5);
  EQI(s_2290.l, 3);
}

int main(void) {
  BeginTest();
  t_2275();
  t_2290();
  EndTest();
  return 0;
}

/* End of $RCSfile$ */
