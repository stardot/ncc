/*
 * ARM C compiler regression test $RCSfile$
 * Copyright (C) 1997 Advanced Risc Machines Ltd. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <stdlib.h>
#include "testutil.h"

typedef unsigned int UINT32;
typedef unsigned char UINT8;

typedef struct {
   UINT32 parg0_save;
   UINT32 parg1_save;
   UINT32 parg2_save;
   UINT32 parg3_save;
   UINT32 parg4_save;
   UINT32 farg0_save;
   UINT32 farg1_save;
   UINT32 farg2_save;
   UINT32 farg3_save;
   UINT32 farg4_save;
   UINT32 bk_save;
   UINT32 s0_save;
   UINT32 s1_save;
   UINT32 s2_save;
   UINT32 s3_save;
   UINT32 s4_save;
   UINT32 pd_save;
   UINT32 sp_save;
   UINT32 fp_save;
   UINT32 lk_save;
   UINT32 st_save;
   UINT32 pc_save;
   UINT32 tc_save;
   UINT32 tb_save;
   UINT32 sb_save;
} ABrisc;


typedef struct tcb {
   UINT32 status;
   struct tcb *next;
   struct tcb *prev;
   UINT32 irqmask_save;
   UINT32 tid;
   UINT32 pri;
   UINT32 or_mask;
   UINT32 and_mask;
   UINT8 *mailbox;
   struct tcb *next_delay;
   struct tcb *prev_delay;
   UINT32 exp_time;
   UINT32 *errp;
   UINT32 retValue;
   struct llh *plist;
   UINT32 newPri;
   UINT32 a1_save;
   UINT32 a2_save;
   UINT32 a3_save;
   UINT32 a4_save;
   UINT32 v1_save;
   UINT32 v2_save;
   UINT32 v3_save;
   UINT32 v4_save;
   UINT32 v5_save;
   UINT32 *sb_save;
   UINT32 *sl_save;
   UINT32 *fp_save;
   UINT32 *ip_save;
   UINT32 *sp_save_usr;
   void (*lr_save_usr) ();
   UINT32 spsr_save;
   void (*lr_save) ();
   UINT32 *stack_end;
   UINT32 timeslice;
   UINT32 *userTask;
   void (*onReady) (struct tcb *ptcb);
   void (*onFault) (struct tcb * ptcb, UINT32 faultCode);
   void (*onDelete) (struct tcb *ptcb);
   UINT32 resumeTime;
   UINT32 runTime;
   ABrisc ABrisc_save;          /* Defined above */
   UINT32 *trendPtr;
} TCB;

typedef struct llh {
   union {
      UINT32 status;
      UINT32 evFlag;
      UINT32 pendOpt;
   } u;
   TCB *next;
   TCB *prev;
   TCB *tail;
} LLH;


TCB *os_tcbList[256];
TCB *os_curTCB;

#define HW_GLOBAL_IRQ_EN        (1 << 0x00)
#define HW_GLOBAL_FIQ_EN        (1 << 0x01)

void OS_tCreate(
                 void (*task) (),
                 UINT32 tid,
                 UINT32 pri,
                 UINT32 *tStack,
                 UINT32 stkLength,
                 TCB *ptcb,
                 UINT32 *errp
)
{
   *errp = 0;

   if (pri >= 32) {
      *errp = 0x0013;
   }
   else if ((tid != 0) && (tid < 256) && (os_tcbList[tid] == 0)) {
      os_tcbList[tid] = ptcb;
      ptcb->tid = tid;
      ptcb->pri = pri;

      ptcb->newPri = 0;

      ptcb->status = 0;
      ptcb->next = 0;
      ptcb->prev = 0;
      ptcb->or_mask = 0;
      ptcb->and_mask = 0;
      ptcb->mailbox = 0;
      ptcb->next_delay = 0;
      ptcb->prev_delay = 0;
      ptcb->exp_time = 0;
      ptcb->errp = 0;
      ptcb->retValue = 0;
      ptcb->plist = 0;
      ptcb->plist = 0;
      ptcb->userTask = 0;
      ptcb->onReady = 0;
      ptcb->onFault = 0;
      ptcb->irqmask_save = HW_GLOBAL_IRQ_EN | HW_GLOBAL_FIQ_EN;
      ptcb->sp_save_usr = tStack;
      ptcb->sl_save = (UINT32 *) ((UINT32) tStack - stkLength * 4 + 256);
      ptcb->stack_end = (UINT32 *) ((UINT32) tStack - stkLength * 4);
      *ptcb->stack_end = 0xAB;
      ptcb->timeslice = 0;
      ptcb->lr_save = task;
      ptcb->spsr_save = 0x10;
      ptcb->lr_save_usr = 0;
      ptcb->fp_save = 0;
      ptcb->ABrisc_save.st_save = 0x0f000001;
      ptcb->ABrisc_save.tc_save = 0;

   } else {
      *errp = 1;
   }
   return;
}


#define EQP(a, b) EQI((int)a, (int)b)
void t1()
{

    /* Initializing the struct */
    int i=0;
    UINT32 tStack=0xa000;
    UINT32 errp;

    TCB tcb;
    char *buffer = (char *)&tcb;

    for (i=0; i<sizeof(TCB); ++i)
        buffer[i]=(unsigned char)(i%256);

    os_tcbList[0x10]=0;

    OS_tCreate(0,
               0x00000010,      /* tid */
               0x00000001,      /* pri */
               &tStack,         /* tStack */
               0x000000ff,      /* stkLength */
               &tcb,
               &errp);

    EQP(errp, 0);
    EQI(tcb.tid, 0x10);
    EQI(tcb.pri, 0x1);
    EQP(tcb.newPri, 0);
    EQI(tcb.status, 0);
    EQP(tcb.next, 0);
    EQP(tcb.prev, 0);
    EQI(tcb.or_mask, 0);
    EQI(tcb.and_mask, 0);
    EQP(tcb.mailbox, 0);
    EQP(tcb.next_delay, 0);
    EQP(tcb.prev_delay, 0);
    EQI(tcb.exp_time, 0);
    EQP(tcb.errp, 0);
    EQI(tcb.retValue, 0);
    EQP(tcb.plist, 0);
    EQP(tcb.userTask, 0);
    EQP(tcb.onReady, 0);
    EQP(tcb.onFault, 0);
    EQI(tcb.irqmask_save, 3);
    EQP(tcb.sp_save_usr, &tStack);
    EQP(*tcb.stack_end, 0xab);
    EQI(tcb.timeslice, 0);
    EQP(tcb.lr_save, 0);
    EQI(tcb.spsr_save, 0x10);
    EQP(tcb.lr_save_usr, 0);
    EQP(tcb.fp_save, 0);
    EQI(tcb.ABrisc_save.st_save, 0x0f000001);
    EQI(tcb.ABrisc_save.tc_save, 0);
}

int new_calls = 0;
size_t requested_size = 0;
void *allocated_block = 0;
void *ptr_arg = 0;
size_t extra_size_arg = 0;

void *New(size_t n, void *heap, size_t heap_size) {
    ++new_calls;
    requested_size = n;
    ptr_arg = heap;
    extra_size_arg = heap_size;
    return allocated_block = (n <= heap_size ? heap : 0);
}

void t2() {
    int x;
    void *res = New(100, &x, 200);
    EQI(new_calls, 1);
    EQI(requested_size, 100);
    EQP(ptr_arg, &x);
    EQI(extra_size_arg, 200);
    EQP(allocated_block, &x);
    EQP(res, &x);
}

int main() {
    t1();
    t2();
    return 0;
}

