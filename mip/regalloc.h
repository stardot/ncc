/*
 * mip/regalloc.h
 * Copyright (C) Acorn Computers Ltd., 1988.
 * Copyright (C) Codemist Ltd., 1988.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _regalloc_h
#define _regalloc_h

#ifndef _defs_LOADED
#  include "defs.h"
#endif
#ifndef _cgdefs_LOADED
#  include "cgdefs.h"
#endif

extern VRegnum vregister(RegSort type);
extern RegSort vregsort(VRegnum r);
extern void allocate_registers(BindList *spill_order);

extern RealRegSet regmaskvec;
/*
 * Note that old back-ends will have used a variable regmask which can
 * now be re-written as (regmaskvec.map)[0], but only if there are at most
 * 32 registers to be so mapped.
 */
#if (NMAGICREGS <= 32)
   /* AM never wants to chase bugs of regmask being used if MAGICREGS>32. */
#  define regmask ((regmaskvec.map)[0])       /* backwards compatibility. */
#endif

extern RealRegister register_number(VRegnum a);
extern void globalregistervariable(VRegnum r);
extern void note_slave(VRegnum slave, VRegnum master);
extern void forget_slave(VRegnum slave, VRegnum master);

typedef union {
    char **s;
    VRegSetP vr;
    VRegnum r;
} RealRegSet_MapArg;

typedef void RealRegSet_MapFn(RealRegister r, RealRegSet_MapArg *arg);

extern void augment_RealRegSet(RealRegSet *, unsigned32);
extern bool member_RealRegSet(RealRegSet const *, unsigned32);
extern unsigned32 delete_RealRegSet(RealRegSet *, unsigned32);
extern bool intersect_RealRegSet(RealRegSet *a, RealRegSet const *b, RealRegSet const *c);
extern void union_RealRegSet(RealRegSet *a, RealRegSet const *b, RealRegSet const *c);
extern void difference_RealRegSet(RealRegSet *a, const RealRegSet *b, const RealRegSet *c);
extern void map_RealRegSet(RealRegSet const *a, RealRegSet_MapFn *f, RealRegSet_MapArg *arg);
extern void print_RealRegSet(RealRegSet const *a);

RealRegSet const *globalregset(void);
void avoidallocating(VRegnum); /* modifications to ALLOCATION_ORDER */

extern void regalloc_init(void);
extern void regalloc_reinit(void);
extern void regalloc_tidy(void);

#endif

/* end of regalloc.h */
