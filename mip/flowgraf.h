/*
 * mip/flowgraf.h
 * Copyright (C) Acorn Computers Ltd., 1988.
 * Copyright (C)  Codemist Ltd., 1988.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _flowgraf_LOADED
#define _flowgraf_LOADED 1

#ifndef _defs_LOADED
#  include "defs.h"
#endif
#ifndef _cgdefs_LOADED
#  include "cgdefs.h"
#endif
#ifndef _jopcode_LOADED
#  include "jopcode.h"
#endif

extern BlockHead *top_block, *bottom_block;   /* exported to cg/regalloc */

extern struct CGState {
  /* Shared between flowgraf and cg, used during jopcode emission only */
    BindListList *curenv;
    BindList *active;

  /* statistics */
    int32 icode_cur, block_cur;

    struct {
        Binder *var;
        VRegnum reg;
    } juststored;
} cgstate;

#define current_env cgstate.curenv
#define active_binders cgstate.active

extern int32 sizeofbinders(BindList *l, bool countall);

extern void finishblock(void);

extern void end_emit(void);

extern Icode *newicodeblock(int32 size);

extern void freeicodeblock(Icode *p, int32 size);

extern void reopen_block(BlockHead *p);

#define start_new_basic_block(l) \
    start_basic_block_at_level(l, active_binders)

extern BlockHead *insertblockbetween(BlockHead *before, BlockHead *after, bool insertingraph);
/* make a new block, and insert it between blocks  before  and  after
   in the blockup_/down_ sense. If insertingraph is true, also insert
   it between before and after in the blknext_ sense.
   (error if there is no arc from before to after).
 */

extern void changesuccessors(BlockHead *b,LabelNumber *newl,LabelNumber *oldl);
/* change all arcs from b whose destination is  oldl  to have destination
   newl  instead.
 */

extern BlockHead *start_basic_block_at_level(LabelNumber *l,
                                             BindList *active_on_entry);

extern bool is_exit_label(LabelNumber *ll);
       /* exported to jopprint.c (lab_xname_); */

extern void emitfl(J_OPCODE op, FileLine fl);

extern void emitshift(J_OPCODE op, VRegnum r1, VRegnum r2, VRegnum r3, int32 m);

extern void emitstring(J_OPCODE op, VRegnum r1, StringSegList *m);

extern void emitbranch(J_OPCODE op, LabelNumber *m);

extern void emitbinder(J_OPCODE op, VRegnum r1, Binder *m);

extern void emitvk(J_OPCODE op, VRegnum r1, int32 n, Binder *m);

extern void emitreg(J_OPCODE op, VRegnum r1, VRegnum r2, VRegnum m);

extern void emitreg4(J_OPCODE op, int flags, VRegnum r1, VRegnum r2, VRegnum r3, VRegnum r4);

extern void emitfloat(J_OPCODE op, VRegnum r1, VRegnum r2, FloatCon *m);

extern void emitint64(J_OPCODE op, VRegnum r1, VRegnum r2, Int64Con *m);

extern void emitsetsp(J_OPCODE op, BindList *b2);

extern void emitsetspandjump(BindList *b2, LabelNumber *l);

extern void emitcall(J_OPCODE op, VRegnum r1, int32 argwords, Binder *m);

extern void emitcallreg(J_OPCODE op, VRegnum r1, int32 argwords, VRegnum m);

extern void emitsetspgoto(BindList *r2, LabelNumber *m);

extern void emitsetspenv(BindList *r2, BindList *m);

extern void emitcasebranch(J_OPCODE op, VRegnum r1, LabelNumber **r2, int32 m);

extern void emit(J_OPCODE op, VRegnum r1, VRegnum r2, int32 m);

extern void emitic(const Icode *const ic);

extern bool is_compare(J_OPCODE op);

extern void lose_dead_code(void);

extern void linearize_code(void);

extern void flowgraph_reinit(void);

#endif

/* end of mip/flowgraf.h */
