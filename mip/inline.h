/*
 * inline.h: inline function expansion
 * Copyright (C) Advanced Risc Machines Ltd., 1993
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _inline_h
#define _inline_h

#ifndef _cgdefs_h
#  include "cgdefs.h"
#endif
#ifndef _jopcode_h
#  include "jopcode.h"
#endif

typedef struct Inline_ArgBinderList Inline_ArgBinderList;
struct Inline_ArgBinderList {
  Inline_ArgBinderList *cdr;
  Binder *globarg,
         *globnarrowarg,
         *instantiatednarrowarg; /* valid only after Inline_Restore */
  TypeExpr *narrowtype;          /* (globalised) */
};

typedef struct Inline_SavedFn {
  struct CurrentFnDetails fndetails;
  BlockHead *top_block, *bottom_block;
  BindList *var_binders, *reg_binders;
  Inline_ArgBinderList *args;
  /* The next two fields are to handle inlining of functions with narrow
   * arguments (passed widened, and narrowed on function entry). It doesn't
   * make much sense to do this when the function is inlined: instead, the
   * argument value is stored directly into the narrowed binder.
   * firstblockignore is a bitmap of the instructions in the first real block
   * of the function which handle the narrowing (and are to be omitted in the
   * inline copy). ix_narrowspenv is index in this block of the J_SETSPENV
   * adding the narrow binders (which must be moved before the argument
   * assignments.
   * firstblockignore will be null for functions without narrow arguments.
   */
  uint32 firstblockignoresize;
  uint8 *firstblockignore;
  int32 ix_narrowspenv;
  uint32 ix_max;
  bool firstblockmergeable,
       lastblockmergeable;
} Inline_SavedFn;

typedef enum {
  T_Binder,
  T_AdconV,
  T_Adcon,
  T_Int,
  T_Plus
} Inline_ArgSubstSort;

typedef struct Inline_ArgSubstList Inline_ArgSubstList;
struct Inline_ArgSubstList {
  Inline_ArgSubstList *cdr;
  Binder *arg;
  union { Binder *b; Expr *ex; } replacement;
  Expr *rest;
  Inline_ArgSubstSort sort;
  int32 size;   /* a MEM_xx value (or MEM_NONE) */
  bool refsleft;
  bool notnull;
};

#define MEM_NONE 255

typedef struct {
  LabelNumber *exitlabel;
  BindList *env;
  int nresults;
  VRegnum resultregs[NARGREGS],
          newresultregs[NARGREGS];
  Inline_ArgSubstList *argreplace;
} Inline_RestoreControl;

Inline_SavedFn *Inline_FindFn(Binder *b);

bool Inline_Save(Binder *b, BindList *local_binders, BindList *regvar_binders);

#if defined(CALLABLE_COMPILER)
#define Inline_RealUse(b)               ((void)0)
#else
void Inline_RealUse(Binder *b);
#endif

void Inline_Restore(Inline_SavedFn *p, Inline_RestoreControl *rc);

void Inline_Init(void);
void Inline_Tidy(void);

#endif
