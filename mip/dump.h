/*
 * dump.h: compiler state save and restore
 * Copyright (C) 1996 Advanced RISC Machines Limited. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$  Codemist 0
 * Checkin $Date$
 * Revising $Author$
 */

#define DS_Version 1

#define DS_Dump 1
#define DS_Load 2
extern unsigned dump_state;

typedef struct {
  uint32 ngensym, nsym,
         nglobbind,
         nglobtag,
         nbindlist;
} Dump_LoadState;

extern Dump_LoadState dump_loadstate;

typedef enum {
  Dump_Dump,
  Dump_Load
} Dump_Sort;

#ifndef NO_DUMP_STATE
uint32 Dump_BinderRef(Binder *);
uint32 Dump_TagRef(TagBinder *);
uint32 Dump_TagOrBinderRef(Binder *);
uint32 Dump_SymRef(Symstr *);
uint32 Dump_NoteSharedBindList(BindList *);

Binder *Dump_LoadedBinder(uint32);
TagBinder *Dump_LoadedTag(uint32);
Binder *Dump_LoadedTagOrBinder(uint32);
Symstr *Dump_LoadedSym(uint32);
BindList *Dump_LoadedSharedBindList(uint32);

void Dump_Sym(Symstr *, FILE *);
Symstr *Dump_LoadSym(size_t, FILE *);

void Dump_BindList(BindList *, FILE *);
BindList *Dump_LoadBindList(FILE *);
  /* (For unshared bindlists) */

void Dump_Friends(Friend *, FILE *);
Friend *Dump_LoadFriends(FILE *);

void Dump_StrSeg(StringSegList *, FILE *);
StringSegList *Dump_LoadStrSeg(FILE *);

void Dump_Expr(Expr *e, FILE *f);
Expr *Dump_LoadExpr(FILE *f);

void Dump_Type(TypeExpr *, FILE *);
TypeExpr *Dump_LoadType(FILE *);

void Dump_LoadString(char *, size_t, FILE *);

void Dump_SharedBindLists(FILE *);

void Dump_Init(Dump_Sort, FILE *);

void Inline_LoadState(FILE *);
void Inline_DumpState(FILE *);

void Bind_LoadState(FILE *);
void Bind_DumpState(FILE *);

void PP_LoadState(FILE *);
void PP_DumpState(FILE *);

void Builtin_LoadState(FILE *f);
void Builtin_DumpState(FILE *f);

void Vargen_LoadState(FILE *f);
void Vargen_DumpState(FILE *f);

#else
#define Dump_BinderRef(a)               ((uint32)0)
#define Dump_TagReg(a)                  ((uint32)0)
#define Dump_SymRef(a)                  ((uint32)0)
#define Dump_SharedBindListRef(a)       ((uint32)0)
#define Dump_LoadedBinder(a)            ((Binder *)0)
#define Dump_LoadedTag(a)               ((TagBinder *)0)
#define Dump_LoadedSym(a)               ((Symstr *)0)
#define Dump_LoadedSharedBindList(a)    ((BindList *)0)
#define Dump_Sym(a,b)                   ((void)0)
#define Dump_LoadSym(a,b)               ((Symstr *)0)
#define Dump_BindList(a,b)              ((void)0)
#define Dump_LoadBindList(a)            ((BindList *)0)
#define Dump_StrSeg(a,b)                ((void)0)
#define Dump_LoadStrSeg(a)              ((StringSegList *)0)
#define Dump_Expr(e,f)                  ((void)0)
#define Dump_LoadExpr(f)                ((Expr *)0)
#define Dump_Type(a,b)                  ((void)0)
#define Dump_LoadType(a)                ((TypeExpr *)0)
#define Dump_LoadString(a,b,c)          ((void)0)
#define Dump_SharedBindLists(a)         ((void)0)
#define Dump_NoteSharedBindList(a)      ((void)0)

#define Dump_Init(a,b)                  ((void)0)
#define Inline_LoadState(a)             ((void)0)
#define Inline_DumpState(a)             ((void)0)
#define Bind_LoadState(a)               ((void)0)
#define Bind_DumpState(a)               ((void)0)
#define PP_LoadState(a)                 ((void)0)
#define PP_DumpState(a)                 ((void)0)
#define Builtin_LoadState(f)            ((void)0)
#define Builtin_DumpState(f)            ((void)0)
#endif /* NO_DUMP_STATE */
