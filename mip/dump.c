/*
 * dump.c: compiler state save and restore
 * Copyright (C) 1996 Advanced RISC Machines Limited. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$  Codemist 0
 * Checkin $Date$
 * Revising $Author$
 */

#include <string.h>
#include <stdio.h>

#include "globals.h"
#include "defs.h"
#include "builtin.h"
#include "store.h"
#include "aeops.h"
#include "sem.h"     /* isbitfield_type */
#include "errors.h"
#include "dump.h"

unsigned dump_state;

Dump_LoadState dump_loadstate;

#define TE_BASICMAX 11
static TypeExpr *te_basic[TE_BASICMAX+1];

static void TE_Basic_Init(void) {
  te_basic[0] = NULL;
  te_basic[1] = te_void;
  te_basic[2] = te_int;
  te_basic[3] = te_lint;
  te_basic[4] = te_uint;
  te_basic[5] = te_ulint;
  te_basic[6] = te_float;
  te_basic[7] = te_double;
  te_basic[8] = te_ldble;
  te_basic[9] = te_char;
  te_basic[10] = te_ushort;
  te_basic[11] = te_boolean;
}

uint32 Dump_BinderRef(Binder *b) {
  if (b == NULL) return 0;
  if ((IPtr)bindparent_(b) == 0) syserr("Dump_BinderRef %p", b);
  return (uint32)(IPtr)bindparent_(b);
}

uint32 Dump_TagRef(TagBinder *b) {
  if (b == NULL) return 0;
  if ((IPtr)tagbindparent_(b) == 0) syserr("Dump_TagRef %p", b);
  return (uint32)(IPtr)tagbindparent_(b);
}

uint32 Dump_TagOrBinderRef(Binder *b) {
  return
    b == NULL
      ? 0
      : (h0_(b) == s_tagbind)
        ? Dump_TagRef((TagBinder *)b) | 0x80000000
        : Dump_BinderRef(b);
}

uint32 Dump_SymRef(Symstr *sym) {
  if (sym == NULL) return 0;
  if ((IPtr)symlab_(sym) == 0) syserr("Dump_SymRef %p", sym);
  return (uint32)(IPtr)symlab_(sym);
}

#define BINDLISTV_SEGSIZE 256
#define BINDLISTV_MAXSEGS 256
static BindList ***bindlists;
static uint32 nbindlistmax;

void Dump_SharedBindLists(FILE *f) {
  uint32 i, n = dump_loadstate.nbindlist;
  uint32 w[2];
  for (i = 1; i < n; i++) {
    uint32 segno = i / BINDLISTV_SEGSIZE,
           segix = i % BINDLISTV_SEGSIZE;
    BindList *bl = bindlists[segno][segix];
    w[0] = Dump_NoteSharedBindList(bl->bindlistcdr);
    w[1] = Dump_BinderRef(bl->bindlistcar);
    fwrite(w, sizeof(uint32), 2, f);
  }
}

uint32 Dump_NoteSharedBindList(BindList *bl) {
  uint32 firsti = 0, i, segno, segix;
  uint32 n = dump_loadstate.nbindlist;
  for (; bl != NULL; bl = bl->bindlistcdr) {
    for (i = 1; i < n; i++) {
      segno = i / BINDLISTV_SEGSIZE;
      segix = i % BINDLISTV_SEGSIZE;
      if (bindlists[segno][segix] == bl) {
        if (firsti == 0) firsti = i;
        dump_loadstate.nbindlist = n;
        return firsti;
      }
    }
    segno = n / BINDLISTV_SEGSIZE;
    segix = n % BINDLISTV_SEGSIZE;
    if (n >= nbindlistmax) {
      if (BINDLISTV_MAXSEGS <= segno)
        syserr("Too many shared bindlists to dump");
      bindlists[segno] = (BindList **)SynAlloc(sizeof(BindList *) * BINDLISTV_SEGSIZE);
      nbindlistmax += BINDLISTV_SEGSIZE;
    }
    bindlists[segno][segix] = bl;
    if (firsti == 0) firsti = n;
    ++n;
  }
  dump_loadstate.nbindlist = n;
  return firsti;
}

void Dump_Sym(Symstr *sym, FILE *f) {
  union { uint16 h[2]; uint32 w[3]; } x;
  size_t len = strlen(symname_(sym));
  x.h[0] = (uint16)len;
  x.h[1] = (uint16)symtype_(sym);
  fwrite(x.h, sizeof(uint16), 2, f);
  x.w[0] = Dump_BinderRef(bind_global_(sym));
  x.w[1] = Dump_TagRef(tag_global_(sym));
  x.w[2] = Dump_SymRef(symfold_(sym));
  fwrite(x.w, sizeof(uint32), 3, f);
  fwrite(symname_(sym), 1, len, f);
}

void Dump_BindList(BindList *bl, FILE *f) {
  /* (For unshared bindlists) */
  uint32 w;
  for (; bl != NULL; bl = bl->bindlistcdr) {
    w = Dump_BinderRef(bl->bindlistcar);
    fwrite(&w, sizeof(uint32), 1, f);
  }
  w = 0;
  fwrite(&w, sizeof(uint32), 1, f);
}

void Dump_Friends(Friend *fr, FILE *f) {
  /* /* this is nearly the same as Dump_BindList */
  uint32 w;
  for (; fr != NULL; fr = fr->friendcdr) {
    w = Dump_TagOrBinderRef(fr->u.friendfn);
    fwrite(&w, sizeof(uint32), 1, f);
  }
  w = 0;
  fwrite(&w, sizeof(uint32), 1, f);
}

void Dump_StrSeg(StringSegList *s, FILE *f) {
  uint32 w = (s == NULL) ? 0 : s->strseglen;
  fwrite(&w, sizeof(uint32), 1, f);
  if (s != NULL) {
    fwrite(s->strsegbase, 1, (size_t)s->strseglen, f);
    Dump_StrSeg(s->strsegcdr, f);
  }
}

static void DumpExprList(ExprList *e, FILE *f) {
  for (; e != NULL; e = cdr_(e))
    Dump_Expr(exprcar_(e), f);
  Dump_Expr(NULL, f);
}

void Dump_Expr(Expr *e, FILE *f) {
  union { uint16 h[2]; uint32 w[5]; } x;
  AEop op;
  if (e == NULL) {
    x.w[0] = 0;
    fwrite(x.w, sizeof(uint32), 1, f);
    return;
  }
  x.w[0] = op = h0_(e);
  switch (op) {
  case s_floatcon:
    x.w[1] = ((FloatCon *)e)->floatlen;
    x.w[2] = ((FloatCon *)e)->floatbin.irep[0];
    x.w[3] = ((FloatCon *)e)->floatbin.irep[1];
    x.w[4] = strlen(((FloatCon *)e)->floatstr);
    fwrite(x.w, sizeof(uint32), 5, f);
    fwrite(((FloatCon *)e)->floatstr, 1, (size_t)x.w[4], f);
    return;

#ifdef EXTENSION_UNSIGNED_STRINGS
  case s_ustring:
#endif
  case s_wstring:
  case s_string:
    fwrite(x.w, sizeof(uint32), 1, f);
    Dump_StrSeg(((String *)e)->strseg, f);
    return;

  case s_fnapstruct:
  case s_fnap:
    fwrite(x.w, sizeof(uint32), 1, f);
    Dump_Type(type_(e), f);
    Dump_Expr(arg1_(e), f);
    DumpExprList(exprfnargs_(e), f);
    return;

  case s_let:
    fwrite(x.w, sizeof(uint32), 1, f);
    Dump_Type(type_(e), f);
    Dump_BindList((BindList *)exprletbind_(e), f);
    Dump_Expr(arg2_(e), f);
    return;

  case s_binder:
    x.w[1] = Dump_BinderRef((Binder *)e);
    fwrite(x.w, sizeof(uint32), 2, f);
    return;

  case s_integer:
    x.w[1] = intval_(e);
    fwrite(x.w, sizeof(uint32), 2, f);
    Dump_Type(type_(e), f);
    return;

  case s_cond:
    fwrite(x.w, sizeof(uint32), 1, f);
    Dump_Type(type_(e), f);
    Dump_Expr(arg1_(e), f);
    Dump_Expr(arg2_(e), f);
    Dump_Expr(arg3_(e), f);
    return;

  case s_qualdot:
  case s_dot:
    fwrite(x.w, sizeof(uint32), 1, f);
    Dump_Type(type_(e), f);
    Dump_Expr(arg1_(e), f);
    x.w[0] = exprdotoff_(e);
    if (!isbitfield_type(type_(e))) {
      fwrite(x.w, sizeof(uint32), 1, f);
    } else {
      x.w[1] = exprbsize_(e);
      x.w[2] = exprmsboff_(e);
      fwrite(x.w, sizeof(uint32), 3, f);
    }
    return;

  default:
    fwrite(x.w, sizeof(uint32), 1, f);
    Dump_Type(type_(e), f);
    Dump_Expr(arg1_(e), f);
    if (isdiad_(op) || op == s_init)
      Dump_Expr(arg2_(e), f);
    return;
  }
}

void Dump_Type(TypeExpr *t, FILE *f) {
  union { uint16 h[2]; uint32 w[3]; } x;
  for (;;) {
    if (t == NULL) {
      x.w[0] = 0;
      fwrite(x.w, sizeof(uint32), 1, f);
      return;
    }
    x.w[0] = h0_(t);
    switch (h0_(t)) {
    case t_content:
    case t_ref:
      x.w[1] = typeptrmap_(t);
      fwrite(x.w, sizeof(uint32), 2, f);
      t = typearg_(t);
      break;

    case s_typespec:
      { size_t n = 3;
        uint32 i;
        for (i = 1; i <= TE_BASICMAX; i++)
          if (t == te_basic[i]) {
            x.w[0] = 0-i;
            fwrite(x.w, sizeof(uint32), 1, f);
            return;
          }
        x.w[1] = typespecmap_(t);
        if (typespecmap_(t) & ENUMORCLASSBITS) {
          x.w[2] = Dump_TagRef(typespectagbind_(t));
        } else if (typespecmap_(t) & bitoftype_(s_typedefname)) {
          x.w[2] = Dump_BinderRef(typespecbind_(t));
        } else
          n = 2;
        fwrite(x.w, sizeof(uint32), n, f);
      }
      return;

    case t_subscript:
      fwrite(x.w, sizeof(uint32), 1, f);
      Dump_Expr(typesubsize_(t), f);
      t = typearg_(t);
      break;

    case t_fnap:
      { FormTypeList *ft = typefnargs_(t);
        x.w[1] = length((List *)ft);
        x.w[2] = typeptrmap_(t);
        fwrite(x.w, sizeof(uint32), 3, f);
        for (; ft != NULL; ft = ft->ftcdr) {
          x.w[0] = Dump_SymRef(ft->ftname);
          fwrite(x.w, sizeof(uint32), 1, f);
          Dump_Type(ft->fttype, f);
          Dump_Expr(ft->ftdefault, f);
        }
        fwrite(&typefnaux_(t), sizeof(TypeExprFnAux), 1, f);
        t = typearg_(t);
        break;
      }

    case t_coloncolon:
      x.w[1] = Dump_TagRef(typespectagbind_(t));
      fwrite(x.w, sizeof(uint32), 2, f);
      t = typearg_(t);
      break;

    case t_ovld:
      fwrite(x.w, sizeof(uint32), 1, f);
      Dump_BindList(typeovldlist_(t), f);
      return;

    default:
      fwrite(x.w, sizeof(uint32), 1, f);
      syserr("Dump_Type %ld", (long)h0_(t));
      return;
    }
  }
}

#define INDEX_SEGSIZE 1024

static Symstr ***symindex;
static BindList ***bindlistindex;
static Binder ***binderindex;
static TagBinder ***tagindex;

static uint32 symno;

Binder *Dump_LoadedBinder(uint32 i) {
  uint32 segno = i / INDEX_SEGSIZE,
         segix = i % INDEX_SEGSIZE;
  return binderindex[segno][segix];
}

TagBinder *Dump_LoadedTag(uint32 i) {
  uint32 segno = i / INDEX_SEGSIZE,
         segix = i % INDEX_SEGSIZE;
  return tagindex[segno][segix];
}

Binder *Dump_LoadedTagOrBinder(uint32 i) {
  return (i & 0x80000000) ? (Binder *)Dump_LoadedTag(i & ~0x80000000)
                          : Dump_LoadedBinder(i);
}

Symstr *Dump_LoadedSym(uint32 i) {
  uint32 segno = i / INDEX_SEGSIZE,
         segix = i % INDEX_SEGSIZE;
  return symindex[segno][segix];
}

BindList *Dump_LoadedSharedBindList(uint32 i) {
  uint32 segno = i / INDEX_SEGSIZE,
         segix = i % INDEX_SEGSIZE;
  return bindlistindex[segno][segix];
}

void Dump_LoadString(char *p, size_t n, FILE *f) {
  fread(p, 1, n, f);
  p[n] = 0;
}

BindList *Dump_LoadBindList(FILE *f) {
  BindList *bl;
  BindList **blp = &bl;
  uint32 w;
  for (;;) {
    BindList *p;
    fread(&w, sizeof(uint32), 1, f);
    if (w == 0) break;
    p = (BindList *)GlobAlloc(SU_Bind, sizeof(BindList));
    p->bindlistcar = Dump_LoadedBinder(w);
    *blp = p;
    blp = &p->bindlistcdr;
  }
  *blp = NULL;
  return bl;
}

Friend *Dump_LoadFriends(FILE *f) {
    /* this is nearly the same as Dump_LoadBindList */
  Friend *fr;
  Friend **frp = &fr;
  uint32 w;
  for (;;) {
    Friend *p;
    fread(&w, sizeof(uint32), 1, f);
    if (w == 0) break;
    p = (Friend *)GlobAlloc(SU_Other, sizeof(Friend));
    p->u.friendfn = Dump_LoadedTagOrBinder(w);
    *frp = p;
    frp = &p->friendcdr;
  }
  *frp = NULL;
  return fr;
}

StringSegList *Dump_LoadStrSeg(FILE *f) {
  uint32 len;
  fread(&len, sizeof(uint32), 1, f);
  if (len == 0) return NULL;
  { StringSegList *s = (StringSegList *)GlobAlloc(SU_Inline, sizeof(StringSegList) + len);
    s->strsegbase = (char *)(s+1);
    s->strseglen = len;
    fread(s->strsegbase, 1, (size_t)len, f);
    s->strsegcdr= Dump_LoadStrSeg(f);
    return s;
  }
}

static ExprList *LoadExprList(FILE *f) {
  ExprList *res = NULL;
  for (;;) {
    Expr *e = Dump_LoadExpr(f);
    if (e == NULL)
      return (ExprList *)dreverse((List *)res);
    res = mkExprList(res, e);
  }
}

Expr *Dump_LoadExpr(FILE *f) {
  union { uint16 h[2]; uint32 w[4]; } x;
  AEop op;
  Expr *e;
  fread(x.w, sizeof(uint32), 1, f);
  if (x.w[0] == 0) return NULL;
  op = x.w[0];
  switch (op) {
  case s_floatcon:
    fread(x.w, sizeof(uint32), 4, f);
    { FloatCon *fc = (FloatCon *)GlobAlloc(SU_Other, sizeof(FloatCon) + x.w[3]);
      h0_(fc) = op;
      fc->floatlen = x.w[0];
      fc->floatbin.irep[0] = x.w[1];
      fc->floatbin.irep[1] = x.w[2];
      Dump_LoadString(fc->floatstr, (size_t)x.w[3], f);
      return (Expr *)fc;
    }

#ifdef EXTENSION_UNSIGNED_STRINGS
  case s_ustring:
#endif
  case s_wstring:
  case s_string:
    { String *s = (String *)global_cons2(SU_Other, op, 0);
      s->strseg = Dump_LoadStrSeg(f);
      return (Expr *)s;
    }

  case s_fnapstruct:
  case s_fnap:
    e = (Expr *)GlobAlloc(SU_Other, sizeof(IPtr[5]));
    h0_(e) = op;
    type_(e) = Dump_LoadType(f);
    arg1_(e) = Dump_LoadExpr(f);
    exprfnargs_(e) = LoadExprList(f);
    return e;

  case s_let:
    e = (Expr *)GlobAlloc(SU_Other, sizeof(IPtr[5]));
    h0_(e) = op;
    type_(e) = Dump_LoadType(f);
    exprletbind_(e) = (SynBindList *)Dump_LoadBindList(f);
    arg2_(e) = Dump_LoadExpr(f);
    return e;

  case s_binder:
    fread(x.w, sizeof(uint32), 1, f);
    return (Expr *)Dump_LoadedBinder(x.w[0]);

  case s_integer:
    fread(x.w, sizeof(uint32), 1, f);
    e = (Expr *)global_list5(SU_Other, op, 0, 0, x.w[0], 0);
    type_(e) = Dump_LoadType(f);
    return e;

  case s_cond:
    e = (Expr *)GlobAlloc(SU_Other, sizeof(IPtr[6]));
    h0_(e) = op;
    exprfileline_(e) = NULL;
    type_(e) = Dump_LoadType(f);
    arg1_(e) = Dump_LoadExpr(f);
    arg2_(e) = Dump_LoadExpr(f);
    arg3_(e) = Dump_LoadExpr(f);
    return e;

  case s_dot:
    { TypeExpr *t = Dump_LoadType(f);
      e = (Expr *)GlobAlloc(SU_Other, isbitfield_type(t) ? sizeof(IPtr[7]) : sizeof(IPtr[5]));
      h0_(e) = op;
      type_(e) = t;
    }
    arg1_(e) = Dump_LoadExpr(f);
    if (!isbitfield_type(type_(e))) {
      fread(x.w, sizeof(uint32), 1, f);
      exprdotoff_(e) = x.w[0];
    } else {
      fread(x.w, sizeof(uint32), 3, f);
      exprdotoff_(e) = x.w[0];
      exprbsize_(e) = x.w[1];
      exprmsboff_(e) = x.w[2];
    }
    return e;

  default:
    e = (Expr *)GlobAlloc(SU_Other, (isdiad_(op) || op == s_init) ? sizeof(IPtr[5]) : sizeof(IPtr[4]));
    h0_(e) = op;
    type_(e) = Dump_LoadType(f);
    arg1_(e) = Dump_LoadExpr(f);
    if (isdiad_(op) || op == s_init)
      arg2_(e) = Dump_LoadExpr(f);
    return e;
  }
}

TypeExpr *Dump_LoadType(FILE *f) {
  union { uint16 h[2]; uint32 w[3]; } x;
  TypeExpr *t;
  uint32 op;
  fread(x.w, sizeof(uint32), 1, f);
  op = x.w[0];
  if ((int32)op <= 0) return te_basic[0-op];

  switch (op) {
  case t_content:
  case t_ref:
    fread(x.w, sizeof(uint32), 1, f);
    t = (TypeExpr *)global_list4(SU_Type, op, 0, x.w[0], 0);
    typearg_(t) = Dump_LoadType(f);
    return t;

  case s_typespec:
    fread(x.w, sizeof(uint32), 1, f);
    t = (TypeExpr *)global_list4(SU_Type, op, x.w[0], 0, 0);
    if (typespecmap_(t) & ENUMORCLASSBITS) {
      fread(x.w, sizeof(uint32), 1, f);
      typespecbind_(t) = (Binder *)Dump_LoadedTag(x.w[0]);
    } else if (typespecmap_(t) & bitoftype_(s_typedefname)) {
      fread(x.w, sizeof(uint32), 1, f);
      typespecbind_(t) = Dump_LoadedBinder(x.w[0]);
    }
    return t;

  case t_subscript:
    t = (TypeExpr *)global_list4(SU_Type, op, 0, 0, 0);
    typesubsize_(t) = Dump_LoadExpr(f);
    typearg_(t) = Dump_LoadType(f);
    return t;

   case t_fnap:
    fread(x.w, sizeof(uint32), 2, f);
    t = (TypeExpr *)GlobAlloc(SU_Type, sizeof(TypeExpr));
    h0_(t) = op;
    typedbginfo_(t) = 0;
    typeptrmap_(t) = x.w[1];
    { FormTypeList **ftp = &typefnargs_(t);
      uint32 i, nft = x.w[0];
      for (i = 0; i < nft; i++) {
        FormTypeList *ft = (FormTypeList *)GlobAlloc(SU_Type, sizeof(FormTypeList));
        fread(x.w, sizeof(uint32), 1, f);
        ft->ftname = Dump_LoadedSym(x.w[0]);
        ft->fttype = Dump_LoadType(f);
        ft->ftdefault = Dump_LoadExpr(f);
        *ftp = ft;
        ftp = &ft->ftcdr;
      }
      *ftp = NULL;
      fread(&typefnaux_(t), sizeof(TypeExprFnAux), 1, f);
      typearg_(t) = Dump_LoadType(f);
      return t;
    }

  case t_coloncolon:
    fread(x.w, sizeof(uint32), 1, f);
    t = (TypeExpr *)global_list4(SU_Type, op, 0, Dump_LoadedTag(x.w[0]), 0);
    typearg_(t) = Dump_LoadType(f);
    return t;

  case t_ovld:
    t = (TypeExpr *)global_list4(SU_Type, op, (TypeExpr*)DUFF_ADDR, 0, 0);
    typeovldlist_(t) = Dump_LoadBindList(f);
    return t;

  default:
    syserr("Dump_LoadType %ld", (long)op);
    return NULL;
  }
}

Symstr *Dump_LoadSym(size_t len, FILE *f) {
  union { uint16 h[2]; uint32 w[3]; } x;
  Symstr *sym = (Symstr *)GlobAlloc(SU_Sym, sizeof(Symstr) + (int32)len);
  fread(x.h, sizeof(uint16), 1, f);
  symtype_(sym) = x.h[0];
  symlab_(sym) = NULL;
  symext_(sym) = NULL;
  fread(x.w, sizeof(uint32), 3, f);
  bind_global_(sym) = Dump_LoadedBinder(x.w[0]);
  tag_global_(sym) = Dump_LoadedTag(x.w[1]);
  symfold_(sym) = (Symstr *)(IPtr)x.w[2];
  Dump_LoadString(symname_(sym), len, f);
  ++symno;
  symindex[symno/INDEX_SEGSIZE][symno%INDEX_SEGSIZE] = sym;
  return sym;
}

static void *AllocIndex(uint32 isize, uint32 n) {
  uint32 nix = (n + INDEX_SEGSIZE) / INDEX_SEGSIZE;
  void **index = (void **)SynAlloc(nix * sizeof(void *));
  while (nix > 0) index[--nix] = SynAlloc(isize * INDEX_SEGSIZE);
  return index;
}

void Dump_Init(Dump_Sort action, FILE *f) {
  TE_Basic_Init();
  if (action == Dump_Load) {
    uint32 i;

    fread(&dump_loadstate, sizeof(uint32), 5, f);
    dump_loadstate.nsym += dump_loadstate.ngensym;
    symindex = (Symstr ***)AllocIndex(sizeof(Symstr *), dump_loadstate.nsym);
    bindlistindex = (BindList ***)AllocIndex(sizeof(BindList *), dump_loadstate.nbindlist);
    binderindex = (Binder ***)AllocIndex(sizeof(Binder *), dump_loadstate.nglobbind);
    tagindex = (TagBinder ***)AllocIndex(sizeof(TagBinder *), dump_loadstate.nglobtag);
    bindlistindex[0][0] = NULL;
    binderindex[0][0] = NULL;
    tagindex[0][0] = NULL;
    symindex[0][0] = NULL;
    symno = 0;

    for (i = 1; i < dump_loadstate.nglobtag; i++)
      tagindex[i/INDEX_SEGSIZE][i%INDEX_SEGSIZE] = (TagBinder *)GlobAlloc(SU_Bind, sizeof(TagBinder));

    for (i = 1; i < dump_loadstate.nglobbind; i++)
      binderindex[i/INDEX_SEGSIZE][i%INDEX_SEGSIZE] = (Binder *)GlobAlloc(SU_Bind, sizeof(Binder));

    for (i = 1; i < dump_loadstate.nbindlist; i++)
      bindlistindex[i/INDEX_SEGSIZE][i%INDEX_SEGSIZE] = (BindList *)GlobAlloc(SU_Inline, sizeof(BindList));

  } else {
    bindlists = (BindList ***)SynAlloc(sizeof(BindList **) * BINDLISTV_MAXSEGS);
    dump_loadstate.nbindlist = 1; nbindlistmax = 0;
  }
}
