/*
 * xvargen.c: static initialised variable extensions for C++ compiler
 * Copyright (C) Codemist Ltd, 1992
 * Copyright (C) Advanced RISC Machines Ltd., 1992, 1994
 * SPDX-Licence-Identifier: Apache-2.0
 * All rights reserved.
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include "globals.h"
#include "vargen.h"
#include "lex.h"               /* for curlex */
#include "syn.h"
#include "sem.h"
#include "simplify.h"
#include "bind.h"
#include "builtin.h"
#include "aetree.h"
#include "codebuf.h"
#ifndef NON_CODEMIST_MIDDLE_END
#include "regalloc.h"  /* to handle global register variables */
#endif
#include "mcdep.h"     /* for dbg_xxx */
#include "store.h"
#include "aeops.h"
#include "util.h"      /* for padsize */
#include "xrefs.h"
#include "inline.h"
#include "errors.h"
#define _VARGEN_H

static void dynamic_init(Expr *e, bool full);
static void vg_note_topdtor(Binder *b, bool toplevel);
static Expr *vg_get_dyninit(bool toplevel);
static Expr *vg_currentdecl_inits;

#ifndef NO_DUMP_STATE
static void Vargen_cpp_LoadState(FILE *f);
static void Vargen_cpp_DumpState(FILE *f);
#endif

#include "vargen.c"
#include <string.h>

static CmdList **dyninitq;
static CmdList *dyninitp;
static CmdList *dtorlist;
static Symstr *filectorname, *filedtorname;
static int32 vg_dyn_count;

static Expr *forge_initlhs(void)
{   int32 boff = initboffset;
    if (target_lsbitfirst) boff = MAXBITSIZE - initbsize - boff;
/* It seems a shame we can't invent names instead of dataseg+offset.    */
#if 0
#ifdef CONST_DATA_IN_CODE
    if (is_constdata())
    {
/* The following line will one day be a syserr, but is currently no     */
/* more harmless than recovering from "static int c = 1; c = 2;         */
/* @@@ However, it does mean that the constdatasegment detection code   */
/* is wrong for, e.g.,  static const int a = f();                       */
        cc_rerr(vargen_rerr_compiler_confused);
        return mk_exprbdot(s_dot, inittype,
                           (Expr *)constdatasegment, constdata.size,
                           initbsize, boff);
    }
    else
#endif
#endif
    return (cur_initlhs == 0) ? mk_exprbdot(s_dot, inittype,
        (Expr *)datasegment,    /*/* should really be struct, but int OK. */
        data_size(), initbsize, boff) :
        mk_exprbdot(s_dot, inittype, (Expr *)cur_initlhs,
                initwoffset, initbsize, boff);
/* AM has changed this code to the more obvious code (for reasons       */
/* see comment containing 'x.a = x.b = nn' in sem.c).  However,         */
/* the absence of globalize_expr(s_let) is a pain!                      */
/* However, we need this to be there (or perhaps use global store for   */
/* all allocations during static initialisers) for things like          */
/*        int *f(); static int a = (*f())++;                            */
}

/* For local statics (see [ES, p92]) we map   static T x = e;   to      */
/*    static T x; static int f; if (!f) (f=1, x=e, atexit(x.dtor));     */
/* where "x=e" above might well be a call x.ctor(e).                    */
/* Better: test 'f' from the top-level dtor code.                       */
/* Strategy: change this code into one which accumulates these          */
/* "dynamic inits to statics" separately from those to auto vars.       */
/* At the end of a declaration (sequence), if at top-level add to       */
/* dyninitp/q (after globalising), if local guard with if-firsttime.    */
/* The initialisations are probably best stored in a field like         */
/* declinit_(), but note that                                           */
/*      void f(int x) { auto a[100] = { 1,x,2,3,... }; ... }            */
/* is a little ambiguous (i.e. does 'x' get stored FIRST/EACH call?)    */
/* and therefore may need two fields.                                   */

static void dynamic_init(Expr *e, bool full)
{   if (!full) e = mkbinary(s_init, forge_initlhs(), e);
    vg_currentdecl_inits =
        vg_currentdecl_inits ? mkbinary(s_comma, vg_currentdecl_inits, e) : e;
}

static void vg_note_topctor(Expr *e)
{   if (e) e = optimise0(e);
    if (e)
    {   CmdList *cmds;
        Cmd *p = (Cmd *) GlobAlloc(SU_Other, (int32)offsetof(Cmd,cmd2));
        p->fileline = init_fl;  /* @@@ beware dataflow */
        h0_(p) = s_semicolon;
        cmd1e_(p) = globalize_expr(e);
        cmds = (CmdList *)global_cons2(SU_Other, NULL, p);
        *dyninitq = cmds;
        dyninitq = &cdr_(cmds);
    }
}

static Expr *vg_get_dyninit(bool toplevel)
{   Expr *e = vg_currentdecl_inits;
    vg_currentdecl_inits = 0;
    if (toplevel)
    {   vg_note_topctor(e);
        e = 0;
    }
    return e;
}

typedef struct DdtorList {
  struct DdtorList *ddtorcdr;
  Symstr *fnname;
  Cmd *fnbody;
} DdtorList;

static DdtorList *ddtorlist;
static int32 ddtor_size;

int32 ddtor_vecsize(void)
{   return ddtor_size * sizeof_ptr;
}

static void vg_note_topdtor(Binder *b, bool toplevel)
{   Cmd *p;
    Expr *e;
    e = mkdtor_v((Expr *)b);
    if (e) e = optimise0(e);
    if (e == 0) return;
    p = (Cmd *) GlobAlloc(SU_Other, (int32)offsetof(Cmd,cmd2));
    p->fileline = init_fl;  /* @@@ beware dataflow */
    h0_(p) = s_semicolon;
    if (toplevel)
    {   cmd1e_(p) = globalize_expr(e);
        dtorlist = (CmdList *)global_cons2(SU_Other, dtorlist, p);
        /* save dtors in reverse order of ctors.                        */
    }
    else
    {   /*cc_rerr(vargen_rerr_local_static_with_dtor, b);*/
        DeclRhsList *d;
        Binder *fn, *gen;
        TypeExpr* t = princtype(bindtype_(b));
        t = globalize_typeexpr(ptrtotype_(t));
        d = mkDeclRhsList(gensymval(YES), t, bitofstg_(s_static));
        gen = instate_declaration(d, TOPLEVEL);
        bindaddr_(gen) = get_datadesc_size();
        (void)obj_symref(bindsym_(gen),get_datadesc_xrarea()+xr_defloc,
                                get_datadesc_size());
        gendcI(sizeof_ptr, TARGET_NULL_BITPATTERN);
        binduses_(gen) |= u_referenced;
        e = mkdtor_v(mkunary(s_content, (Expr *)gen));
        e = optimise0(e);
        cmd1e_(p) = globalize_expr(e);
        d = mkDeclRhsList(gensymval(YES),
                te_fntype(te_void, 0, 0, 0, 0, 0),
                bitofstg_(s_static) | b_fnconst | b_undef);
        fn = instate_declaration(d, TOPLEVEL);
        ddtor_size++;
        ddtorlist = (DdtorList *)global_list3(SU_Other, ddtorlist, bindsym_(fn), p);
        dynamic_init(mkfnap(cppsim.xpush_ddtor,
                mkExprList1(mkunary(s_addrof, (Expr *)fn))), 1);
        dynamic_init(mkbinary(s_init, (Expr *)gen,
                mkunary(s_addrof, (Expr *)b)), 1);
    }
}

static TopDecl *vg_mk_initfn(Symstr *fn, bool ext, Cmd *c)
{   TypeExprFnAux s;
    DeclRhsList *d = mkDeclRhsList(
        /* declname = */ currentfunction.symstr = fn,
        /* decltype = */ mkTypeExprfn(t_fnap, te_void, 0, 0,
                              packTypeExprFnAux(s, 0,0,0,0,0)),
        /* declstg =  */ ext ? bitofstg_(s_extern) | b_fnconst | b_memfns :
                               bitofstg_(s_static) | b_fnconst | b_memfns);
    Binder *b = instate_declaration(d , TOPLEVEL);
    binduses_(b) |= u_referenced;
    bindstg_(b) |= b_generated;                 /* compiler generated fn */
    if (usrdbg(DBG_PROC+DBG_VAR))
    {
#if 0
        c->fileline.p = dbg_notefileline(c->fileline);
#endif
#ifndef NEW_DBG_PROC_INTERFACE
        dbg_proc(bindsym_(b), bindtype_(b),
                 (bindstg_(b) & bitofstg_(s_extern)) != 0, c->fileline);
#else
        dbg_proc(b, bindparent_(b),
                 (bindstg_(b) & bitofstg_(s_extern)) != 0, c->fileline);
#endif
    }
    return mkTopDeclFnDef(s_fndef, b, 0, c, 0);
}

typedef struct VtabList {
  struct VtabList *vtcdr;
  TagBinder *vtclass;
  int32 vtsize;
  Symstr *vtname;
} VtabList;

static VtabList *vtablist;

void vg_note_vtable(TagBinder *cl, int32 sz, Symstr *name)
{   /* assert: cl is not local store (local classes can't have vfns?).  */
    /* name is the top-level name for the vtable (get from cl later?).  */
    VtabList *p;
    if (debugging(DEBUG_BIND))
        cc_msg("Need $r [%ld] for $c\n", name, (long)sz, cl);
    for (p = vtablist; p != NULL; p = p->vtcdr)
        if (cl == p->vtclass)
        {   if (!(sz == p->vtsize && name == p->vtname))
                syserr("vg_note_vtab $c", cl);
            return;
        }
    vtablist = (VtabList *)global_list4(SU_Other, vtablist, cl, sz, name);
}

typedef struct WrapperList {
  struct WrapperList *wcdr;
  Binder *wrapper, *fnbind;
} WrapperList;

static WrapperList *wrapperlist;

static TopDecl *vg_generate_wrapper_def(Binder *wrapper, Binder *fnbind)
{   TypeExpr *fntype = bindtype_(wrapper);
    FormTypeList *ft;
    SynBindList *argbinders = 0;
    Binder *temp = 0, *thisb = 0;
    ExprList *l = 0;
    Cmd *p;
    Expr *e;
    for (ft = typefnargs_(fntype); ft != NULL; ft = ft->ftcdr)
    {   TypeExpr *t = princtype(ft->fttype);
        Expr *ee;
        Symstr *sv = ft->ftname;
        sv = sym_insert_id(symname_((sv) ? sv : gensymval(NO)));
        if (temp != 0)
            temp = bindcdr_(temp) = mk_binder(sv, bitofstg_(s_auto), ft->fttype);
        else
            thisb = temp = mk_binder(sv, bitofstg_(s_auto), ft->fttype);
        ee = (Expr *)temp;
        if (h0_(t) == t_ref) ee = mk_expr1(s_content, typearg_(t), ee);
        if (ft != typefnargs_(fntype))
            l = mkExprList(l, ee);
        binduses_(temp) |= u_referenced;
        argbinders = mkSynBindList(argbinders, temp);
    }
    p = (Cmd *) GlobAlloc(SU_Other, (int32)offsetof(Cmd, cmd2));
    p->fileline = init_fl;      /* @@@ beware dataflow */
    e = optimise0(mkfnap(mkfieldselector(s_arrow, (Expr *)thisb,
                        (ClassMember *)fnbind), dreverse(l)));
    if (!isvoidtype(typearg_(fntype)))
    {   h0_(p) = s_return;
        e = optimise0(mk_expr1(s_return, typearg_(fntype), e));
    }
    else
        h0_(p) = s_semicolon;
    cmd1e_(p) = e;
    return mkTopDeclFnDef(s_fndef, wrapper, (SynBindList *)dreverse((List *)argbinders),
                mk_cmd_block(curlex.fl, 0, mkCmdList(0, p)), 0);
}

static void vg_note_wrapper(Binder *wrapper, Binder *fnbind)
{   wrapperlist = (WrapperList *)global_list3(SU_Other, wrapperlist, wrapper, fnbind);
}

Binder *generate_wrapper(Binder *a)
{   TypeExpr *fntype = bindtype_(realbinder_(a));
    DeclRhsList *d;
    Binder *fn;
    FormTypeList *ft, *ftp = 0, *ftq = 0;

    for (ft = typefnargs_(fntype);  ft != NULL;  ft = ft->ftcdr)
    {   FormTypeList *f = (FormTypeList *) BindAlloc(sizeof(FormTypeList));
        TypeExpr *t = princtype(ft->fttype);
        f->ftcdr = NULL;
        if (isclasstype_(t))
            f->fttype = mk_typeexpr1(t_ref, t, 0);
        else
            f->fttype = ft->fttype;
        f->ftdefault = ft->ftdefault;
        if (ftp == NULL)
        {   ftp = (ftq = f);
            f->ftname = sym_insert_id("__pseudo_this");
        }
        else
        {   ftq = (ftq->ftcdr = f);
            f->ftname = ft->ftname;
        }
    }
    fntype = g_mkTypeExprfn(t_fnap, typearg_(fntype),
                0, ftp, &typefnaux_(fntype));
    d  = mkDeclRhsList(sym_insert_id(symname_(gensymval(YES))), fntype,
                        bitofstg_(s_static)|b_fnconst);
    fn  = instate_declaration(d, TOPLEVEL);
    binduses_(fn) |= u_referenced;
    bindstg_(fn) |= b_generated;                /* compiler generated function */
    vg_note_wrapper(fn, a);
    return fn;
}

static Cmd *vg_vtabcmd(TagBinder *cl, int32 sz)
{   VfnList *x = vfn_list(cl);
#if 0
    curlex.fl.p = dbg_notefileline(curlex.fl);
#endif
    x = (VfnList *)dreverse((List *)x);
    return mk_cmd_e(s_thunkentry, curlex.fl, (Expr *)x);
    if (debugging(DEBUG_DATA))
    {   for (; x; x = x->vfcdr) cc_msg("$b ", x->vfmem);
        cc_msg("\n");
    }
    /* NOTREACHED */
    return NULL;
}

static bool hackrefdataseg;

#ifdef TARGET_HAS_DEBUGGER
static int saved_usrdbgmask;
#endif
static int need_finalise;

TopDecl *vg_dynamic_init(void)
{   TopDecl *d;
    if (!hackrefdataseg && (dyninitp != NULL || dtorlist != NULL))
    {   /* reference the datasegment from the codesegment to ensure     */
        /* that C$$ctorvec is loaded if any routine is referenced as    */
        /* well as if any static datum is externally referenced.        */
        codebuf_reinit1(0);
        codebuf_reinit2();
        lit_findword(0, LIT_ADCON, bindsym_(datasegment),
                     LITF_INCODE|LITF_FIRST|LITF_LAST);
        codeseg_flush(NULL);
        hackrefdataseg = YES;
    }
#if 0
    curlex.fl.p = dbg_notefileline(curlex.fl);
#endif
    obj_clearcommoncode();
    obj_clearvtable();
#ifdef TARGET_HAS_DEBUGGER
    if (saved_usrdbgmask == -1)
    {   saved_usrdbgmask = (usrdbgmask & DBG_ANY);
        usrdbgmask &= ~DBG_ANY;
    }
#endif
    if (vtablist != NULL)
    {   char v[128+3];
        strcpy(v, "x$"); strcpy(v+2, symname_(vtablist->vtname));
        obj_setcommoncode();
        obj_setvtable();
        codebuf_reinit1(v);
        d = vg_mk_initfn(vtablist->vtname, 1,
                         vg_vtabcmd(vtablist->vtclass, vtablist->vtsize));
        vtablist = vtablist->vtcdr;
        return d;
    }
    if (dyninitp != NULL)
    {   codebuf_reinit1("x$ctor");
        d = vg_mk_initfn(sym_insert_id("__C"), 0,
                         mk_cmd_block(curlex.fl, 0, dyninitp));
        filectorname = bindsym_(d->v_f.fn.name);
        dyninitp = NULL; dyninitq = &dyninitp;
        obj_symref(sym_insert_id("__cpp_initialise"), xr_code, 0);
        return d;
    }
    if (dtorlist != NULL)
    {   codebuf_reinit1("x$dtor");
        d = vg_mk_initfn(sym_insert_id("__D"), 0,
                         mk_cmd_block(curlex.fl, 0, dtorlist));
        filedtorname = bindsym_(d->v_f.fn.name);
        dtorlist = NULL;
        if (need_finalise == 0)
        {   need_finalise = 1;
            obj_symref(sym_insert_id("__cpp_finalise"), xr_code, 0);
        }
        return d;
    }
    if (ddtorlist != NULL)
    {   char v[128+3];
        strcpy(v, "x$"); strcpy(v+2, symname_(ddtorlist->fnname));
        obj_setcommoncode();
        codebuf_reinit1(v);
        d = vg_mk_initfn(ddtorlist->fnname, 0,
                mk_cmd_block(curlex.fl, 0, mkCmdList(0, ddtorlist->fnbody)));
        ddtorlist = ddtorlist->ddtorcdr;
        if (need_finalise == 0)
            obj_symref(sym_insert_id("__cpp_finalise"), xr_code, 0);
        need_finalise |= 4;
        return d;
    }
    if (wrapperlist != NULL)
    {   char v[128+3];
        strcpy(v, "x$"); strcpy(v+2, symname_(bindsym_(wrapperlist->wrapper)));
        obj_setcommoncode();
        codebuf_reinit1(v);
        d = vg_generate_wrapper_def(wrapperlist->wrapper, wrapperlist->fnbind);
        wrapperlist = wrapperlist->wcdr;
        return d;
    }
#ifdef TARGET_HAS_DEBUGGER
    usrdbgmask = saved_usrdbgmask;    /* to ensure dbg_writedebug works */
    saved_usrdbgmask = -1;
#endif
    return NULL;
}

static void vg_ctordtor_table(Symstr *fnname, char *segname)
{   /* Beware this routine which uses a routine intended for a named    */
    /* code segment to generate data...                                 */
    /* -- maybe add a new 'LIT_SETSEG' opcode (codebuf/adddata).        */
    /* -- maybe unify code and data generation.                         */
/* @@@ make this take no data space */
    if (fnname != NULL)
    {   codebuf_reinit1(segname);
        codebuf_reinit2();
        lit_findword(0, LIT_ADCON, fnname, LITF_INCODE|LITF_FIRST|LITF_LAST);
        codeseg_flush(NULL);
        gendcAX(bindsym_(codesegment), 0, xr_data);
    }
}

void vg_ref_dynamic_init(void)
{   vg_ctordtor_table(filectorname, "x$ctorvec");
    vg_ctordtor_table(filedtorname, "x$dtorvec");
    if (need_finalise & 4)
    {   Symstr *sv = bindsym_(ddtorsegment);
        obj_symref(sv, xr_data, 0);
        /* @@@ make this take no data space */
        gendcAX(sv, 0, xr_data);
    }
}

#ifndef NO_DUMP_STATE

#include "dump.h"
static Cmd *LoadCmd(FILE *f) {
    Cmd *c = (Cmd *)GlobAlloc(SU_Other, (int32)offsetof(Cmd,cmd2));
    h0_(c) = s_semicolon;
    cmd1e_(c) = Dump_LoadExpr(f);
    return c;
}

static void Vargen_cpp_LoadState(FILE *f) {
    int32 w[3];
    int32 n;
    fread(&n, 1, sizeof(int32), f);
    if (n != 0) {
        VtabList *p, **q = &vtablist;
        for (; --n >= 0; q = &p->vtcdr) {
            fread(w, 3, sizeof(int32), f);
            p = (VtabList *)GlobAlloc(SU_Other, sizeof(VtabList));
            p->vtcdr = NULL;
            p->vtclass = Dump_LoadedTag(w[0]);
            p->vtsize = w[1];
            p->vtname = Dump_LoadedSym(w[2]);
            *q = p;
        }
    }
    fread(&n, 1, sizeof(int32), f);
    if (n != 0) {
        WrapperList *p, **q = &wrapperlist;
        for (; --n >= 0; q = &p->wcdr) {
            fread(w, 2, sizeof(int32), f);
            p = (WrapperList *)GlobAlloc(SU_Other, sizeof(WrapperList));
            p->wcdr = NULL;
            p->wrapper = Dump_LoadedBinder(w[0]);
            p->fnbind = Dump_LoadedBinder(w[1]);
            *q = p;
        }
    }
    fread(&n, 1, sizeof(int32), f);
    if (n != 0) {
        CmdList *p, **q = &dtorlist;
        for (; --n >= 0; q = &cdr_(p)) {
            p = (CmdList *)GlobAlloc(SU_Other, sizeof(CmdList));
            cdr_(p) = NULL; cmdcar_(p) = LoadCmd(f);
            *q = p;
        }
    }
    fread(&n, 1, sizeof(int32), f);
    if (n != 0) {
        DdtorList *p, **q = &ddtorlist;
        for (; --n >= 0; q = &p->ddtorcdr) {
            p = (DdtorList *)GlobAlloc(SU_Other, sizeof(DdtorList));
            p->ddtorcdr = NULL;
            fread(w, 1, sizeof(int32), f);
            p->fnname = Dump_LoadedSym(w[0]);
            p->fnbody = LoadCmd(f);
            *q = p;
        }
    }
    fread(&n, 1, sizeof(int32), f);
    if (n != 0) {
        CmdList *p;
        for (; --n >= 0; dyninitq = &cdr_(p)) {
            p = (CmdList *)GlobAlloc(SU_Other, sizeof(CmdList));
            cdr_(p) = NULL; cmdcar_(p) = LoadCmd(f);
            *dyninitq = p;
        }
    }
}

static void DumpCmd(Cmd *c, FILE *f) {
    if (h0_(c) != s_semicolon) syserr("Vargen_cpp_DumpState %ld", (long)h0_(c));
    Dump_Expr(cmd1e_(c), f);
}

static void Vargen_cpp_DumpState(FILE *f) {
    int32 w[3];
    int32 n = length((List *)vtablist);
    fwrite(&n, 1, sizeof(int32), f);
    if (vtablist != NULL)
    {   VtabList *p = vtablist;
        for (; p != NULL; p = p->vtcdr) {
            w[0] = Dump_TagRef(p->vtclass);
            w[1] = p->vtsize;
            w[2] = Dump_SymRef(p->vtname);
            fwrite(w, 3, sizeof(int32), f);
        }
    }
    n = length((List *)wrapperlist);
    fwrite(&n, 1, sizeof(int32), f);
    if (wrapperlist != NULL) {
        WrapperList *p = wrapperlist;
        for (; p != NULL; p = p->wcdr) {
            w[0] = Dump_BinderRef(p->wrapper);
            w[1] = Dump_BinderRef(p->fnbind);
            fwrite(w, 2, sizeof(int32), f);
        }
    }
    n = length((List *)dtorlist);
    fwrite(&n, 1, sizeof(int32), f);
    if (dtorlist != NULL) {
        CmdList *p = dtorlist;
        for (; p != NULL; p = cdr_(p))
            DumpCmd(cmdcar_(p), f);
    }
    n = length((List *)ddtorlist);
    fwrite(&n, 1, sizeof(int32), f);
    if (ddtorlist != NULL) {
        DdtorList *p = ddtorlist;
        for (; p != NULL; p = p->ddtorcdr) {
            w[0] = Dump_SymRef(p->fnname);
            fwrite(w, 1, sizeof(int32), f);
            DumpCmd(p->fnbody, f);
        }
    }
    n = length((List *)dyninitp);
    fwrite(&n, 1, sizeof(int32), f);
    if (dyninitp != NULL) {
        CmdList *p = dyninitp;
        for (; p != NULL; p = cdr_(p))
            DumpCmd(cmdcar_(p), f);
    }
}
#endif

void vargen_init(void)
{   hackrefdataseg = NO;
#ifdef TARGET_HAS_DEBUGGER
    saved_usrdbgmask = -1;
#endif
    need_finalise = 0;
    dyninitp = NULL; dyninitq = &dyninitp;
    dtorlist = NULL;
    vtablist = NULL;
    filectorname = NULL;
    filedtorname = NULL;
    vg_dyn_count = 0;
    vg_currentdecl_inits = NULL;
    ddtorlist = NULL;
    ddtor_size = 0;
    wrapperlist = NULL;
}

/* end of cppfe/vargen.c */
