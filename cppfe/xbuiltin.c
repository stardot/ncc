/*
 * xbuiltin.c: constants/global symbols for C++ compiler.
 * Copyright (C) Codemist Ltd., 1988-1993
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1992, 1994
 * SPDX-Licence-Identifier: Apache-2.0
 * All rights reserved.
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <time.h>
#include <string.h>
#include "globals.h"
#include "defs.h"
#include "builtin.h"
#include "bind.h"
#include "store.h"
#include "aeops.h"
#include "aetree.h"
#include "sem.h"        /* for ovld_instance_name_1 */
#include "dump.h"
#include "cg.h"
#include "jopcode.h"
#define _BUILTIN_H

static void builtin_init_cpp(void);
#ifndef NO_DUMP_STATE
static void Builtin_DumpState_cpp(FILE *f);
static void Builtin_LoadState_cpp(FILE *f);
#endif

#include "builtin.c"

Symstr *thissym = DUFF_SYMSTR,
       *ctorsym = DUFF_SYMSTR,
       *dtorsym = DUFF_SYMSTR,
       *assignsym = DUFF_SYMSTR,
       *vtabsym = DUFF_SYMSTR,
       *deletesym = DUFF_SYMSTR,
       *dtorgenlabsym = DUFF_SYMSTR;

#define DUFF_EXPR ((Expr*)DUFF_ADDR)
#define DUFF_BINDER ((Binder*)DUFF_ADDR)

cpp_op_simulation cppsim =
    {   DUFF_SYMSTR, DUFF_SYMSTR,
        DUFF_BINDER,
        DUFF_EXPR,
        DUFF_EXPR, DUFF_EXPR,
        DUFF_EXPR, DUFF_EXPR, DUFF_EXPR, DUFF_EXPR
    };

static Binder *library_function_binder_cpp(char *name, TypeExpr* fntype)
{   Symstr *sv = ovld_instance_name_1(name, fntype);
    return library_function_binder(sv, fntype);
}

static Expr *library_function_cpp(char *name, TypeExpr* fntype)
{   Symstr *sv = ovld_instance_name_1(name, fntype);
    return library_function_1(sv, fntype);
}

static void Builtin_DumpState_cpp(FILE *f) {
    WriteSymRef(thissym, f);
    WriteSymRef(ctorsym, f);
    WriteSymRef(dtorsym, f);
    WriteSymRef(assignsym, f);
    WriteSymRef(vtabsym, f);
    WriteSymRef(deletesym, f);
    WriteSymRef(dtorgenlabsym, f);
    WriteSymRef(cppsim.xnew, f);
    WriteSymRef(cppsim.xdel, f);

    WriteBinderRef(cppsim.xpvfn, f);

    Dump_Expr(cppsim.xpush_ddtor, f);
    Dump_Expr(cppsim.xnewvec, f);
    Dump_Expr(cppsim.xdelvec, f);
    Dump_Expr(cppsim.xmapvec1, f);
    Dump_Expr(cppsim.xmapvec1c, f);
    Dump_Expr(cppsim.xmapvec1ci, f);
    Dump_Expr(cppsim.xmapvec2, f);
}

static void Builtin_LoadState_cpp(FILE *f) {
    thissym = ReadSymRef(f);
    ctorsym = ReadSymRef(f);
    dtorsym = ReadSymRef(f);
    assignsym = ReadSymRef(f);
    vtabsym = ReadSymRef(f);
    deletesym = ReadSymRef(f);
    dtorgenlabsym = ReadSymRef(f);
    cppsim.xnew = ReadSymRef(f);
    cppsim.xdel = ReadSymRef(f);

    cppsim.xpvfn = ReadBinderRef(f);

    cppsim.xpush_ddtor = Dump_LoadExpr(f);
    cppsim.xnewvec = Dump_LoadExpr(f);
    cppsim.xdelvec = Dump_LoadExpr(f);
    cppsim.xmapvec1 = Dump_LoadExpr(f);
    cppsim.xmapvec1c = Dump_LoadExpr(f);
    cppsim.xmapvec1ci = Dump_LoadExpr(f);
    cppsim.xmapvec2 = Dump_LoadExpr(f);
}

static void builtin_init_cpp(void)
{
    te_boolean = initprimtype_(bitoftype_(s_bool));
    thissym = sym_insert_id("___this");
/* The next 2 lines have these exact names to match [ES] and overload.c */
    ctorsym = sym_insert_id("__ct");
    dtorsym = sym_insert_id("__dt");
    assignsym = sym_insert_id("__as");
    vtabsym = sym_insert_id("__VTABLE");
/* Maybe we need __vtp (pointer member) and __vt (static table?)        */
    deletesym = sym_insert_id("__is_delete");
    dtorgenlabsym = sym_insert_id("__generated_dtor_part");

    cppsim.xnew = bindsym_(toplevel_function("__nw",
        te_fntype(te_voidptr, te_size_t, 0, 0, 0, 0)));
    cppsim.xdel = bindsym_(toplevel_function("__dl",
        te_fntype(te_void, te_voidptr, 0, 0, 0, 0)));

/* Arguable we should just parse appropriate strings instead of this.   */
  { TypeExpr *PFv_v = g_ptrtotype_(g_te_fntype(te_void, 0, 0, 0, 0, 0));
    TypeExpr *PFPv_v =
        g_ptrtotype_(g_te_fntype(te_void, te_voidptr, 0, 0, 0, 0));
    TypeExpr *PFPvi_v =
        g_ptrtotype_(g_te_fntype(te_void, te_voidptr, te_int, 0, 0, 0));
    TypeExpr *PFPvT1_v =
        g_ptrtotype_(g_te_fntype(te_void, te_voidptr, te_voidptr, 0, 0, 0));

    cppsim.xpvfn = library_function_binder_cpp("__pvfn",
        g_te_fntype(te_void, 0, 0, 0, 0, 0));

    cppsim.x__throw = library_function_cpp("__throw",
        g_te_fntype(te_void, 0, 0, 0, 0, 0));
    cppsim.x__ex_pop = library_function_cpp("__ex_pop",
        g_te_fntype(te_void, 0, 0, 0, 0, 0));
    cppsim.x__ex_top = library_function_cpp("__ex_top",
        g_te_fntype(te_charptr, 0, 0, 0, 0, 0));
    cppsim.x__ex_push = library_function_cpp("__ex_push",
        g_te_fntype(te_charptr, te_int, te_int, 0, 0, 0));
    cppsim.xpush_ddtor = library_function_cpp("__push_ddtor",
        g_te_fntype(te_void, PFv_v, 0, 0, 0, 0));
    cppsim.xnewvec = library_function_cpp("__nw_v",
        g_te_fntype(te_voidptr, te_voidptr, te_size_t, te_size_t, PFPv_v, 0));
    cppsim.xdelvec = library_function_cpp("__dl_v",
        g_te_fntype(te_void, te_voidptr, te_size_t, PFPvi_v, 0, 0));
    cppsim.xmapvec1 = library_function_cpp("__vc",
        g_te_fntype(te_void, te_voidptr, te_size_t, te_int, PFPv_v, 0));
    cppsim.xmapvec1c = library_function_cpp("__vc",
        g_te_fntype(te_void, te_voidptr, te_voidptr, te_int, PFPv_v, 0));
    cppsim.xmapvec1ci = library_function_cpp("__vc",
        g_te_fntype(te_void, te_voidptr, te_voidptr, te_int, PFPvi_v, 0));
    cppsim.xmapvec2 = library_function_cpp("__vc",
        g_te_fntype(te_void, te_voidptr, te_voidptr, te_voidptr, te_size_t, PFPvT1_v));
  }

    te_wchar = initprimtype_(wchar_typespec);
    te_stringchar = initprimtype_(bitoftype_(s_const)|bitoftype_(s_char));
    te_wstringchar = initprimtype_(bitoftype_(s_const)|wchar_typespec);
#ifdef EXTENSION_UNSIGNED_STRINGS
    te_ustringchar = initprimtype_(bitoftype_(s_const)|bitoftype_(s_unsigned)|bitoftype_(s_char));
#endif
}

/* End of xbuiltin.c */
