/*
 * builtin.c: constants/global symbols for C compiler.
 * Copyright (C) Codemist Ltd., 1987-1992.
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1990-1992.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$ Codemist 23
 * Checkin $Date$
 * Revising $Author$
 */

/* AM memo: names in here are really getting out of control.            */
/* Rework soon, but remember that all names should be distinct in the   */
/* the first 8 chars for linkers like the os370 ones....                */

/* AM memo: more thought is required in this file to account for        */
/* natural (and unnatural) machine parameterisations.  In particular    */
/* getting the bsd vax/bsd sun/sysV names right is a pain.              */

#ifndef _BUILTIN_H
#include <time.h>
#include <string.h>
#include "globals.h"
#include "defs.h"
#include "builtin.h"
#include "bind.h"
#include "store.h"
#include "aeops.h"
#include "aetree.h"
#include "dump.h"
#include "cg.h"
#include "jopcode.h"

#define builtin_init_cpp()       ((void)0)
#define Builtin_DumpState_cpp(f) ((void)0)
#define Builtin_LoadState_cpp(f) ((void)0)
#endif

/* The following line indicates more thought is required re naming. */
#ifdef TARGET_LINKER_OMITS_DOLLAR
#  define SYSPFX "__"
#else
#  define SYSPFX "x$"
#endif

FloatCon  *fc_two_31;   /* floating point constant 2^31 */

FPConst fc_zero;         /* floating point constants 0.0  */
#ifdef PASCAL /*ECN*/
FPConst fc_half;         /*                          0.5  */
FPConst fc_big;          /*               FLT or DBL MAX  */
#endif
FPConst fc_one;          /*                          1.0  */
FPConst fc_two;          /*                          2.0  */
FPConst fc_minusone;     /*                          -1.0  */

TypeExpr *te_boolean;
Expr *lit_false;
Expr *lit_true;
Expr *lit_zero;
Expr *lit_one;

TypeExpr *te_char;    /* = (global)primtype_(bitoftype_(s_char)) */
TypeExpr *te_int;     /* = (global)primtype_(bitoftype_(s_int)) */
TypeExpr *te_ushort, *te_uint, *te_lint, *te_ulint;  /* and friends */
TypeExpr *te_llint, *te_ullint;
TypeExpr *te_stringchar, *te_wstringchar, *te_wchar;
#ifdef EXTENSION_UNSIGNED_STRINGS
TypeExpr *te_ustringchar;
#endif
TypeExpr *te_double;  /* = (global)primtype_(bitoftype_(s_double)) */
TypeExpr *te_float;   /* its short friend */
TypeExpr *te_ldble;   /* and its long one */
TypeExpr *te_void;    /* = (global)primtype_(bitoftype_(s_void)) */
TypeExpr *te_charptr; /* = (global)ptrtotype_(te_char))) */
TypeExpr *te_intptr;  /* = (global)ptrtotype_(te_int))) */
TypeExpr *te_voidptr; /* = (global)ptrtotype_(te_void))) */

/* since no-one looks inside datasegment and code segment perhaps they
   should be Symstr's */
Binder *datasegment, *codesegment, *constdatasegment, *ddtorsegment;
#ifdef TARGET_HAS_BSS
Binder *bsssegment;
#endif
Binder *extablesegment, *exhandlersegment;
Symstr *mainsym, *setjmpsym, *assertsym, *first_arg_sym, *last_arg_sym;
Symstr *libentrypoint, *stackoverflow, *stack1overflow,
       *countroutine, *count1routine;

Symstr *traproutine;
Symstr *targeterrno;

#define PUREBIT   bitoffnaux_(s_pure)
#define COMMBIT   bitoffnaux_(s_commutative)
op_simulation sim;

static Symstr *mallocsym, *callocsym, *reallocsym;

bool returnsheapptr(Symstr *fn) {
    return (fn == mallocsym || fn == callocsym || fn == reallocsym ||
            strncmp("__nw__", fn->symname, 6) == 0);
}

static Binder *library_function_binder(Symstr* sym, TypeExpr* fntype)
{   return global_mk_binder(0,
                            sym,
                            bitofstg_(s_extern) | b_undef | b_fnconst,
                            fntype);
}

static Expr *library_function_1(Symstr* sym, TypeExpr* fntype)
{   return (Expr*) global_list4(SU_Other, s_addrof,
                        global_list4(SU_Type, t_content, fntype, 0, 0),
                        (FileLine *)0,
                        library_function_binder(sym, fntype));
}

static Expr *library_function(char *name, int minf, int maxf, int32 flags)
{   Symstr *sv = sym_insert_id(name);
    TypeExprFnAux s;
    TypeExpr *fntype = g_mkTypeExprfn(t_fnap, te_int, 0, 0,
                           packTypeExprFnAux(s, minf, maxf, 0, 0, flags));
    return library_function_1(sv, fntype);
}

TypeExpr *te_fntype(TypeExpr *res, TypeExpr *a1, TypeExpr *a2,
                                   TypeExpr *a3, TypeExpr *a4,
                                   TypeExpr *a5)
{   TypeExprFnAux s;
    int n = 0;
    FormTypeList *f = 0;
    if (a5) f = mkFormTypeList(f, 0, a5, 0), n++;
    if (a4) f = mkFormTypeList(f, 0, a4, 0), n++;
    if (a3) f = mkFormTypeList(f, 0, a3, 0), n++;
    if (a2) f = mkFormTypeList(f, 0, a2, 0), n++;
    if (a1) f = mkFormTypeList(f, 0, a1, 0), n++;
    return mkTypeExprfn(t_fnap, res, 0, f, packTypeExprFnAux(s, n,n,0,0,0));
}

TypeExpr *g_te_fntype(TypeExpr *res, TypeExpr *a1, TypeExpr *a2,
                                            TypeExpr *a3, TypeExpr *a4,
                                            TypeExpr *a5)
{   TypeExprFnAux s;
    int n = 0;
    FormTypeList *f = 0;
    if (a5) f = g_mkFormTypeList(f, 0, a5, 0), n++;
    if (a4) f = g_mkFormTypeList(f, 0, a4, 0), n++;
    if (a3) f = g_mkFormTypeList(f, 0, a3, 0), n++;
    if (a2) f = g_mkFormTypeList(f, 0, a2, 0), n++;
    if (a1) f = g_mkFormTypeList(f, 0, a1, 0), n++;
    return g_mkTypeExprfn(t_fnap, res, 0, f, packTypeExprFnAux(s, n,n,0,0,0));
}

#ifdef CPLUSPLUS
/* We could use this more for C things too, but beware top-level names  */
/* starting with a single '_' which could upset conforming C progs.     */
static Binder *toplevel_function(char *name, TypeExpr *t)
{   Symstr *sv = sym_insert_id(name);
    DeclRhsList *d = mkDeclRhsList(
        /* declname = */ sv,
        /* decltype = */ t,
        /* declstg =  */ bitofstg_(s_extern) | b_fnconst | b_undef);
    (void)instate_declaration(d, TOPLEVEL);
    /* instate_declaration() returns the unwanted INSTANCE Binder.      */
    return bind_global_(sv);
}
#endif

static Expr *floating_function(int nargs, TypeExpr *result,
                               TypeExpr *a1, TypeExpr *a2, char *name, int32 fl)
{
    Symstr *w = sym_insert_id(name),
           *a_name = sym_insert_id("a"),
           *b_name = sym_insert_id("b");
    Binder *b;
    FormTypeList *a = g_mkFormTypeList(0, a_name, a1, 0);
    TypeExprFnAux s;
    fl |=
#ifdef SOFTWARE_FLOATING_POINT_RETURNS_DOUBLES_IN_REGISTERS
               (result == te_double) ? bitoffnaux_(s_structreg)|bitoffnaux_(s_pure) :
#else
               (result == te_double) ? 0 :
#endif
               bitoffnaux_(s_pure);
    if (fl & f_resultinflags) fl &= ~bitoffnaux_(s_pure);
    if (nargs != 1) a->ftcdr = g_mkFormTypeList(0, b_name, a2, 0);
    b = global_mk_binder(0,
                         w,
                         bitofstg_(s_extern) | b_undef | b_fnconst,
                         g_mkTypeExprfn(t_fnap, result, 0, a,
                            packTypeExprFnAux(s, nargs, nargs, 0, 0, fl)));
    return (Expr *)b;
}

static Expr *ll_function(int nargs, TypeExpr *result,
                         TypeExpr *a1, TypeExpr *a2, char *name, int32 fl)
{
    Symstr *w = sym_insert_id(name),
           *a_name = sym_insert_id("a"),
           *b_name = sym_insert_id("b");
    Binder *b;
    FormTypeList *a = g_mkFormTypeList(0, a_name, a1, 0);
    TypeExprFnAux s;
    fl |= (result == te_llint || result == te_ullint) ? bitoffnaux_(s_structreg)|bitoffnaux_(s_pure) :
                                                        bitoffnaux_(s_pure);
    if (fl & f_resultinflags) fl &= ~bitoffnaux_(s_pure);
    if (nargs != 1) a->ftcdr = g_mkFormTypeList(0, b_name, a2, 0);
    b = global_mk_binder(0,
                         w,
                         bitofstg_(s_extern) | b_undef | b_fnconst,
                         g_mkTypeExprfn(t_fnap, result, 0, a,
                            packTypeExprFnAux(s, nargs, nargs, 0, 0, fl)));
    return (Expr *)b;
}

#ifdef UNIQUE_DATASEG_NAMES
/* The following routine hacks round a bug in Acorn's linker (June 87) */
/* w.r.t. local symbols in different files being confused.             */
/* Something like it is probably needed for 370 CSECT names.           */
/* Acorn linker bug corrected July 87, so this code disabled.          */
/* ... but the code is still useful for Helios!                        */

static int main_compilation_count = 0;

static char *probably_unique_name(int ch)
{
    static char name[32];
#ifdef TARGET_LINKER_OMITS_DOLLAR
    sprintf(name, "__%c%lx", ch, (long)(20L*time(NULL)+main_compilation_count));
#else
    sprintf(name, "x$%c%lx", ch, (long)(20L*time(NULL)+main_compilation_count));
#endif
    return name;
}
#endif

static void initfpconst(FPConst *fc, const char val[])
{
    fc->s = real_of_string(val, ts_float);
    fc->d = real_of_string(val, ts_double);
}

static Expr *globalize_integer(TypeExpr *t, int n)
{   return (Expr *)global_list5(SU_Const,
        s_integer, t,
        (FileLine *)NULL, (Expr *)n, (Expr *)NULL);
}

#ifndef NO_DUMP_STATE

static Binder *ReadBinderRef(FILE *f) {
    uint32 w;
    fread(&w, sizeof(uint32), 1, f);
    return Dump_LoadedBinder(w);
}

static Symstr *ReadSymRef(FILE *f) {
    uint32 w;
    fread(&w, sizeof(uint32), 1, f);
    return Dump_LoadedSym(w);
}

void Builtin_LoadState(FILE *f) {
    sim.mulfn = Dump_LoadExpr(f);
    sim.divfn = Dump_LoadExpr(f);
    sim.udivfn = Dump_LoadExpr(f);
    sim.divtestfn = Dump_LoadExpr(f);
    sim.remfn = Dump_LoadExpr(f);
    sim.uremfn = Dump_LoadExpr(f);
    sim.fdivfn = Dump_LoadExpr(f);
    sim.ddivfn = Dump_LoadExpr(f);
#ifdef TARGET_HAS_DIV_10_FUNCTION
    sim.div10fn = Dump_LoadExpr(f);
    sim.udiv10fn = Dump_LoadExpr(f);
    sim.rem10fn = Dump_LoadExpr(f);
    sim.urem10fn = Dump_LoadExpr(f);
#endif
    sim.xprintf = Dump_LoadExpr(f);
    sim.xfprintf = Dump_LoadExpr(f);
    sim.xsprintf = Dump_LoadExpr(f);

    sim.yprintf = ReadSymRef(f);
    sim.yfprintf = ReadSymRef(f);
    sim.ysprintf = ReadSymRef(f);

    sim.dadd = (Expr *)ReadBinderRef(f);
    sim.dsubtract = (Expr *)ReadBinderRef(f);
    sim.dmultiply = (Expr *)ReadBinderRef(f);
    sim.ddivide = (Expr *)ReadBinderRef(f);
    sim.dnegate = (Expr *)ReadBinderRef(f);
    sim.dgreater = (Expr *)ReadBinderRef(f);
    sim.dgeq = (Expr *)ReadBinderRef(f);
    sim.dless = (Expr *)ReadBinderRef(f);
    sim.dleq = (Expr *)ReadBinderRef(f);
    sim.dequal = (Expr *)ReadBinderRef(f);
    sim.dneq = (Expr *)ReadBinderRef(f);
    sim.dfloat = (Expr *)ReadBinderRef(f);
    sim.dfloatu = (Expr *)ReadBinderRef(f);
    sim.dfix = (Expr *)ReadBinderRef(f);
    sim.dfixu = (Expr *)ReadBinderRef(f);

    sim.fadd = (Expr *)ReadBinderRef(f);
    sim.fsubtract = (Expr *)ReadBinderRef(f);
    sim.fmultiply = (Expr *)ReadBinderRef(f);
    sim.fdivide = (Expr *)ReadBinderRef(f);
    sim.fnegate = (Expr *)ReadBinderRef(f);
    sim.fgreater = (Expr *)ReadBinderRef(f);
    sim.fgeq = (Expr *)ReadBinderRef(f);
    sim.fless = (Expr *)ReadBinderRef(f);
    sim.fleq = (Expr *)ReadBinderRef(f);
    sim.fequal = (Expr *)ReadBinderRef(f);
    sim.fneq = (Expr *)ReadBinderRef(f);
    sim.ffloat = (Expr *)ReadBinderRef(f);
    sim.ffloatu = (Expr *)ReadBinderRef(f);
    sim.ffix = (Expr *)ReadBinderRef(f);
    sim.ffixu = (Expr *)ReadBinderRef(f);

    sim.fnarrow = (Expr *)ReadBinderRef(f);
    sim.dwiden = (Expr *)ReadBinderRef(f);

#ifdef TARGET_SOFTFP_SUPPORT_INCLUDES_REVERSE_OPS
    sim.drsb = (Expr *)ReadBinderRef(f);
    sim.drdiv = (Expr *)ReadBinderRef(f);
    sim.frsb = (Expr *)ReadBinderRef(f);
    sim.frdiv = (Expr *)ReadBinderRef(f);
#endif

    sim.llnot = (Expr *)ReadBinderRef(f);
    sim.llneg = (Expr *)ReadBinderRef(f);
    sim.lladd = (Expr *)ReadBinderRef(f);
    sim.llrsb = (Expr *)ReadBinderRef(f);
    sim.llsub = (Expr *)ReadBinderRef(f);
    sim.llmul = (Expr *)ReadBinderRef(f);
    sim.llurdv = (Expr *)ReadBinderRef(f);
    sim.lludiv = (Expr *)ReadBinderRef(f);
    sim.llsrdv = (Expr *)ReadBinderRef(f);
    sim.llsdiv = (Expr *)ReadBinderRef(f);
    sim.llurrem = (Expr *)ReadBinderRef(f);
    sim.llurem = (Expr *)ReadBinderRef(f);
    sim.llsrrem = (Expr *)ReadBinderRef(f);
    sim.llsrem = (Expr *)ReadBinderRef(f);
    sim.lland = (Expr *)ReadBinderRef(f);
    sim.llor = (Expr *)ReadBinderRef(f);
    sim.lleor = (Expr *)ReadBinderRef(f);
    sim.llshiftl = (Expr *)ReadBinderRef(f);
    sim.llushiftr = (Expr *)ReadBinderRef(f);
    sim.llsshiftr = (Expr *)ReadBinderRef(f);
    sim.llcmpeq = (Expr *)ReadBinderRef(f);
    sim.llcmpne = (Expr *)ReadBinderRef(f);
    sim.llucmpgt = (Expr *)ReadBinderRef(f);
    sim.llucmpge = (Expr *)ReadBinderRef(f);
    sim.llucmplt = (Expr *)ReadBinderRef(f);
    sim.llucmple = (Expr *)ReadBinderRef(f);
    sim.llscmpgt = (Expr *)ReadBinderRef(f);
    sim.llscmpge = (Expr *)ReadBinderRef(f);
    sim.llscmplt = (Expr *)ReadBinderRef(f);
    sim.llscmple = (Expr *)ReadBinderRef(f);
    sim.llfroml = (Expr *)ReadBinderRef(f);
    sim.llfromu = (Expr *)ReadBinderRef(f);
    sim.llsfromd = (Expr *)ReadBinderRef(f);
    sim.llsfromf = (Expr *)ReadBinderRef(f);
    sim.llufromd = (Expr *)ReadBinderRef(f);
    sim.llufromf = (Expr *)ReadBinderRef(f);
    sim.lltol = (Expr *)ReadBinderRef(f);
    sim.llstod = (Expr *)ReadBinderRef(f);
    sim.llstof = (Expr *)ReadBinderRef(f);
    sim.llutod = (Expr *)ReadBinderRef(f);
    sim.llutof = (Expr *)ReadBinderRef(f);

    sim.readcheck1 = Dump_LoadExpr(f);
    sim.readcheck2 = Dump_LoadExpr(f);
    sim.readcheck4 = Dump_LoadExpr(f);
    sim.writecheck1 = Dump_LoadExpr(f);
    sim.writecheck2 = Dump_LoadExpr(f);
    sim.writecheck4 = Dump_LoadExpr(f);
    sim.proc_entry = Dump_LoadExpr(f);
    sim.proc_exit  = Dump_LoadExpr(f);

    sim.memcpyfn = Dump_LoadExpr(f);
    sim.memsetfn = Dump_LoadExpr(f);
    sim.realmemcpyfn = Dump_LoadExpr(f);
    sim.realmemsetfn = Dump_LoadExpr(f);

    sim.strcpysym = ReadSymRef(f);
    sim.strlensym = ReadSymRef(f);
    mallocsym = ReadSymRef(f);
    callocsym = ReadSymRef(f);
    reallocsym = ReadSymRef(f);

    sim.dpow = ReadSymRef(f);
    sim.dceil = ReadSymRef(f);
    sim.dfloor = ReadSymRef(f);
    sim.dmod = ReadSymRef(f);
    sim.dabs = ReadSymRef(f);

    sim.inserted_word = Dump_LoadExpr(f);

    datasegment = ReadBinderRef(f);
    codesegment = ReadBinderRef(f);
#ifdef TARGET_HAS_BSS
    bsssegment = ReadBinderRef(f);
#endif
    ddtorsegment = ReadBinderRef(f);
    constdatasegment = ReadBinderRef(f);
    extablesegment = ReadBinderRef(f);
    exhandlersegment = ReadBinderRef(f);
    mainsym = ReadSymRef(f);
    setjmpsym = ReadSymRef(f);
    first_arg_sym = ReadSymRef(f);
    last_arg_sym = ReadSymRef(f);

    lit_false = Dump_LoadExpr(f);
    lit_true = Dump_LoadExpr(f);
    lit_zero = Dump_LoadExpr(f);
    lit_one = Dump_LoadExpr(f);

    if (LanguageIsCPlusPlus) Builtin_LoadState_cpp(f);
}

static void WriteBinderRef(Binder *b, FILE *f) {
    uint32 w = Dump_BinderRef(b);
    fwrite(&w, sizeof(uint32), 1, f);
}

static void WriteSymRef(Symstr *sym, FILE *f) {
    uint32 w = Dump_SymRef(sym);
    fwrite(&w, sizeof(uint32), 1, f);
}

void Builtin_DumpState(FILE *f) {
    Dump_Expr(sim.mulfn, f);
    Dump_Expr(sim.divfn, f);
    Dump_Expr(sim.udivfn, f);
    Dump_Expr(sim.divtestfn, f);
    Dump_Expr(sim.remfn, f);
    Dump_Expr(sim.uremfn, f);
    Dump_Expr(sim.fdivfn, f);
    Dump_Expr(sim.ddivfn, f);
#ifdef TARGET_HAS_DIV_10_FUNCTION
    Dump_Expr(sim.div10fn, f);
    Dump_Expr(sim.udiv10fn, f);
    Dump_Expr(sim.rem10fn, f);  /* obsolete */
    Dump_Expr(sim.urem10fn, f); /* obsolete */
#endif
    Dump_Expr(sim.xprintf, f);
    Dump_Expr(sim.xfprintf, f);
    Dump_Expr(sim.xsprintf, f);

    WriteSymRef(sim.yprintf, f);
    WriteSymRef(sim.yfprintf, f);
    WriteSymRef(sim.ysprintf, f);

    WriteBinderRef(exb_(sim.dadd), f);
    WriteBinderRef(exb_(sim.dsubtract), f);
    WriteBinderRef(exb_(sim.dmultiply), f);
    WriteBinderRef(exb_(sim.ddivide), f);
    WriteBinderRef(exb_(sim.dnegate), f);
    WriteBinderRef(exb_(sim.dgreater), f);
    WriteBinderRef(exb_(sim.dgeq), f);
    WriteBinderRef(exb_(sim.dless), f);
    WriteBinderRef(exb_(sim.dleq), f);
    WriteBinderRef(exb_(sim.dequal), f);
    WriteBinderRef(exb_(sim.dneq), f);
    WriteBinderRef(exb_(sim.dfloat), f);
    WriteBinderRef(exb_(sim.dfloatu), f);
    WriteBinderRef(exb_(sim.dfix), f);
    WriteBinderRef(exb_(sim.dfixu), f);

    WriteBinderRef(exb_(sim.fadd), f);
    WriteBinderRef(exb_(sim.fsubtract), f);
    WriteBinderRef(exb_(sim.fmultiply), f);
    WriteBinderRef(exb_(sim.fdivide), f);
    WriteBinderRef(exb_(sim.fnegate), f);
    WriteBinderRef(exb_(sim.fgreater), f);
    WriteBinderRef(exb_(sim.fgeq), f);
    WriteBinderRef(exb_(sim.fless), f);
    WriteBinderRef(exb_(sim.fleq), f);
    WriteBinderRef(exb_(sim.fequal), f);
    WriteBinderRef(exb_(sim.fneq), f);
    WriteBinderRef(exb_(sim.ffloat), f);
    WriteBinderRef(exb_(sim.ffloatu), f);
    WriteBinderRef(exb_(sim.ffix), f);
    WriteBinderRef(exb_(sim.ffixu), f);

    WriteBinderRef(exb_(sim.fnarrow), f);
    WriteBinderRef(exb_(sim.dwiden), f);

#ifdef TARGET_SOFTFP_SUPPORT_INCLUDES_REVERSE_OPS
    WriteBinderRef(exb_(sim.drsb), f);
    WriteBinderRef(exb_(sim.drdiv), f);
    WriteBinderRef(exb_(sim.frsb), f);
    WriteBinderRef(exb_(sim.frdiv), f);
#endif

    WriteBinderRef(exb_(sim.llnot), f);
    WriteBinderRef(exb_(sim.llneg), f);
    WriteBinderRef(exb_(sim.lladd), f);
    WriteBinderRef(exb_(sim.llrsb), f);
    WriteBinderRef(exb_(sim.llsub), f);
    WriteBinderRef(exb_(sim.llmul), f);
    WriteBinderRef(exb_(sim.llurdv), f);
    WriteBinderRef(exb_(sim.lludiv), f);
    WriteBinderRef(exb_(sim.llsrdv), f);
    WriteBinderRef(exb_(sim.llsdiv), f);
    WriteBinderRef(exb_(sim.llurrem), f);
    WriteBinderRef(exb_(sim.llurem), f);
    WriteBinderRef(exb_(sim.llsrrem), f);
    WriteBinderRef(exb_(sim.llsrem), f);
    WriteBinderRef(exb_(sim.lland), f);
    WriteBinderRef(exb_(sim.llor), f);
    WriteBinderRef(exb_(sim.lleor), f);
    WriteBinderRef(exb_(sim.llshiftl), f);
    WriteBinderRef(exb_(sim.llushiftr), f);
    WriteBinderRef(exb_(sim.llsshiftr), f);
    WriteBinderRef(exb_(sim.llcmpeq), f);
    WriteBinderRef(exb_(sim.llcmpne), f);
    WriteBinderRef(exb_(sim.llucmpgt), f);
    WriteBinderRef(exb_(sim.llucmpge), f);
    WriteBinderRef(exb_(sim.llucmplt), f);
    WriteBinderRef(exb_(sim.llucmple), f);
    WriteBinderRef(exb_(sim.llscmpgt), f);
    WriteBinderRef(exb_(sim.llscmpge), f);
    WriteBinderRef(exb_(sim.llscmplt), f);
    WriteBinderRef(exb_(sim.llscmple), f);
    WriteBinderRef(exb_(sim.llfroml), f);
    WriteBinderRef(exb_(sim.llfromu), f);
    WriteBinderRef(exb_(sim.llsfromf), f);
    WriteBinderRef(exb_(sim.llsfromd), f);
    WriteBinderRef(exb_(sim.llufromf), f);
    WriteBinderRef(exb_(sim.llufromd), f);
    WriteBinderRef(exb_(sim.lltol), f);
    WriteBinderRef(exb_(sim.llstod), f);
    WriteBinderRef(exb_(sim.llstof), f);
    WriteBinderRef(exb_(sim.llutod), f);
    WriteBinderRef(exb_(sim.llutof), f);
    Dump_Expr(sim.readcheck1, f);
    Dump_Expr(sim.readcheck2, f);
    Dump_Expr(sim.readcheck4, f);
    Dump_Expr(sim.writecheck1, f);
    Dump_Expr(sim.writecheck2, f);
    Dump_Expr(sim.writecheck4, f);
    Dump_Expr(sim.proc_entry, f);
    Dump_Expr(sim.proc_exit , f);

/* _memcpyfn and _memsetfn are internals for (aligned) struct copy/clr  */
    Dump_Expr(sim.memcpyfn, f);
    Dump_Expr(sim.memsetfn, f);
    Dump_Expr(sim.realmemcpyfn, f);
    Dump_Expr(sim.realmemsetfn, f);

    WriteSymRef(sim.strcpysym, f);
    WriteSymRef(sim.strlensym, f);
    WriteSymRef(mallocsym, f);
    WriteSymRef(callocsym, f);
    WriteSymRef(reallocsym, f);

    WriteSymRef(sim.dpow, f);
    WriteSymRef(sim.dceil, f);
    WriteSymRef(sim.dfloor, f);
    WriteSymRef(sim.dmod, f);
    WriteSymRef(sim.dabs, f);

    Dump_Expr(sim.inserted_word, f);

    WriteBinderRef(datasegment, f);
    WriteBinderRef(codesegment, f);
#ifdef TARGET_HAS_BSS
    WriteBinderRef(bsssegment, f);
#endif
    WriteBinderRef(ddtorsegment, f);
    WriteBinderRef(constdatasegment, f);
    WriteBinderRef(extablesegment, f);
    WriteBinderRef(exhandlersegment, f);
    WriteSymRef(mainsym, f);
    WriteSymRef(setjmpsym, f);
    WriteSymRef(first_arg_sym, f);
    WriteSymRef(last_arg_sym, f);

    Dump_Expr(lit_false, f);
    Dump_Expr(lit_true, f);
    Dump_Expr(lit_zero, f);
    Dump_Expr(lit_one, f);

    if (LanguageIsCPlusPlus) Builtin_DumpState_cpp(f);
}
#endif

void builtin_init(void)
{
    initfpconst(&fc_zero, "0.0");
#ifdef PASCAL /*ECN*/
    initfpconst(&fc_half, "0.5");
    fc_big.s = real_of_string("3.40282347e+38", ts_float);
    fc_big.d = real_of_string("1.79769313486231571e+308", ts_double);
#endif
    initfpconst(&fc_one, "1.0");
    initfpconst(&fc_two, "2.0");
    initfpconst(&fc_minusone, "-1.0");

    fc_two_31 = real_of_string("2147483648.0", ts_double);
#define initprimtype_(t) (TypeExpr*)global_list4(SU_Other, s_typespec, (t),0,0);
    te_char = initprimtype_(bitoftype_(s_char));
    te_int = initprimtype_(ts_int);
    te_ushort = initprimtype_(ts_short|bitoftype_(s_unsigned));
    te_uint = initprimtype_(ts_int|bitoftype_(s_unsigned));
    te_lint = initprimtype_(ts_long);
    te_ulint = initprimtype_(ts_long|bitoftype_(s_unsigned));
    te_llint = initprimtype_(ts_longlong);
    te_ullint = initprimtype_(ts_longlong|bitoftype_(s_unsigned));
    te_double = initprimtype_(ts_double);
    te_float = initprimtype_(ts_float);
    te_ldble = initprimtype_(ts_longdouble);
    te_void = initprimtype_(bitoftype_(s_void));
#define g_ptrtotype_(t) (TypeExpr*)global_list4(SU_Other, t_content, (t), 0, 0)
    te_charptr = g_ptrtotype_(te_char);
    te_intptr = g_ptrtotype_(te_int);
    te_voidptr = g_ptrtotype_(te_void);

#if defined(TARGET_IS_UNIX) && !defined(TARGET_IS_SPARC) && !defined(TARGET_IS_ALPHA)
    sim.mulfn = library_function("x$mul", 2, 2, PUREBIT);
    sim.divfn = library_function("x$div", 2, 2, PUREBIT);
    sim.udivfn = library_function("x$udiv", 2, 2, PUREBIT);
    sim.divtestfn = library_function("x$divtest", 1, 1, PUREBIT);
    sim.remfn = library_function("x$mod", 2, 2, PUREBIT);
    sim.uremfn = library_function("x$umod", 2, 2, PUREBIT);
    sim.fdivfn = library_function("x$fdiv", 2, 2, PUREBIT);
    sim.ddivfn = library_function("x$ddiv", 2, 2, PUREBIT);
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
    sim.mulfn = library_function("__multiply", 2, 2, PUREBIT);
    sim.divfn = library_function("__divide", 2, 2, PUREBIT);
    sim.udivfn = library_function("__udivide", 2, 2, PUREBIT);
    sim.divtestfn = library_function("__divtest", 1, 1, PUREBIT);
    sim.remfn = library_function("__remainder", 2, 2, PUREBIT);
    sim.uremfn = library_function("__uremainder", 2, 2, PUREBIT);
    sim.fdivfn = library_function("__fdivide", 2, 2, PUREBIT);
    sim.ddivfn = library_function("__ddivide", 2, 2, PUREBIT);
#else
/* the 'obsolete's below refer to the ARM only.                         */
    sim.mulfn = library_function("x$multiply", 2, 2, PUREBIT);  /* obsolete */
#if defined(TARGET_IS_ARM_OR_THUMB) && !defined(OBSOLETE_ARM_NAMES)
    sim.divfn = library_function(TARGET_PREFIX("__rt_sdiv"), 2, 2, PUREBIT);
    sim.udivfn = library_function(TARGET_PREFIX("__rt_udiv"), 2, 2, PUREBIT);
    sim.divtestfn = library_function(TARGET_PREFIX("__rt_divtest"), 1, 1, PUREBIT);
#else
    sim.divfn = library_function("x$divide", 2, 2, PUREBIT);
    sim.udivfn = library_function("x$udivide", 2, 2, PUREBIT);
    sim.divtestfn = library_function("x$divtest", 1, 1, PUREBIT);
#endif
    sim.remfn = library_function("x$remainder", 2, 2, PUREBIT);     /* obsolete */
    sim.uremfn = library_function("x$uremainder", 2, 2, PUREBIT);   /* obsolete */
    sim.fdivfn = library_function("x$fdivide", 2, 2, PUREBIT);
    sim.ddivfn = library_function("x$ddivide", 2, 2, PUREBIT);
#endif
#endif
#ifdef TARGET_HAS_DIV_10_FUNCTION
#if defined(TARGET_IS_ARM_OR_THUMB) && !defined(OBSOLETE_ARM_NAMES)
    sim.div10fn = library_function(TARGET_PREFIX("__rt_sdiv10"), 1, 1, PUREBIT);
    sim.udiv10fn = library_function(TARGET_PREFIX("__rt_udiv10"), 1, 1, PUREBIT);
#else
    sim.div10fn = library_function("_kernel_sdiv10", 1, 1, PUREBIT);
    sim.udiv10fn = library_function("_kernel_udiv10", 1, 1, PUREBIT);
#endif
    sim.rem10fn = library_function("_kernel_srem10", 1, 1, PUREBIT);  /* obsolete */
    sim.urem10fn = library_function("_kernel_urem10", 1, 1, PUREBIT); /* obsolete */
#endif
    sim.xprintf = library_function("_printf", 1, 1999, 0L);
    sim.xfprintf = library_function("_fprintf", 2, 1999, 0L);
    sim.xsprintf = library_function("_sprintf", 2, 1999, 0L);
    sim.yprintf = sym_insert_id("printf");
    sim.yfprintf = sym_insert_id("fprintf");
    sim.ysprintf = sym_insert_id("sprintf");

#ifdef STRING_COMPRESSION
    sim.xprintf_z = library_function("_printf$Z", 1, 1999, 0L);
    sim.xfprintf_z = library_function("_fprintf$Z", 2, 1999, 0L);
    sim.xsprintf_z = library_function("_sprintf$Z", 2, 1999, 0L);
    sim.yprintf_z = library_function("printf$Z", 1, 1999, 0L);
    sim.yfprintf_z = library_function("fprintf$Z", 2, 1999, 0L);
    sim.ysprintf_z = library_function("sprintf$Z", 2, 1999, 0L);
#endif
    sim.dadd = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_dadd"), COMMBIT);
    sim.dsubtract = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_dsub"), 0);
    sim.dmultiply = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_dmul"), COMMBIT);
    sim.ddivide = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_ddiv"), 0);
    sim.dnegate = floating_function(1,te_double,te_double,NULL,TARGET_PREFIX("_dneg"), 0);
if (resultinflags)
{
    sim.dgreater = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dcmpge"), f_resultinflags | Q_HI);
    sim.dgeq = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dcmpge"), f_resultinflags | Q_HS);
    sim.dless = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dcmple"), f_resultinflags | Q_LO);
    sim.dleq = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dcmple"), f_resultinflags | Q_LS);
    sim.dequal = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dcmpeq"), COMMBIT | f_resultinflags | Q_EQ);
    sim.dneq = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dcmpeq"), COMMBIT | f_resultinflags | Q_NE);
}
else
{
    sim.dgreater = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dgr"), 0);
    sim.dgeq = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dgeq"), 0);
    sim.dless = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dls"), 0);
    sim.dleq = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dleq"), 0);
    sim.dequal = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_deq"), COMMBIT);
    sim.dneq = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dneq"), COMMBIT);
}
    sim.dfloat = floating_function(1,te_double,te_int,NULL,TARGET_PREFIX("_dflt"), 0);
    sim.dfloatu = floating_function(1,te_double,te_uint,NULL,TARGET_PREFIX("_dfltu"), 0);
    sim.dfix = floating_function(1,te_int,te_double,NULL,TARGET_PREFIX("_dfix"), 0);
    sim.dfixu = floating_function(1,te_uint,te_double,NULL,TARGET_PREFIX("_dfixu"), 0);

    sim.fadd = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_fadd"), COMMBIT);
    sim.fsubtract = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_fsub"), 0);
    sim.fmultiply = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_fmul"), COMMBIT);
    sim.fdivide = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_fdiv"), 0);
    sim.fnegate = floating_function(1,te_float,te_int,NULL,TARGET_PREFIX("_fneg"), 0);
if (resultinflags)
{
    sim.fgreater = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fcmpge"), f_resultinflags | Q_HI);
    sim.fgeq = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fcmpge"), f_resultinflags | Q_HS);
    sim.fless = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fcmple"), f_resultinflags | Q_LO);
    sim.fleq = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fcmple"), f_resultinflags | Q_LS);
    sim.fequal = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fcmpeq"), COMMBIT | f_resultinflags | Q_EQ);
    sim.fneq = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fcmpeq"), COMMBIT | f_resultinflags | Q_NE);
}
else
{
    sim.fgreater = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fgr"), 0);
    sim.fgeq = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fgeq"), 0);
    sim.fless = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fls"), 0);
    sim.fleq = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fleq"), 0);
    sim.fequal = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_feq"), COMMBIT);
    sim.fneq = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fneq"), COMMBIT);
}
    sim.ffloat = floating_function(1,te_float,te_int,NULL,TARGET_PREFIX("_fflt"), 0);
    sim.ffloatu = floating_function(1,te_float,te_uint,NULL,TARGET_PREFIX("_ffltu"), f_resultinintregs);
    sim.ffix = floating_function(1,te_int,te_int,NULL,TARGET_PREFIX("_ffix"), 0);
    sim.ffixu = floating_function(1,te_uint,te_int,NULL,TARGET_PREFIX("_ffixu"), f_nofpregargs);

    sim.fnarrow = floating_function(1,te_float,te_double,NULL,TARGET_PREFIX("_d2f"), f_nofpregargs|f_resultinintregs);
    sim.dwiden = floating_function(1,te_double,te_float,NULL,TARGET_PREFIX("_f2d"), f_nofpregargs|f_resultinintregs);

#ifdef TARGET_SOFTFP_SUPPORT_INCLUDES_REVERSE_OPS
    sim.drsb = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_drsb"), 0);
    sim.drdiv = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_drdiv"), 0);
    sim.frsb = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_frsb"), 0);
    sim.frdiv = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_frdiv"), 0);
#endif

    sim.llnot = ll_function(1, te_llint, te_llint, NULL, TARGET_PREFIX("_ll_not"), 0);
    sim.llneg = ll_function(1, te_llint, te_llint, NULL, TARGET_PREFIX("_ll_neg"), 0);
    sim.lladd = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_add"), COMMBIT);
    sim.llrsb = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_rsb"), 0);
    sim.llsub = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_sub"), 0);
    sim.llmul = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_mul"), COMMBIT);
    sim.llurdv = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_urdv"), 0);
    sim.lludiv = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_udiv"), 0);
    sim.llsrdv = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_srdv"), 0);
    sim.llsdiv = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_sdiv"), 0);
    sim.llurrem = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_urrem"), 0);
    sim.llurem = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_urem"), 0);
    sim.llsrrem = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_srrem"), 0);
    sim.llsrem = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_srem"), 0);
    sim.lland = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_and"), COMMBIT);
    sim.llor = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_or"), COMMBIT);
    sim.lleor = ll_function(2, te_llint, te_llint, te_llint, TARGET_PREFIX("_ll_eor"), COMMBIT);
    sim.llshiftl = ll_function(2, te_llint, te_llint, te_uint, TARGET_PREFIX("_ll_shift_l"), 0);
    sim.llushiftr = ll_function(2, te_llint, te_llint, te_uint, TARGET_PREFIX("_ll_ushift_r"), 0);
    sim.llsshiftr = ll_function(2, te_llint, te_llint, te_uint, TARGET_PREFIX("_ll_sshift_r"), 0);
if (resultinflags)
{
    sim.llcmpeq = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_cmpeq"), f_resultinflags + Q_EQ);
    sim.llcmpne = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_cmpeq"), f_resultinflags + Q_NE);
    sim.llucmpgt = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_cmpeq"), f_resultinflags + Q_HI);
    sim.llucmpge = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_cmpeq"), f_resultinflags + Q_HS);
    sim.llucmplt = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_cmpeq"), f_resultinflags + Q_LO);
    sim.llucmple = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_cmpeq"), f_resultinflags + Q_LS);
    sim.llscmpgt = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_scmple"), f_resultinflags + Q_LT);
    sim.llscmpge = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_scmpge"), f_resultinflags + Q_GE);
    sim.llscmplt = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_scmpge"), f_resultinflags + Q_LT);
    sim.llscmple = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_scmple"), f_resultinflags + Q_GE);
}
else
{
    sim.llcmpeq = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_cmpeq"), 0);
    sim.llcmpne = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_cmpne"), 0);
    sim.llucmpgt = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_ucmpgt"), 0);
    sim.llucmpge = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_ucmpge"), 0);
    sim.llucmplt = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_ucmplt"), 0);
    sim.llucmple = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_ucmple"), 0);
    sim.llscmpgt = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_scmpgt"), 0);
    sim.llscmpge = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_scmpge"), 0);
    sim.llscmplt = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_scmplt"), 0);
    sim.llscmple = ll_function(2, te_int, te_llint, te_llint, TARGET_PREFIX("_ll_scmple"), 0);
}
    sim.llfroml = ll_function(1, te_llint, te_lint, NULL, TARGET_PREFIX("_ll_from_l"), 0);
    sim.llfromu = ll_function(1, te_llint, te_ulint, NULL, TARGET_PREFIX("_ll_from_u"), 0);
    sim.llsfromd = ll_function(1, te_llint, te_double, NULL, TARGET_PREFIX("_ll_sfrom_d"), f_nofpregargs);
    sim.llsfromf = ll_function(1, te_llint, te_float, NULL, TARGET_PREFIX("_ll_sfrom_f"), f_nofpregargs);
    sim.llufromd = ll_function(1, te_ullint, te_double, NULL, TARGET_PREFIX("_ll_ufrom_d"), f_nofpregargs);
    sim.llufromf = ll_function(1, te_ullint, te_float, NULL, TARGET_PREFIX("_ll_ufrom_f"), f_nofpregargs);
    sim.lltol = ll_function(1, te_lint, te_llint, NULL, TARGET_PREFIX("_ll_to_l"), 0);
    sim.llstod = ll_function(1, te_double, te_llint, NULL, TARGET_PREFIX("_ll_sto_d"), f_resultinintregs);
    sim.llstof = ll_function(1, te_float, te_llint, NULL, TARGET_PREFIX("_ll_sto_f"), f_resultinintregs);
    sim.llutod = ll_function(1, te_double, te_ullint, NULL, TARGET_PREFIX("_ll_uto_d"), f_resultinintregs);
    sim.llutof = ll_function(1, te_float, te_ullint, NULL, TARGET_PREFIX("_ll_uto_f"), f_resultinintregs);

#if defined(TARGET_IS_ARM) && !defined(OBSOLETE_ARM_NAMES)
    sim.readcheck1 = library_function("__rt_rd1chk", 1, 1, PUREBIT);
    sim.readcheck2 = library_function("__rt_rd2chk", 1, 1, PUREBIT);
    sim.readcheck4 = library_function("__rt_rd4chk", 1, 1, PUREBIT);
    sim.writecheck1 = library_function("__rt_wr1chk", 1, 1, PUREBIT);
    sim.writecheck2 = library_function("__rt_wr2chk", 1, 1, PUREBIT);
    sim.writecheck4 = library_function("__rt_wr4chk", 1, 1, PUREBIT);
#else
    sim.readcheck1 = library_function("_rd1chk", 1, 1, PUREBIT);
    sim.readcheck2 = library_function("_rd2chk", 1, 1, PUREBIT);
    sim.readcheck4 = library_function("_rd4chk", 1, 1, PUREBIT);
    sim.writecheck1 = library_function("_wr1chk", 1, 1, PUREBIT);
    sim.writecheck2 = library_function("_wr2chk", 1, 1, PUREBIT);
    sim.writecheck4 = library_function("_wr4chk", 1, 1, PUREBIT);
#endif
    sim.proc_entry = library_function("_proc_entry", 1, 1999, 0L);
    sim.proc_exit  = library_function("_proc_exit",  1, 1999, 0L);

/* _memcpyfn and _memsetfn are internals for (aligned) struct copy/clr  */
    sim.memcpyfn = library_function("_memcpy", 3, 3, 0L);
    sim.memsetfn = library_function("_memset", 3, 3, 0L);
    sim.realmemcpyfn = library_function("memcpy", 3, 3, 0L);
    sim.realmemsetfn = library_function("memset", 3, 3, 0L);

    sim.strcpysym = sym_insert_id("strcpy");
    sim.strlensym = sym_insert_id("strlen");
    mallocsym = sym_insert_id("malloc");
    callocsym = sym_insert_id("calloc");
    reallocsym = sym_insert_id("realloc");

    sim.dpow = sym_insert_id("pow");
    sim.dceil = sym_insert_id("ceil");
    sim.dfloor = sym_insert_id("floor");
    sim.dmod = sym_insert_id("fmod");
    sim.dabs = sym_insert_id("fabs");

/* _word(nnn) is a specially-treated 'function' to put nnn in-line in the */
/* generated code.  People may have views on a better name for it, esp.   */
/* in view of machines with byte and halfword instructions!               */
/* Introduced by ACN to help him with an 88000 library.                   */
    sim.inserted_word = library_function("_word", 1, 1, 0L);
    add_toplevel_binder(exb_(arg1_(sim.inserted_word)));        /* @@@?   */

#ifdef TARGET_LINKER_OMITS_DOLLAR
    stackoverflow = sym_insert_id("__stack_overflow");
    stack1overflow = sym_insert_id("__stack_overflow_1");
#else
#if defined(TARGET_IS_ARM_OR_THUMB) && !defined(OBSOLETE_ARM_NAMES)
    stackoverflow  = sym_insert_id(TARGET_PREFIX("__rt_stkovf_split_small"));
    stack1overflow = sym_insert_id(TARGET_PREFIX("__rt_stkovf_split_big"));
#else
    stackoverflow = sym_insert_id("x$stack_overflow");
    stack1overflow = sym_insert_id("x$stack_overflow_1");
#endif
#endif
    datasegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('d')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("__dataseg"),
#else
                sym_insert_id("x$dataseg"),
#endif
#endif
                bitofstg_(s_static),
                te_void);
    codesegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('c')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("__codeseg"),
#else
                sym_insert_id("x$codeseg"),
#endif
#endif
                bitofstg_(s_static),
                te_void);
#ifdef TARGET_HAS_BSS
    bsssegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('z')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("_bssseg"),
#else
                sym_insert_id("x$bssseg"),
#endif
#endif
                bitofstg_(s_static),
                te_void);
#endif
    /* C++ only really */
    ddtorsegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('v')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("_ddtorvec"),
#else
                sym_insert_id("x$ddtorvec"),
#endif
#endif
                bitofstg_(s_static),
                te_void);
    constdatasegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('q')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("_constdata"),
#else
                sym_insert_id("x$constdata"),
#endif
#endif
                bitofstg_(s_static)|u_constdata,
                te_void);
    extablesegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('t')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("_extable"),
#else
                sym_insert_id("x$extable"),
#endif
#endif
                bitofstg_(s_static),
                te_void);
    exhandlersegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('h')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("_exhandler"),
#else
                sym_insert_id("x$exhandler"),
#endif
#endif
                bitofstg_(s_static),
                te_void);
    mainsym = sym_insert_id("main");
    setjmpsym = sym_insert_id("setjmp");
    assertsym = sym_insert_id("___assert");
/* AM: hmm, is the name '___assert right in that users might get to see */
/* it if (say) a semicolon is omitted (check macro which use) and       */
/* query the next line which would mean ___assert without () fn call    */
/* would not get reported, or be done confusingly.  Probably OK.        */
    implicit_decl(assertsym, 1);    /* forge an 'extern int ___assert()' */
    first_arg_sym = sym_insert_id("___first_arg");
    last_arg_sym = sym_insert_id("___last_arg");
    libentrypoint = sym_insert_id("__main");
#ifdef TARGET_LINKER_OMITS_DOLLAR
    countroutine = sym_insert_id("__mcount");/*for Unix, x$ goes*/
#else
    countroutine = sym_insert_id("x$mcount");/*for Unix, x$ goes*/
#endif
    count1routine = sym_insert_id("_count1");
#ifdef RANGECHECK_SUPPORTED
#ifdef PASCAL /*ECN*/
    sim.abcfault = sym_insert_id("_range");
    sim.valfault = sym_insert_id("_badvalue");
#else
    sim.abcfault = sym_insert_id("__range"); /* BSD F77 library name */
# ifdef TARGET_LINKER_OMITS_DOLLAR
    sim.valfault = sym_insert_id("__badvalue");
# else
    sim.valfault = sym_insert_id("x$badvalue");
#endif
#endif
#endif
    traproutine    = sym_insert_id("__syscall");
    targeterrno    = sym_insert_id("errno");

    if (LanguageIsCPlusPlus)
        builtin_init_cpp();
    else
    {   te_boolean = te_int;
        te_wchar = initprimtype_(wchar_typespec);
        te_stringchar = te_char;
        te_wstringchar = te_wchar;
#ifdef EXTENSION_UNSIGNED_STRINGS
        te_ustringchar = initprimtype_(bitoftype_(s_unsigned)|bitoftype_(s_char));
#endif
    }

    lit_false = globalize_integer(te_boolean, NO);
    lit_true = globalize_integer(te_boolean, YES);
    lit_zero = globalize_integer(te_int, 0);
    lit_one = globalize_integer(te_int, 1);
}

/* end of builtin.c */
