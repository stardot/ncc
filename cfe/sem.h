/*
 * sem.c: semantic analysis phase of C compiler
 * Copyright (C) Codemist Ltd, 1988-1992.
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Ltd., 1990-1992.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _sem_h
#define _sem_h

#ifndef _defs_LOADED
#  include "defs.h"
#endif

#define isvolatile_expr(e) (isvolatile_type(typeofexpr(e)))
#define isunaligned_expr(e) (isunaligned_type(typeofexpr(e)))

#ifdef PASCAL /*ECN*/
#define ptrtotype_(t) ((TypeExpr *)syn_list5(t_content, (t), 0, 0, \
                                                    syn_list2(s_content, 0)))
#else
#define ptrtotype_(t) mk_typeexpr1(t_content, t, 0)
#endif
/* The next line is a type-check cheat -- casts of int to (TypeExpr *) !! */
#define primtype_(m)  mk_typeexpr1(s_typespec, (TypeExpr *)(IPtr)(m), 0)
#define primtype2_(m,b) \
                mk_typeexpr1(s_typespec, (TypeExpr *)(IPtr)(m), (Expr *)(b))
#define isprimtype_(x,s) (h0_(x) == s_typespec && \
                          (typespecmap_(x) & bitoftype_(s)))
#define isprimtypein_(x,set) (h0_(x) == s_typespec && \
                              (typespecmap_(x) & (set)))
#define isclasstype_(x)  isprimtypein_(x, CLASSBITS)
#define isclassorenumtype_(x)  isprimtypein_(x, ENUMORCLASSBITS)
#define issimpletype_(x) (h0_(x) == s_typespec && \
                          !(typespecmap_(x) & CLASSBITS))
/* Note, in contexts where default promotions have been done, that      */
/* the s_char/s_enum bits in the following tests are irrelevant.        */
/* Apr 92: see the now finer grain coerceunary() for C++ (and C).       */
#define INTEGRALTYPEBITS (bitoftype_(s_bool)|bitoftype_(s_char)|\
                bitoftype_(s_int)|bitoftype_(s_enum))
#define ARITHTYPEBITS (INTEGRALTYPEBITS|bitoftype_(s_double))
#define isintegraltype_(x) isprimtypein_(x, INTEGRALTYPEBITS)
#define isarithtype_(x) isprimtypein_(x, ARITHTYPEBITS)
#define isfntype(x)   (h0_(princtype(x)) == t_fnap)

#define istypevar(x)    (h0_(princtype(x)) == t_unknown)

bool isvoidtype(TypeExpr *t);
bool isptrtovoidtype(TypeExpr *t);
bool isnullptrconst(const Expr *e);

extern Expr *errornode;

extern void typeclash(AEop op);

extern bool issimplevalue(Expr *e);
extern bool issimplelvalue(Expr *e);
extern Expr *ensurelvalue(Expr *e, AEop op);

extern TypeExpr *prunetype(TypeExpr *t), *princtype(TypeExpr *t);
extern SET_BITMAP qualifiersoftype(TypeExpr *t);
SET_BITMAP recursivequalifiers(TypeExpr *x);
extern TypeExpr *mkqualifiedtype(TypeExpr *t, SET_BITMAP qualifiers);

extern bool isvolatile_type(TypeExpr *x);
extern bool isunaligned_type(TypeExpr *x);
extern bool isbitfield_type(TypeExpr *x);
extern TypeExpr *unbitfield_type(TypeExpr *x);
extern TagBinder *isclassenumorref_type(TypeExpr *x);
extern bool pointerfree_type(TypeExpr *t);

extern TypeExpr *typeofexpr(Expr *x);

extern SET_BITMAP typeofenumcontainer(TagBinder *tb);

extern bool structfield(ClassMember *l, int32 sort, StructPos *p);
/* Used to iterate over the fields of a structure (union).  Returns fields
   in p describing the head field in l, and updates the internal fields
   in p to point past that element.
 */
extern void structpos_init(StructPos *p, TagBinder *tb);
/* Prepare a StrucPos object for use by structfield */

extern int32 sizeofclass(TagBinder *b, bool *padded);
extern int32 sizeoftypenotepadding(TypeExpr *x, bool *padded);

#define sizeoftype(t) sizeoftypenotepadding(t, NULL)

extern bool sizeoftypelegal(TypeExpr *x);
 /* !is_incompletetype, really */

extern int32 alignoftype(TypeExpr *x);

/* equivtype functions return 0=differ, 1=equivalent for C, 2=identical */
extern int equivtype(TypeExpr *t1, TypeExpr *t2);
extern int qualfree_equivtype(TypeExpr *t1, TypeExpr *t2);
extern int widened_equivtype(TypeExpr *t1, TypeExpr *t2);

extern TypeExpr *modify_formaltype(TypeExpr *t);
extern TypeExpr *widen_formaltype(TypeExpr *t);
extern TypeExpr *promoted_formaltype(TypeExpr *t);
extern TypeExpr *modify_actualtype(TypeExpr *t, const Expr* e);

extern Expr *mkintconst(TypeExpr *te, int32 n, Expr *e);

extern Expr *fixflt(Expr *e);

extern void moan_nonconst(Expr *e, msg_t nonconst_msg,
                          msg_t nonconst1_msg, msg_t nonconst2_msg);

extern Expr *coerceunary(Expr*);
extern Expr *mkintegral(AEop op, Expr *a);
extern Expr *mktest(AEop opreason, Expr *a);
extern Expr *mkunary(AEop op, Expr *a);
extern Expr *mkbinary(AEop op, Expr *a, Expr *b);
extern Expr *mkfnap(Expr *e, ExprList *l);
extern Expr *mkcast(AEop op, Expr *e, TypeExpr *tr);
extern Expr *skip_invisible_or_cast(Expr *e);
extern Expr *skip_invisible(Expr *e);
Expr *mk1fnap(Expr *fn, ExprList *args);

extern Expr *rooted_path(Expr *path, SET_BITMAP qualifiers, Expr *root);
extern Expr *mkfieldselector(AEop op, Expr *e, ClassMember *mem);

extern Expr *mkcond(Expr *a, Expr *b, Expr *c);

extern void sem_init(void);

#ifdef CPLUSPLUS

extern TypeExpr *core_type(TagBinder *b);
extern TagBinder *core_class(TagBinder *b);
extern VfnList *vfn_list(TagBinder *cl);
extern bool typehasctor(TypeExpr *t);
extern Expr *thisify(Expr *e);
extern ClassMember *type_derived_from(TypeExpr *tr, TypeExpr *te);
extern Expr *mkopap(AEop op, TagBinder *tb, Expr *a, ExprList *l);
extern Binder *class_has_conversion(TagBinder *b, TypeExpr *t, int pick_best);
extern Binder *class_has_ctor(TagBinder *b, TypeExpr *t, const Expr *e, int pick_best);
extern Binder *class_has_ctor_1(TagBinder *b, TypeExpr *t, const Expr *e, int pick_best,
                                bool silent);
extern Expr *user_conversion(Expr *e, TagBinder *b, TypeExpr *t);

extern Expr *commacons(Expr *a, Expr *b);
extern Expr *mkcommalist(Expr *a, ...);
extern Expr *vbase_init(TagBinder *cl, Expr *var);
extern Expr *vtab_init(TagBinder *cl, Expr *var);

extern bool isarraytype(TypeExpr *t, TypeExpr **elmtype);
extern int32 arraysize(TypeExpr *t, TypeExpr **elmtype); /* @@@ should return size_t */

extern Expr *mkctor_v(Expr *b, ExprList *init);
extern Expr *mkctor_t(TypeExpr *t, ExprList *init);
extern Expr *mkdtor_v(Expr *e);
extern Expr *mkdtor_v_list(SynBindList *d);
extern Cmd *mkthrow(FileLine fl, Expr *e);
extern Cmd *mkexpop(FileLine fl);
extern Cmd *mkextop(FileLine fl);
extern Expr *mkexpush(FileLine fl, Expr* typeid, Expr* size);
extern Expr *array_of_class_map_expr(Expr *p, Expr *nelts, int32 stride, Expr *mapfn);
extern Expr *array_of_class_map(Expr *p, int32 nelts, int32 stride, bool up,
                                Expr *mapfn, bool twoargmapfn);
extern Expr *array_of_class_copy(Expr *to, Expr *fm, Expr *frmlimit, int32 stride,
        Binder *mapfn);
extern Expr *mknew(TypeExpr *newt, TypeExpr *t, Binder *newgeneric, Expr* nelts,
                   ExprList* placement, ExprList* init, bool forceglobal);
extern Expr *mkdelete(Expr* e, TypeExpr* t, bool isarray, Binder *delgeneric,
                      bool forceglobal);

extern bool cfront_allows_pointercast(TypeExpr *te, TypeExpr *tr);
extern TypeExpr *lvalue_type(Expr *x);
extern void fixup_template_arg_type(TypeExpr *f, ClassMember *actuals);
extern ScopeSaver globalize_template_arg_binders(ScopeSaver f, ExprList *a);
extern bool contains_typevars(TypeExpr *t);
extern bool has_template_parameter(ScopeSaver tformals);
extern TypeExpr *type_deduction(TypeExpr *fntype, ExprList *args, ExprList *actuals,
                                ScopeSaver *tformals, bool silent, bool *failed);
extern TagBinder *class_template_reduce(TagBinder *primary, TagBindList *instances,
                                        ScopeSaver tactuals);
extern void check_temp_arg_linkage(Expr *e);
extern void check_temp_type_arg_linkage(TypeExpr *t);
extern BindList *temp_reduce(BindList *temps, ExprList *l, ExprList *tactuals, Binder *bgeneric);
extern int env_size(Binder *b);
extern bool is_an_instance(BindList *ins, Binder *b);
extern ScopeSaver dup_env_actuals(ScopeSaver tformals, ExprList *a);
extern void add_instance(Binder *b, BindList **bl, bool su);
extern bool is_dependent_type(TypeExpr *t);

/* nasty: routines for overload.c */
extern Symstr *ovld_add_memclass(Symstr *sv, TagBinder *scope, bool staticfn);
extern Symstr *ovld_instance_name(Symstr *sv, TypeExpr *t);
extern Symstr *ovld_instance_name_1(const char *name, TypeExpr *t);
extern Symstr *ovld_tmptfn_instance_name(Symstr *sv, ScopeSaver f);
extern Expr *ovld_picknullary(Binder *bgeneric);
extern List *mk_candidates(BindList *, int, int, List *);
extern List *mk_operator_candidates(AEop, TypeExpr *, TypeExpr *, List *);
extern Binder *ovld_reduce(Binder *b, List *, ExprList *l, ExprList *ll);
extern Binder *ovld_resolve(Binder *b, BindList *alternatives,
                            ExprList *l, ExprList *ll, bool silent);
extern Symstr *specialized_name_of(TagBinder *tmptb);
extern bool is_operator_name(Symstr *opname);
extern Symstr *ovld_template_app(Symstr *sv, ScopeSaver f, ExprList *a);
extern Symstr *ovld_function_template_name(Symstr *sv, TypeExpr *t);
extern Symstr *operator_name(AEop op);
extern Symstr *conversion_name(TypeExpr *t);
extern String *exception_name(TypeExpr *t);
extern Expr *allowable_boolean_conversion(Expr *);
extern bool comparable_templates(TagBinder *tb1, TagBinder *tb2);
extern void merge_default_template_args(ScopeSaver new, TagBinder *tempclass, Binder *tempfn);

/* even nastier: really defined in xsyn.c... FIX LATER */
extern void diagnose_access(Symstr *sv, TagBinder *scope);
extern Expr *cpp_mkbinaryorop(AEop, Expr*, Expr*);

#else

#define commacons(a,b) 0
#define core_type(b) 0
#define core_class(b) (b)
#define vfn_list(cl) 0
#define typehasctor(t) 0
#define thisify(e) 0
#define type_derived_from(tr, te) 0
#define mkopap(opname, tb, a, l) 0
#define exists_conversion_sequence_from(tfm,tto) 0
#define mkfnap_cpp(e, l, curried, let, firstarg) (e)
#define add_instance(a,b,c) ((void)0)
#define is_dependent_type(a) 0
#define merge_default_template_args(a,b,c) 0

#define mkctor_v(b, init) 0
#define mkdtor_v(e) 0
#define mkdtor_v_list(d) 0
#define mkthrow(fn, e) 0
#define mkexpop(fl) 0
#define mkextop(fl) 0
#define mkexpush(fl,ti,ts) 0

#define cfront_allows_pointercast(te, tr) 0

/* nasty: routines for overload.c */
#define ovld_add_memclass(sv, scope, staticfn) 0
#define ovld_instance_name(sv, t) 0
#define ovld_resolve(b, alternatives, l, ll, silent) 0
#define operator_name(op) 0
#define conversion_name(t) 0
#define exception_name(t) 0
#define contains_typevars(t) 0
#define has_template_parameter(t) 0
#define type_deduction(a,b,c,d,e,f) (a)
#define class_template_reduce(a,b,c) (a)
#define fixup_template_arg_type(a,b) ((void)0)
#define check_temp_type_arg_linkage(a) 0
#define check_temp_arg_linkage(a) 0
#define temp_reduce(a,b,c,d) a
#define is_an_instance(a,b) 0
#define globalize_template_arg_binders(a,b) 0
#define specialized_name_of(a) 0
#endif

#endif

/* end of cfe/sem.h */
