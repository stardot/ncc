/*
 * mip/bind.h:
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Codemist Ltd., 1987-1992.
 * Copyright (C) Advanced RISC Machines Limited, 1991-1992.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _bind_h
#define _bind_h

#ifndef _defs_LOADED
#  include "defs.h"
#endif

/* The next three lines interface syn.c and bind.c: see syn.c for more.  */
#define DUPL_OK            0x001
#define TOPLEVEL           0x002      /* Global lexical scope and global */
                                      /* storage scope...                */
#define GLOBALSTG          0x004      /* Not global lexical scope, but   */
                                      /* global storage scope for e.g.   */
                                      /* types defined in argument lists */
#define LOCALSCOPE         0x000      /* Local lexical and storage scope */

#define TEMPLATE          0x1000      /* OR-able (but only with TOPLEVEL)*/
                                      /* (probably really a stg bit).    */
#define SPECIALIZE        0x10000     /* template <> ... */
#define INSTANTIATE       0x20000     /* template struct ... */
#define INSTANCE          0x40000     /* template fn instance */

extern const char *sym_name_table[];  /* translation back to strings  */

#define SYM_LOCAL          0
#define SYM_GLOBAL         1

extern Symstr *(sym_lookup)(char const *name, int glo);

extern Symstr *sym_insert(char const *name, AEop type);

extern Symstr *sym_insert_id(char const *name);

extern Symstr *gensymval(bool glo);
extern Symstr *gensymvalwithname(bool glo, char const *name);

extern bool isgensym(Symstr const *sym);

extern Binder *global_mk_binder(Binder *b, Symstr *c, SET_BITMAP d,
                                TypeExpr *e);
extern Binder *mk_binder(Symstr *c, SET_BITMAP d, TypeExpr *e);
extern TagBinder *global_mk_tagbinder(TagBinder *b, Symstr *c, AEop d);
extern TagBinder *mk_tagbinder(Symstr *c, AEop d);
extern TagBinder *clone_tagbinder(Symstr *newname, TagBinder *b);
extern LabBind *mk_labbind(LabBind *b, Symstr *c);

#define gentempbinderwithname(typ, name) \
  mk_binder(gensymvalwithname(0, name), bitofstg_(s_auto), typ)

#define gentempbinder(typ) \
  mk_binder(gensymval(0), bitofstg_(s_auto), typ)

#define genglobinder(typ) \
  global_mk_binder(0, gensymval(1), bitofstg_(s_static), typ)

extern void add_toplevel_binder(Binder *b);

typedef enum {
  TD_NotDef,
  TD_ContentDef,
  TD_Decl
} TagDefSort;

/* The distinction between TD_ContentDef and TD_Decl is to allow
     struct a { int x; }; struct a;
   but forbid
     struct a { int x; }; struct a { anything };
   @@@ AM: it may be subsumable into the TB_DEFD needed for C++ (q.v.)
 */

typedef enum {
  Scope_Ord,
  Scope_Args,
  Scope_TemplateArgs,
  Scope_TemplateDef
} ScopeSort;

typedef struct Scope Scope;
struct Scope {
    Scope *prev;
    Binder *scopemems;          /* A scope either contains Binders      */
                                /* and TagBinders (a local scope)       */
                                /* and members too (class scope).       */
                                /* (unused if class_tag != 0).          */
    TagBinder *class_tag;
    ScopeSort kind;
};

extern int push_scope(TagBinder *class_tag, ScopeSort kind);
extern int push_var_scope(ScopeSaver init, ScopeSort kind);

#ifdef CALLABLE_COMPILER
extern void set_local_scope(Scope *scope);
#endif

extern ScopeSaver pop_scope(int);
extern ScopeSaver pop_scope_no_check(int);

#define FB_INHERIT   1
#define FB_LOCALS    2
#define FB_CLASSES   4
#define FB_GLOBAL    8
#define FB_FNBINDER  32       /* return a fn binder, not an exprdotmemfn */
#define FB_KEEPI     64       /* keep the top invisible node, if any */
#define FB_TYPENAME  128
#define FB_CLASSNAME 256
#define FB_CIV       512
#define FB_NOTYET    1024
#define FB_THISSCOPE 2048
#define LOCALSCOPES  FB_LOCALS
#define ALLSCOPES    (FB_LOCALS+FB_CLASSES+FB_GLOBAL+FB_INHERIT)
#define INDERIVATION FB_INHERIT
#define INCLASSONLY  0

extern ClassMember *curlex_member;  /* @@@ Think about a better location  */
                                    /* for this decl, defined in syn.c... */
extern int derivation_level;
extern Expr *findpath(Symstr *sv, TagBinder *cl, int flags, TagBinder *inBase);
extern Binder *findbinding(Symstr *sv, TagBinder *cl, int flags);
extern Expr *path_to_member(ClassMember *member, TagBinder *cl, int flags);
extern TagBinder *findtagbinding(Symstr *sv, TagBinder *cl, int flags);

/*
 * In the following, bindflg takes any combination of TOPLEVEL + GLOBALTAG.
 */
extern TagBinder *instate_tagbinding(Symstr *sv, AEop s, TagDefSort defining,
        int bindflg, bool *newtag);

extern void instate_alias(Symstr *a, Binder *b);

extern Binder *instate_declaration(DeclRhsList *d, int declflag);

extern ClassMember *instate_member(DeclRhsList *d, int bindflg);

extern Binder *implicit_decl(Symstr *a, int32 fn);

extern void reset_vg_after_init_of_tentative_defn(void);
void save_vargen_state(TentativeDefn *td);

extern LabBind *label_define(Symstr *id);
extern LabBind *label_reference(Symstr *id);
extern void label_resolve(void);
extern void save_labels(LabBind **lc, LabBind **symlabs);
extern void restore_labels(LabBind *lc, LabBind **symlabs);

extern void bind_cleanup(void);
extern void bind_init(void);

#ifdef CPLUSPLUS
ClassMember *mk_member(TagBinder *basename, TypeExpr *t,
                       SET_BITMAP attributes, TagBinder *parent);
ClassMember *mk_core_part(TagBinder *cl, ClassMember *bases,
                          ClassMember *vbases);
extern int accessOK;
extern void check_access(ClassMember *m, TagBinder *privately_deriving_class);
extern int push_multi_scope(TagBinder *class_tag);
extern TypeExpr *memfn_realtype(TypeExpr *fntype, TagBinder *cl);
extern TypeExpr *add_delete_arg(TypeExpr *fntype, bool is_declaration);
extern Binder *instate_memfn(Symstr *fsv, TypeExpr *t);
extern TagBinder *current_member_scope(void);
extern TagBinder *set_access_context(TagBinder *cl, Binder *fn);
extern void mk_friend_class(TagBinder *classtag, TagBinder *ofclass);
extern void mk_friend_fn(Binder *bspecific, TagBinder *ofclass);
/* derived_from is logically 'bool' but returns the base ClassMember:   */
extern ClassMember *derived_from(TagBinder *base, TagBinder *scope);
/* same class or derived_from */
extern bool derived_fromeq(TagBinder *base, TagBinder *scope);

/* The following provide for-style iterators for C++ classes which      */
/* can steer round CB_CORE etc.                                         */
typedef struct ClassMemo { struct ClassMember *outer, *core; } ClassMemo;
extern ClassMember *ClassMemo_next(ClassMemo *m);
extern ClassMember *ClassMemo_first(ClassMemo *m, ClassMember *p);
#define forClassMember(p,i,m) \
    for (p = ClassMemo_first(m, i); p != 0; p = ClassMemo_next(m))
extern void push_exprtemp_scope(void);     /* @@@ implemented in xsyn.c */
extern void pop_exprtemp_scope(void);      /* @@@ implemented in xsyn.c */
extern Binder *genexprtemp(TypeExpr *t);   /* ditto */
extern Expr *killexprtemp(void);           /* @@@ ditto                 */
extern int killnexprtemp(Binder *);
extern Binder *genreftemp(TypeExpr *t);    /* @@@ implemented in xsyn.c */
extern int killreftemp(Binder *b);         /* @@@ implemented in xsyn.c */
                     /* declaration here avoids sem.c:syn.h dependency. */
extern Binder *instate_classname(DeclRhsList *d, TagBinder *tb);
extern bool is_two_arg_delete(Binder* delgeneric);
extern bool is_local_binder(Binder *b);
extern bool is_template_arg_binder(Expr *e);
extern bool has_template_arg_scope(void);
extern BindList *clone_bindlist(BindList * bl, bool glob);
extern ScopeSaver dup_template_scope(void);
extern bool no_access_context(void);
extern void check_access_1(ClassMember *l, TagBinder *scope);
extern void push_saved_temps(int32 scopeid);
extern SynBindList *pop_saved_temps(SynBindList *bl);
extern void syn_note_generated_fn(TopDecl *d);
extern int32 base_vtable_sz(TagBinder *b);
extern TagBindList *rootOfPath;
#else

#define current_member_scope()               ((TagBinder *)0)
#define derived_from(b, s)                   ((ClassMember *)0)
#define path_to_base_member(m, t, f, v, pdc) 0
#define path_to_member_2(m, b, f, l, pdc)    0
#define instate_member_cpp(d, b)             0
#define instate_declaration_cpp(d, f)        0
#define genreftemp(t)                        ((Binder *)0)
#define killreftemp(b)                       0
#define genexprtemp(t)                       ((Binder *)0)
#define killexprtemp()                       0
#define push_exprtemp_scope()                ((void)0)
#define pop_exprtemp_scope()                 ((void)0)
#define push_saved_temps(id)                 ((void)0)
#define pop_saved_temps(bl)                  (bl)
#define syn_note_generated_fn(a)             ((void)0)
#define mk_friend_class(a, b)                (void)0
#define is_local_binder(a)                   (bool)0
#define is_template_arg_binder(a)            (bool)0
#define has_template_arg_scope()             (bool)0
#define dup_template_scope()                 (ScopeSaver)0
#define clone_bindlist(a,b)                  (BindList *)a

#endif

#endif

/* end of mip/bind.h */
