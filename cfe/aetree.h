/*
 * aetree.h - AE Tree constructor and print functions.
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Codemist Ltd., 1987-1992.
 * Copyright (C) Advanced RISC Machines Limited, 1991-1992.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$ Codemist 15
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _aetree_h
#define _aetree_h

#ifndef _defs_LOADED
#  include "defs.h"
#endif

/* NB: it is unclear that the TypeExpr creation fns should be in this   */
/* mip file -- TypeExpr's are moving to being a cfe local (opaque)      */
/* whose interface to mip is via mcrepoftype.                           */

#define mk_typevar()    ((TypeExpr *)global_cons2(SU_Type, t_unknown, 0))

extern AEop tagbindsort(TagBinder *b);

extern Expr *mk_expr1(AEop op, TypeExpr *t, Expr *a1);

extern Expr *mk_expr2(AEop op, TypeExpr *t, Expr *a1, Expr *a2);

extern Expr *mk_exprlet(AEop op, TypeExpr *t, SynBindList *a1, Expr *a2);

extern Expr *mk_expr3(AEop op, TypeExpr *t, Expr *a1, Expr *a2, Expr *a3);

#ifdef EXTENSION_VALOF
extern Expr *mk_expr_valof(AEop op, TypeExpr *t, Cmd *c);
#endif

#ifdef TARGET_HAS_INLINE_ASSEMBLER
extern AsmInstr *mk_asminstr(void);
#endif

extern Expr *mk_exprwdot(AEop op, TypeExpr *t, Expr *a1, IPtr a2);

extern Expr *mk_exprbdot(AEop op, TypeExpr *t, Expr *a1, int32 a2, int32 a3,
                         int32 a4);

/* mkDeclRhsList is slowly dying in C++ (syntax aid only).              */
extern DeclRhsList *mkDeclRhsList(Symstr *sv, TypeExpr *t, SET_BITMAP s);

extern TopDecl *mkTopDeclFnDef(AEop a, Binder *b, SynBindList *c,
                               Cmd *d, bool e);

extern TypeExpr *mk_typeexpr1(AEop op, TypeExpr *t, Expr *a1);
extern TypeExpr *mkTypeExprfn(AEop a, TypeExpr *b, SET_BITMAP s,
                              FormTypeList *c, const TypeExprFnAux *d);
extern TypeExpr *g_mkTypeExprfn(AEop a, TypeExpr *b, SET_BITMAP s,
                                FormTypeList *c, const TypeExprFnAux *d);

extern FormTypeList *mkFormTypeList(FormTypeList *ftcdr, Symstr *ftname,
                                    TypeExpr *fttype, Expr *ftdefault);
extern FormTypeList *g_mkFormTypeList(FormTypeList *ftcdr, Symstr *ftname,
                                      TypeExpr *fttype, Expr *ftdefault);

/* temporary home, pending demise... */
extern int32 evaluate(Expr *a);

extern Expr *globalize_int(int32 n);
extern FormTypeList *globalize_formals(FormTypeList *d);

/* globalize_typeexpr caches only basic types (including structs/typedefs) */
/* and pointers to things which are already cached.  Tough ched arrays/fns */
extern TypeExpr *globalize_typeexpr(TypeExpr *t);
/* and this omits default argument values to avoid a redeclaration by a */
/* postponed class function definition                                  */
extern TypeExpr *globalize_typeexpr_no_default_arg_vals(TypeExpr *t);

extern StringSegList *globalize_strseg(StringSegList *s);

extern Expr *globalize_expr(Expr *e);

extern ScopeSaver globalize_env(ScopeSaver env);

extern TypeExpr *clone_typeexpr(TypeExpr *t);

extern void aetree_init(void);

extern Cmd *mk_cmd_0(AEop op, FileLine x);
  /* op = s_break,s_endcase,s_continue */

extern Cmd *mk_cmd_e(AEop op, FileLine x, Expr *e);
  /* op = s_return,s_semicolon */

extern Cmd *mk_cmd_default(FileLine x, Cmd *c);

extern Cmd *mk_cmd_lab(AEop op, FileLine x, LabBind *b, Cmd *c);

extern Cmd *mk_cmd_block(FileLine x, SynBindList *bl, CmdList *cl);

extern Cmd *mk_cmd_do(FileLine x, Cmd *c, Expr *e);

extern Cmd *mk_cmd_if(FileLine x, Expr *e, Cmd *c1, Cmd *c2);

extern Cmd *mk_cmd_switch(FileLine x, Expr *e, Cmd *c1, Cmd *c2, Cmd *c3);

extern Cmd *mk_cmd_for(FileLine x, Expr *e1, Expr *e2, Expr *e3, Cmd *c);

extern Cmd *mk_cmd_case(FileLine x, Expr *e, Cmd *c1, Cmd *c2);

extern Cmd *mk_cmd_try(FileLine x, Cmd *body, Handler *handler_list, Cmd *exit);

extern bool is_fpzero(Expr const *e);
extern bool is_fpone(Expr const *e);
extern bool is_fpminusone(Expr const *e);

extern int32 result2;
extern bool integer_constant(Expr const *e);

extern bool is_intzero(Expr const *e);
extern bool is_intone(Expr const *e);
extern bool is_intminusone(Expr const *e);
extern bool resultinflags_fn(Expr *e);

extern void eprintf(char const *s, ...);

extern void pr_typeexpr(TypeExpr *x, Symstr *s);

extern void pr_stringsegs(StringSegList *z);

void pr_int64(int64 const *x);

extern void pr_expr(Expr *x);

extern void pr_exproftype(char const *s, Expr *x);

extern void pr_cmd(Cmd *c);

extern void pr_topdecl(TopDecl *x);

extern void pr_typeexpr_nl(TypeExpr *x, Symstr *s);

extern void pr_expr_nl(Expr *x);

#endif

/* end of aetree.h */
