/*
 * cfe/syn.h
 * Copyright (C) Acorn Computers Ltd., 1988
 * Copyright (C) Codemist Ltd., 1988-1993
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _syn_h
#define _syn_h

#ifndef _defs_LOADED
#  include "defs.h"
#endif

extern bool syn_hashif(void);
extern bool implicit_return_ok;

extern int32 syn_begin_agg(void);
extern void syn_end_agg(int32 beganbrace);
extern Expr *syn_rdinit(TypeExpr *t, Binder *whole, int32 flag);
extern bool syn_canrdinit(void);

extern Expr *rd_expr(int n);
extern Expr *rd_ANSIstring(void);
extern TopDecl *rd_topdecl(bool);
extern void syn_init(void);
#ifdef CPLUSPLUS
extern void xsyn_reinit(void);
#define syn_reinit() xsyn_reinit()
#else
#define syn_reinit() ((void)0)
#endif

extern AEop peepsym(void);
extern void checkfor_ket(AEop s);
extern void checkfor_delimiter_ket(AEop s, msg_t expected1, msg_t expected1a);
extern void checkfor_2ket(AEop s, AEop t);
extern void checkfor_delimiter_2ket(AEop s, AEop t);

#define PASTCOMMA 10
#define UPTOCOMMA 11
#define UPTORELOP 29

#ifdef CPLUSPLUS
extern int recursing;           /* check by mkctor_v_1 but save/restore on
                                   function context changes */
extern void add_pendingfn(Symstr *name, Symstr *realname, TypeExpr *t, SET_BITMAP stg,
                          TagBinder *scope, ScopeSaver formaltags, int tokhandle,
                          ScopeSaver templateformals, bool tfn);
extern void syn_attempt_template_memfn(Binder *generic, Binder *specific);
extern TagBinder *syn_implicit_instantiate_2(TagBinder *tb);
extern ScopeSaver copy_env(ScopeSaver env, int n);
extern void parameter_names_transfer(FormTypeList *from, FormTypeList *to);
extern void add_expr_dtors(Expr *edtor);
extern void add_to_saved_temps(SynBindList *tmps);
#else
#define add_pendingfn(a,b,c,d,e,f,g,h,i) ((void)0)
#define copy_env(a,b) 0
#define parameter_names_transfer(a,b) 0
#define add_expr_dtors(a) 0
#define add_to_saved_temps(a) 0
#endif

#endif

/* end of cfe/syn.h */
