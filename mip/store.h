/*
 * mip/store.h, version 6
 * Copyright (C) Codemist Ltd., 1988.
 * Copyright (C) Acorn Computers Ltd., 1988.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

/* There are four classes or storage:
     PermAlloc:  permanent (for cmd-line arguments, etc)
     GlobAlloc:  per translation unit
     BindAlloc:  until alloc_reinit
     SynAlloc:   until drop_local_store

     alloc_mark/alloc_unmark can be used to hold keep Syn/Bind store
     past it's normal life.
*/

/* AM memo: I would like to improve the security of the following by   */
/* discouraging the use of macros involving casts (or void *).         */

#ifndef _store_LOADED
#define _store_LOADED 1

extern void ClearToNull(void **a, int32 n);

typedef enum {
    SU_Data,
    SU_Xref,
    SU_Xsym,
    SU_Sym,
    SU_Bind,
    SU_Type,
    SU_Const,
    SU_PP,
    SU_Dbg,
    SU_Inline,
    SU_Other
} StoreUse;

#define global_cons2(t,a,b) xglobal_cons2(t,(IPtr)(a),(IPtr)(b))
#define global_list3(t,a,b,c) xglobal_list3(t,(IPtr)(a),(IPtr)(b),(IPtr)(c))
#define global_list4(t,a,b,c,d) xglobal_list4(t,(IPtr)(a),(IPtr)(b),(IPtr)(c),(IPtr)(d))
#define global_list5(t,a,b,c,d,e) xglobal_list5(t,(IPtr)(a),(IPtr)(b),(IPtr)(c),(IPtr)(d),(IPtr)(e))
#define global_list6(t,a,b,c,d,e,f) xglobal_list6(t,(IPtr)(a),(IPtr)(b),(IPtr)(c),(IPtr)(d),(IPtr)(e),(IPtr)(f))

extern VoidStar xglobal_cons2(StoreUse t, IPtr a, IPtr b);
extern VoidStar xglobal_list3(StoreUse t, IPtr a, IPtr b, IPtr c);
extern VoidStar xglobal_list4(StoreUse t, IPtr a, IPtr b, IPtr c, IPtr d);
extern VoidStar xglobal_list5(StoreUse t, IPtr a, IPtr b, IPtr c, IPtr d, IPtr e);
extern VoidStar xglobal_list6(StoreUse t, IPtr a, IPtr b, IPtr c, IPtr d, IPtr e, IPtr f);
#ifdef CALLABLE_COMPILER
extern VoidStar xglobal_list7(StoreUse t, IPtr a, IPtr b, IPtr c, IPtr d, IPtr e, IPtr f, IPtr g);
#endif

extern VoidStar GlobAlloc(StoreUse t, int32 n);

#ifndef CALLABLE_COMPILER
extern VoidStar PermAlloc(int32 n);
extern VoidStar BindAlloc(int32 n);
extern VoidStar SynAlloc(int32 n);

extern VoidStar discard2(VoidStar p);
extern VoidStar discard3(VoidStar p);
#else
#define PermAlloc(n)    GlobAlloc(SU_Other,n)
#define BindAlloc(n)    GlobAlloc(SU_Other,n)
#define SynAlloc(n)     GlobAlloc(SU_Other,n)

#define discard2        discard
#define discard3        discard
extern VoidStar discard(VoidStar p);
#endif

#define NewPerm(T) ((T *)PermAlloc(sizeof(T)))
#define NewPermN(T,n) ((T *)PermAlloc((int32)sizeof(T)*(n)))
#define NewPermK(T,n) ((T *)PermAlloc((int32)sizeof(T)+(n)))

#define NewGlob(T,sort) ((T *)GlobAlloc((sort), sizeof(T)))
#define NewGlobN(T,sort,n) ((T *)GlobAlloc((sort), (int32)sizeof(T)*(n)))
#define NewGlobK(T,sort,n) ((T *)GlobAlloc((sort), (int32)sizeof(T)+(n)))

#define NewSyn(T) ((T *)SynAlloc(sizeof(T)))
#define NewSynN(T,n) ((T *)SynAlloc((int32)sizeof(T)*(n)))
#define NewSynK(T,n) ((T *)SynAlloc((int32)sizeof(T)+(n)))

#define NewBind(T) ((T *)BindAlloc(sizeof(T)))
#define NewBindN(T,n) ((T *)BindAlloc((int32)sizeof(T)*(n)))
#define NewBindK(T,n) ((T *)BindAlloc((int32)sizeof(T)+(n)))


#define syn_cons2(a, b) xsyn_list2((IPtr)(a), (IPtr)(b))
#define binder_cons2(a, b) xbinder_list2((IPtr)(a), (IPtr)(b))
#define binder_icons2(a, b) xbinder_list2((IPtr)(a), b)
#define binder_icons3(a, b, c) xbinder_list3((IPtr)(a), b, c)

#define syn_list2(a,b) xsyn_list2((IPtr)(a),(IPtr)(b))
#define syn_list3(a,b,c) xsyn_list3((IPtr)(a),(IPtr)(b),(IPtr)(c))
#define syn_list4(a,b,c,d) xsyn_list4((IPtr)(a),(IPtr)(b),(IPtr)(c),(IPtr)(d))
#define syn_list5(a,b,c,d,e) xsyn_list5((IPtr)(a),(IPtr)(b),(IPtr)(c),(IPtr)(d),(IPtr)(e))
#define syn_list6(a,b,c,d,e,f) xsyn_list6((IPtr)(a),(IPtr)(b),(IPtr)(c),(IPtr)(d),(IPtr)(e),(IPtr)(f))
#define syn_list7(a,b,c,d,e,f,g) xsyn_list7((IPtr)(a),(IPtr)(b),(IPtr)(c),(IPtr)(d),(IPtr)(e),(IPtr)(f),(IPtr)(g))


#ifndef CALLABLE_COMPILER
extern VoidStar xsyn_list2(IPtr a, IPtr b);
extern VoidStar xsyn_list3(IPtr a, IPtr b, IPtr c);
extern VoidStar xsyn_list4(IPtr a, IPtr b, IPtr c, IPtr d);
extern VoidStar xsyn_list5(IPtr a, IPtr b, IPtr c, IPtr d, IPtr e);
extern VoidStar xsyn_list6(IPtr a, IPtr b, IPtr c, IPtr d, IPtr e, IPtr f);
extern VoidStar xsyn_list7(IPtr a, IPtr b, IPtr c, IPtr d, IPtr e, IPtr f, IPtr g);
#else
#define xsyn_list2(a,b)         xglobal_cons2(SU_Other,a,b)
#define xsyn_list3(a,b,c)       xglobal_list3(SU_Other,a,b,c)
#define xsyn_list4(a,b,c,d)     xglobal_list4(SU_Other,a,b,c,d)
#define xsyn_list5(a,b,c,d,e)   xglobal_list5(SU_Other,a,b,c,d,e)
#define xsyn_list6(a,b,c,d,e,f) xglobal_list6(SU_Other,a,b,c,d,e,f)
#define xsyn_list7(a,b,c,d,e,f,g) xglobal_list7(SU_Other,a,b,c,d,e,f,g)
#endif

#define binder_list2(a,b) xbinder_list2((IPtr)(a),(IPtr)(b))
#define binder_list3(a,b,c) xbinder_list3((IPtr)(a),(IPtr)(b),(IPtr)(c))

#ifndef CALLABLE_COMPILER
extern VoidStar xbinder_list2(IPtr a, IPtr b);
extern VoidStar xbinder_list3(IPtr a, IPtr b, IPtr c);

typedef struct Mark Mark;
extern Mark* alloc_mark(void);
extern void alloc_unmark(Mark*);
extern void drop_local_store(void);
extern void alloc_reinit(void);

extern void alloc_noteAEstoreuse(void);
extern void show_store_use(void);

extern void alloc_perfileinit(void);
extern void alloc_perfilefinalise(void);

extern void alloc_initialise(void);
extern void alloc_finalise(void);
#else
#define xbinder_list2(a,b)      xglobal_cons2(SU_Other,a,b)
#define xbinder_list3(a,b,c)    xglobal_list3(SU_Other,a,b,c)

#define alloc_mark()            ((void)0)
#define alloc_unmark()          ((void)0)
#define drop_local_store()      ((void)0)
#define alloc_reinit()          ((void)0)
#define alloc_noteAEstoreuse()  ((void)0)
#define show_store_use()        ((void)0)
#define alloc_perfileinit()     ((void)0)
#define alloc_perfilefinalise() ((void)0)
#define alloc_initialise()      ((void)0)
#define alloc_finalise()        ((void)0)

#endif /* CALLABLE_COMPILER */

#endif
