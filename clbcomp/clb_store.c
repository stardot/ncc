/*
 * clbcomp/clb_store.c: Storage allocation for the Codemist C compiler
 * Copyright (C) Codemist Ltd., 1987-1992.
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1990-1992, 1996.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifdef __STDC__
#  include <stdlib.h>
#  include <string.h>
#else
#  include "stddef.h"                                   /* for size_t */
#  include "strings.h"
extern char *calloc();
extern free();
#endif
#include "globals.h"
#include "store.h"
#include "defs.h"
#include "mcdep.h"  /* usrdbg(xxx) */
#include "errors.h"

typedef struct FreeList {
    struct FreeList *next;
    int32 rest[1];
} FreeList;

void ClearToNull(void **a, int32 n) {
  while (--n >= 0) a[n] = NULL;
}

VoidStar xglobal_cons2(StoreUse t, IPtr a, IPtr b)
{
    IPtr *p = (IPtr *) GlobAlloc(t, sizeof(IPtr[2]));
    p[0] = a; p[1] = b;
    return (VoidStar) p;
}

VoidStar xglobal_list3(StoreUse t, IPtr a, IPtr b, IPtr c)
{
    IPtr *p = (IPtr *) GlobAlloc(t, sizeof(IPtr[3]));
    p[0] = a; p[1] = b; p[2] = c;
    return (VoidStar) p;
}

VoidStar xglobal_list4(StoreUse t, IPtr a, IPtr b, IPtr c, IPtr d)
{
    IPtr *p = (IPtr *) GlobAlloc(t, sizeof(IPtr[4]));
    p[0] = a; p[1] = b; p[2] = c; p[3] = d;
    return (VoidStar) p;
}

VoidStar xglobal_list5(StoreUse t, IPtr a, IPtr b, IPtr c, IPtr d, IPtr e)
{
    IPtr *p = (IPtr *) GlobAlloc(t, sizeof(IPtr[5]));
    p[0] = a; p[1] = b; p[2] = c; p[3] = d; p[4] = e;
    return (VoidStar)p;
}

VoidStar xglobal_list6(StoreUse t, IPtr a, IPtr b, IPtr c, IPtr d, IPtr e, IPtr f)
{
    IPtr *p = (IPtr *) GlobAlloc(t, sizeof(IPtr[6]));
    p[0] = a; p[1] = b; p[2] = c; p[3] = d; p[4] = e; p[5] = f;
    return (VoidStar)p;
}

VoidStar xglobal_list7(StoreUse t, IPtr a, IPtr b, IPtr c, IPtr d, IPtr e, IPtr f, IPtr g)
{   IPtr *p = (IPtr *) GlobAlloc(t, sizeof(IPtr[7]));
    p[0] = a; p[1] = b; p[2] = c; p[3] = d; p[4] = e; p[5] = f; p[6] = g;
    return (VoidStar)p;
}

VoidStar GlobAlloc(StoreUse t, int32 n)
{   char *p = calloc(1,n);
    IGNORE(t);
    if (p == NULL) syserr("out of store");
    return p;
}

VoidStar discard(VoidStar p)
{   FreeList *q = ((FreeList *)p)->next;
    free(p);
    return (VoidStar) q;
}
/* end of clbcomp/clb_store.c */
