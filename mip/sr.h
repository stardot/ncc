/*
 * sr.h: binder live range splitting
 * Copyright (C) Advanced Risc Machines Ltd., 1993
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _sr_h
#define _sr_h

struct SuperBinder {
  SuperBinder *cdr;
  Binder *binder;
  int32 spillcount;
};

extern SuperBinder *superbinders;

extern BindList *splitranges(BindList *local_binders, BindList *regvar_binders);

extern void splitrange_init(void);

#endif
