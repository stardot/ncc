/*
 * C compiler file ccomp/ccomp.h
 * Copyright (C) Advanced Risc Machines Ltd., 1996
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _ccomp_LOADED
#define _ccomp_LOADED 1

extern char *expr_string;
extern void clb_init(void);
extern Expr *clb_parse_expr(char *s);
#endif
