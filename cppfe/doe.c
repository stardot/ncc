/*
 * C++ compiler file doe.c
 * Copyright (C) Advanced RISC Machines Ltd., 1995
 * SPDX-Licence-Identifier: Apache-2.0
 * All rights reserved.
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

/* Parser functions for the declaration sub-grammar.
 * Parsing is tuned towards resolving d-o-e ambiguity.
 * ::*, CV-qualifiers and a missing expr in [] all
 * indicative of a declaration.
 * Expressions are not parsed; they are skipped.
 * All syntax errors will be reported after the
 * ambiguity has been resolved.
 * Any ambiguity can be resolved syntactically,
 * hence there is no instate declaration, no name lookup.
 * Integer results (applicable to all):
 *      2: definitely a declaration
 *      1: not decided yet
 *      0: definitely not a declaration (by implication an expr)
 */

#define DOE_DECL        2
#define DOE_UNDECIDED   1
#define DOE_NOTDECL     0

#ifndef _SYN_H
#include "aeops.h"
#endif

#define cursym  (curlex.sym & ~s_qualified)
#define laterm_(s)      \
        (s == s_semicolon || \
         s == s_lbrace)

#define class_cv_starter_(sym)  \
        (sym == s_class || sym == s_union || \
        sym == s_struct || sym == s_enum || \
        sym == s_const || sym == s_volatile)

static int rd_cv_seq(void)
{   if (cursym == s_const || cursym == s_volatile)
        return DOE_DECL;
    return DOE_UNDECIDED;
}

static int skip_match(void)
{   int count = 1;
    AEop cur = cursym;
    while (!laterm_(cur) && cur != s_eof && count)
    {   if (cur == s_lpar ||
            cur == s_lbracket ||
            cur == s_lbrace) ++count;
        if (cur == s_rpar ||
            cur == s_rbracket ||
            cur == s_rbrace) --count;
        if (cur == s_ellipsis ||
            cur == s_const ||
            cur == s_volatile ||
            class_cv_starter_(cur))
            return DOE_DECL;
        cur = lex_buffersym();
    }
    return DOE_UNDECIDED;
}

static int rd_nested_name_spec(void)
{   /* no template id yet */
    while (cursym == s_identifier)
    {   lex_buffersym();
        if (cursym == s_coloncolon)
            lex_buffersym();
        else
            return DOE_NOTDECL;
    }
    return DOE_UNDECIDED;
}

static int rd_type_spec(void)
{
    if (istypestarter_(cursym) || cursym == s_typestartsym)
    {   lex_buffersym();
        return DOE_UNDECIDED;
    }
    if (class_cv_starter_(cursym))
        return DOE_DECL;
    if (cursym == s_coloncolon)
        lex_buffersym();
    if (cursym == s_identifier)
    {   int sofar;
        if (lex_buffersym() != s_coloncolon)
            return DOE_UNDECIDED;
        lex_buffersym();
        sofar = rd_nested_name_spec();
        if (sofar != DOE_UNDECIDED) return sofar;
        if (cursym != s_identifier)
            return DOE_NOTDECL;
        lex_buffersym();
        return DOE_UNDECIDED;
    }
    /* DOE_NOTDECL here means no type spec */
    return DOE_NOTDECL;
}

static int rd_type_spec_seq(void)
{   int sofar;
    while ((sofar = rd_type_spec()) == DOE_UNDECIDED);
    return (sofar == DOE_DECL) ? sofar : DOE_UNDECIDED;
}

static int rd_unqual_id(void)
{
    switch (cursym)
    {
    case s_bitnot:
        return (lex_buffersym() == s_identifier) ? DOE_UNDECIDED : DOE_NOTDECL;
    case s_operator:
        lex_buffersym();
        if hastypefield_(cursym)
        {   lex_buffersym();
            return DOE_UNDECIDED;
        }
        {   int sofar = rd_type_spec_seq();
            AEop sym = cursym;
            if (sofar != DOE_UNDECIDED) return sofar;
            while (sym == s_times ||
                sym == s_and ||
                sym == s_coloncolon ||
                sym == s_identifier)
            {   if (sym == s_times || sym == s_and)
                {   lex_buffersym();
                    if (sym == s_const || sym == s_volatile)
                        return DOE_DECL;
                }
                return DOE_DECL;
            }
            return DOE_UNDECIDED;
        }
    default:
        return (cursym == s_identifier) ?
            (lex_buffersym(), DOE_UNDECIDED) : DOE_NOTDECL;
    }
}

#if 0
static int rd_id_expr(void)
{   int sofar = rd_unqual_id();
    while (sofar == DOE_UNDECIDED && cursym == s_coloncolon)
    {   lex_buffersym();
        sofar = rd_unqual_id();
    }
    return sofar;
}
#endif

static int rd_abs_declarator(void);

static int rd_type_id(void)
{   AEop sym = cursym;
    if (istypestarter_(sym) || class_cv_starter_(sym) ||
        sym == s_coloncolon || sym == s_identifier)
    {   int sofar = rd_type_spec_seq();
        if (sofar != DOE_UNDECIDED) return sofar;
        sym = cursym;
        if (sym == s_times ||
            sym == s_and ||
            sym == s_coloncolon ||
            sym == s_lpar)
            return rd_abs_declarator();
        return DOE_UNDECIDED;
    }
    else if (sym == s_typestartsym)
        return DOE_UNDECIDED;
    return DOE_NOTDECL;
}

static int rd_direct_abs_decl(void)
{   AEop sym;
  while ((sym = cursym) == s_lpar || cursym == s_lbracket)
  {
    lex_buffersym();
    if (sym == s_lpar)
    {   int sofar;
        if ((sofar = skip_match()) != DOE_UNDECIDED)
            return sofar;
        if (cursym == s_const || cursym == s_volatile)
            return DOE_DECL;
        /* exception spec */
    }
    else if (sym == s_lbracket)
    {   int sofar;
        if (cursym == s_rbracket)
            return DOE_DECL;           /* definite decl */
        if ((sofar = skip_match()) != DOE_UNDECIDED)
            return sofar;
    }
  }
    return DOE_UNDECIDED;
}

static int rd_ptr_op(void)
{   int sofar = DOE_UNDECIDED;

    switch (cursym) {
    case s_and:
        return DOE_UNDECIDED;
    case s_times:
        while (cursym == s_times && sofar == DOE_UNDECIDED)
          lex_buffersym(), sofar = rd_cv_seq();
        return sofar;
    case s_coloncolon:
        lex_buffersym();
        if ((sofar = rd_nested_name_spec()) != DOE_UNDECIDED)
          return sofar;
        if (cursym != s_times) return DOE_NOTDECL;
        lex_buffersym();
        return rd_cv_seq();
    default:
        return DOE_NOTDECL;
    }
}

static int rd_doe_declarator(bool concrete)
{   int sofar = DOE_DECL;

    switch (cursym)
    {
    case s_coloncolon:
    case s_and:
    case s_times:
        sofar = rd_ptr_op();
        if (sofar != DOE_UNDECIDED) return sofar;
        break;
    case s_lpar:
        lex_buffersym();
        sofar = rd_doe_declarator(concrete);
        if (sofar != DOE_UNDECIDED) return sofar;
        if (cursym != s_rpar)
            return DOE_NOTDECL;
        lex_buffersym();
        concrete = NO;
        break;
    case s_identifier:
        if (!concrete) return DOE_NOTDECL;
        break;
    case s_typestartsym:
        return DOE_DECL;
    default:
        return DOE_NOTDECL;
    }
    /* Three possibilities: I must have read a ptr_operator,
       a (...) or an identifier.
     */
    if (concrete)
    {   if (cursym == s_lpar)
            return rd_doe_declarator(concrete);
        if ((sofar = rd_unqual_id()) != DOE_UNDECIDED)
            return sofar;
        if (cursym == s_coloncolon)
        { /* seen a "id::" */
          while (cursym == s_coloncolon || cursym == s_identifier)
            lex_buffersym();
          if (cursym != s_times) return DOE_NOTDECL;
          lex_buffersym();
          if ((sofar = rd_unqual_id()) != DOE_UNDECIDED)
            return sofar;
        }
    }
    if (cursym == s_lbracket || cursym == s_lpar)
        return rd_direct_abs_decl();
    return DOE_UNDECIDED;
}

static int rd_abs_declarator(void)
{   return rd_doe_declarator(NO);
}

static int rd_init_declarator(void)
{   int sofar = rd_doe_declarator(YES);
    if (sofar != DOE_UNDECIDED)
        return sofar;
    if (cursym == s_assign)
    {   if (lex_buffersym() == s_lbrace)
            return DOE_DECL;
        while (!laterm_(cursym) && cursym != s_comma)
            lex_buffersym();
        return DOE_UNDECIDED;
    }
    if (cursym == s_lpar)
    {   lex_buffersym();
        return skip_match();
    }
    return sofar;
}

static int rd_init_declarator_seq(void)
{   int sofar = DOE_UNDECIDED;
    while (sofar == DOE_UNDECIDED && !laterm_(cursym))
    {
        sofar = rd_init_declarator();
        if (cursym == s_comma) lex_buffersym();
    }
    return sofar;
}

/* this must be preceded by a call to rd_type_name() */
static int is_declaration(int n)
{   int sofar = DOE_UNDECIDED;
    lex_buffersym();
    if (cursym == s_lpar)
        sofar = (n == PASTCOMMA) ? rd_init_declarator_seq() : rd_init_declarator();
    lex_endbuffering();
    return sofar;
}

static int is_type_id(void)
{   int sofar = rd_type_id();
    lex_endbuffering();
    return sofar;
}
