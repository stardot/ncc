/*
 * C++ compiler file xlex.c
 * Copyright (C) Codemist Ltd., 1988-1992
 * Copyright (C) Acorn Computers Ltd., 1988-1990
 * Copyright (C) Advanced RISC Machines Ltd., 1990-1992, 1994
 * SPDX-Licence-Identifier: Apache-2.0
 * All rights reserved.
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifdef __STDC__
#  include <stddef.h>
#  include <string.h>
#else
#  include "stddef.h"
#  include <strings.h>
#endif
#include <ctype.h>

#include "globals.h"
#include "lex.h"
#include "bind.h"
#include "pp.h"
#include "store.h"
#include "util.h"
#include "aeops.h"
#include "errors.h"
#ifdef TARGET_HAS_INLINE_ASSEMBLER
#  include "inlnasm.h"
#endif

#define _LEX_H

static void lex_getbodysym(void);
static void lex_putbodysym(void);

static Symstr *template_old_sv, *template_new_sv;
static SymInfo old_nextlex = {s_nothing};

typedef struct { Symstr *old, *new; SymInfo next; } Restorable_Names;
static List *restorable_names_list;
static void save_names(bool glob);
static void restore_names(void);
#include "lex.c"

static const SymBuf lexbuf_empty =
   { 0, { s_eof }, (SymInfo *)DUFF_ADDR, 0, /*pos*/ -1, 0, NO, /* old_put_handle */ -1 };
#define INIT_NLEXBUFS 8         /* initially max 8 member fn defs.      */
#define INIT_LEXBUFSIZE 32      /* initially max 32 tokens per def.     */

/* C++ requires function definitions within class definitions to be     */
/* lexed, but not parsed (typedefs) nor typechecked [ES, p178].         */
/* We save them as as token streams until we are ready -- the end       */
/* is observable by counting {}'s.                                      */
/* Note there is a complication about reading such a saved token stream */
/* since such a fn body may contain embedded fns within yet other       */
/* (local) class definitions.                                           */
/* Globalise everything to be buffered for now (very little to do).     */

static int lex_findsavebuf(void)
{   int i;
    for (i = 0; i<lexbuf_max; i++)
        if (lexbuf_vec[i].pos < 0)
            return i;
    {   int nmax = (lexbuf_max == 0 ? INIT_NLEXBUFS : 2*lexbuf_max);
        SymBuf *nvec = (SymBuf *)GlobAlloc(SU_Other,
                                     (int32)nmax * sizeof(SymBuf));
        memcpy(nvec, lexbuf_vec, lexbuf_max * sizeof(SymBuf));
        for (i = lexbuf_max; i < nmax; i++) nvec[i] = lexbuf_empty;
        lexbuf_max = nmax, lexbuf_vec = nvec;
        return lex_findsavebuf();       /* retry */
    }
}

static void lex_ensurebuf(SymBuf *p)
{   int nsize = (p->size == 0 ? INIT_LEXBUFSIZE : p->size*2);
    SymInfo *nbuf = (SymInfo *)GlobAlloc(SU_Other,
                                         (int32)nsize * sizeof(SymInfo));
    memcpy(nbuf, p->buf, p->size * sizeof(SymInfo));
    p->size = nsize, p->buf = nbuf;
}

static void lex_putbodysym()
{   SymBuf *p = &lexbuf_vec[nextsym_put_handle];
    int put_handle = nextsym_put_handle;
    bool is_lbrace = (curlex.sym == s_lbrace);
    /* write back to every Symbuf we're a nested defn of. */
    for (;;) {
        int k = p->pos;
        /* save the tokens in p->buf, followed by a s_eof token.*/
        if (k >= p->size) lex_ensurebuf(p);
        lex_beware_reinit();                        /* globalise curlex */
        if (debugging(DEBUG_LEX))
            cc_msg("lex_putbodysym: saving $l in [%d]\n", put_handle);
        p->buf[k++] = curlex;
        p->pos = k;
        /* skip s_lbrace between its already recorded */
        if (!is_lbrace && (p->old_put_handle >= 0))
        {   put_handle = p->old_put_handle;
            p = &lexbuf_vec[p->old_put_handle];
        }
        else
            break;
    }
}

/* lex_bodybegin() and lex_bodyend() deal with saving templates.        */
int lex_bodybegin()
{   int olde_put_handle = nextsym_put_handle;
    nextsym_put_handle = lex_findsavebuf();
    lexbuf_vec[nextsym_put_handle].old_put_handle = olde_put_handle;
    if (debugging(DEBUG_LEX))
    {   if (olde_put_handle >= 0)
            cc_msg("lex_bodybegin: new nested level [%d] %d\n", nextsym_put_handle,
                   olde_put_handle);
        else
            cc_msg("lex_bodybegin: [%d]\n", nextsym_put_handle);
    }
    lexbuf_vec[nextsym_put_handle].pos = 0;     /* inuse/ready to write */
    lex_putbodysym();           /* first token ':' or '{'.              */
    return nextsym_put_handle;
}

static void lex_dumpbuf(const SymBuf *p)
{
    if (p->pos < 0)
        cc_msg("unused lexbuf\n");
    else
    {   int i = 0;
        cc_msg("    ");
        for (;;)
        {   cc_msg("$k ", p->buf[i]);
            ++i;
            if (0 < p->count && p->count <= i)
                break;
            else if (p->buf[i].sym == s_eof)
                break;
            if (i % 8 == 0)
                cc_msg("\n    ");
        }
        cc_msg("\n");
    }
}

int lex_bodyend()
{   int h = nextsym_put_handle;
    SymBuf *p;
    p = &lexbuf_vec[h];
    if (p->pos >= p->size) lex_ensurebuf(p);
    p->buf[p->pos].sym = s_eof;         /* the token after '}' or ';'   */
    p->buf[p->pos].fl = p->pos < 0 ? p->buf[p->pos - 1].fl : curlex.fl;
    p->pos = 0;                         /* inuse/ready to read.         */
    nextsym_put_handle = p->old_put_handle;
    if (debugging(DEBUG_LEX))
    {
        cc_msg("lex_bodyend: [%d]\n", h);
        lex_dumpbuf(p);
    }
    return h;
}

/* lex_savebody() reads and saves (on the fly) a member fn body.        */
/* maybe should be called lex_saveblock */
int lex_savebody(void)
{   int h = lex_findsavebuf();
    SymBuf *p = &lexbuf_vec[h];
    int k = 0, braces = 0;
    if (debugging(DEBUG_LEX))
        cc_msg("lex_savebody: [%d]\n", h);
    p->pos = 0;                 /* inuse + ready for reader.        */
/* save all the tokens in p->buf, followed by a s_eof token.        */
    for (;;)
    {   if (k >= p->size) lex_ensurebuf(p);
        lex_beware_reinit();                    /* globalise curlex     */
        if (debugging(DEBUG_LEX))
            cc_msg("lex_savebody: saving $l in [%d]\n", h);
        p->buf[k++] = curlex;
        switch (curlex.sym)
        {   case s_lbrace: braces++;
            default:       nextsym();
                           break;
            case s_rbrace: if (--braces == 0) curlex.sym = s_eof;
                           else nextsym();
                           break;
            case s_eof:    p->count = k;
                           if (debugging(DEBUG_LEX))
                           {   cc_msg("lex_savebody: [%d] is\n", h);
                               lex_dumpbuf(p);
                           }
                           return h;
        }
    }
}

int lex_saveexpr(void)
{   int h = lex_findsavebuf();
    SymBuf *p = &lexbuf_vec[h];
    int k = 0, parens = 0;
    if (debugging(DEBUG_LEX))
        cc_msg("lex_saveexpr: [%d]\n", h);
    p->pos = 0;                 /* inuse + ready for reader.        */
/* save all the tokens of expr(UPTOCOMMA) in p->buf.                */
    for (;;)
    {   if (k >= p->size) lex_ensurebuf(p);
        lex_beware_reinit();                    /* globalise curlex     */
        if (debugging(DEBUG_LEX))
            cc_msg("lex_saveexpr: saving $l in [%d]\n", h);
        p->buf[k++] = curlex;
        switch (curlex.sym)
        {   case s_lpar:  parens++;
            default:      break;
            case s_rpar:  if (parens > 0)
                          {   --parens;
                              break;
                          }
            case s_comma: if (parens == 0)
                          {
            case s_eof:       p->buf[k-1].sym = s_comma;
                              p->count = k;
                              if (debugging(DEBUG_LEX))
                              {   cc_msg("lex_saveexpr: [%d] is\n", h);
                                  lex_dumpbuf(p);
                              }
                              return h;
                          }
                          break;
        }
        nextsym();
    }
}

static void save_names(bool glob)
{   Restorable_Names *tmp = (Restorable_Names *)
        ((glob) ? GlobAlloc(SU_Other, sizeof(Restorable_Names)) :
         SynAlloc(sizeof(Restorable_Names)));
    tmp->old = template_old_sv;
    tmp->new = template_new_sv;
    tmp->next = old_nextlex; old_nextlex.sym = s_nothing;
    restorable_names_list = (glob) ?
        global_cons2(SU_Other, restorable_names_list, (IPtr)tmp) :
        syn_cons2(restorable_names_list, (IPtr)tmp);
}

static void restore_names(void)
{   /* getbodysym() does an implicit lex_closebody(). Perhaps we should
       do a save_names() in doe. Done, but not in doe.
     */
    if (!restorable_names_list) syserr("Lost restoreable names");
    {   Restorable_Names *tmp = (Restorable_Names *)restorable_names_list->car;
        restorable_names_list = restorable_names_list->cdr;
        template_old_sv = tmp->old;
        template_new_sv = tmp->new;
        old_nextlex = tmp->next;
    }
}

void lex_openbody(int h, bool dup, bool record, Symstr *old_sv, Symstr *new_sv)
{   if (dup)
    {   int h2 = lex_findsavebuf();
        SymBuf *p = &lexbuf_vec[h], *p2 = &lexbuf_vec[h2];
        if (debugging(DEBUG_LEX))
        {
            cc_msg("lex_openbody: dup [%d] -> [%d]\n", h, h2);
            lex_dumpbuf(p);
        }
/* The memcpy of symbols is a bit wasteful when we could share the      */
/* the buffer vectors, but this would make (re-)allocation harder.      */
/* @@@ maybe not, let's do it soon.                                     */
        if (p2->size < p->size)
        {   int nsize = p->size;
            SymInfo *nbuf = (SymInfo *)GlobAlloc(SU_Other,
                                           (int32)nsize * sizeof(SymInfo));
            p2->size = nsize, p2->buf = nbuf;
        }
        memcpy(p2->buf, p->buf, p->size * sizeof(SymInfo));
        p2->pos = 0;
        p2->dup_hack = YES;
        h = h2;
    }
    lexbuf_vec[h].prev = nextsym_lookaside;
    if (record)
        lexbuf_vec[h].old_put_handle = -1;
    else
    {   lexbuf_vec[h].old_put_handle = nextsym_put_handle;
        nextsym_put_handle = -1;
    }
    lex_beware_reinit();                        /* globalise curlex     */
    lexbuf_vec[h].prevsym = curlex;
    nextsym_lookaside = &lexbuf_vec[h];
    if (debugging(DEBUG_LEX))
        cc_msg("lex_openbody: [%d]\n", h);
    save_names(NO);
    template_old_sv = old_sv, template_new_sv = new_sv;
    old_nextlex = nextlex; nextlex.sym = s_nothing;
    nextsym();                  /* currently always '{' (or ':'?)       */
}

void lex_closebody(void)
{   if (debugging(DEBUG_LEX))
        cc_msg("lex_closebody: [%d]\n", (int)(nextsym_lookaside - lexbuf_vec));
    nextsym_lookaside->pos = -1;
    nextsym_lookaside->count = 0;
    nextsym_lookaside->dup_hack = NO;
    template_new_sv = template_old_sv = NULL;
    curlex = nextsym_lookaside->prevsym;
    if (nextsym_lookaside->old_put_handle != -1)
        nextsym_put_handle = nextsym_lookaside->old_put_handle;
    nextsym_lookaside = nextsym_lookaside->prev;
    nextlex = old_nextlex; /*old_nextlex.sym = s_nothing;*/
    restore_names();
}

static void lex_getbodysym(void)
{   /* The first test is needed for d-or-e code, but breaks template    */
    /* code.  In fact the whole 're-lex' edifice is tottering.          */
    /* (Another victim of absent specification.)                        */
    /* Template end is marked with s_eof, doe with a count.             */
    /* Fix with a special hack flag.                                    */
    Symstr *changed = NULL;
    if (nextsym_lookaside->pos >= nextsym_lookaside->count &&
        !nextsym_lookaside->dup_hack)
    {   /* syserr("lex_getbodysym overflow"); */
        /* should we really do implicit 'close' here?                   */
        save_names(NO);
        lex_closebody();
    }
    else
    {   curlex = nextsym_lookaside->buf[nextsym_lookaside->pos];
        if (curlex.sym == s_identifier &&
            curlex.a1.sv == template_old_sv)
        {   AE_op pre = nextsym_lookaside->buf[nextsym_lookaside->pos-1].sym,
                 post = nextsym_lookaside->buf[nextsym_lookaside->pos+1].sym;
            if (pre != s_class && pre != s_comma &&
                pre != s_lpar && post != s_less)
            {   changed = curlex.a1.sv;
                curlex.a1.sv = template_new_sv;
            }
        }
        if (curlex.sym != s_eof) nextsym_lookaside->pos++;
    }
    if (debugging(DEBUG_TEMPLATE))
       (changed) ?
          cc_msg("lex_getbodysym replacing $r -> $l\n", changed):
          cc_msg("lex_getbodysym $l\n");
}

static bool had_nextlex;
static int save_nextsym_put_handle;

AEop lex_buffersym(void)
{   int h = buffersym_bufidx;
    SymBuf *p;
    if (h < 0)
    {   buffersym_bufidx = h = lex_findsavebuf();
        lexbuf_vec[h].pos = 0; /* claim it */
        /* stop any recording while we're peeking ahead but curlex
           and nextlex will already have been recorded */
        save_nextsym_put_handle = nextsym_put_handle;
        /* we don't use lexbuf_vec[h].old_put_handle because that's
           for preventing recording during playback, which we want */
        lexbuf_vec[h].old_put_handle = -1;
        nextsym_put_handle = -1;
        had_nextlex = nextlex.sym != s_nothing;
        if (debugging(DEBUG_LEX))
            cc_msg("lex_buffersym: open [%d]\n", h);
    }
    p = &lexbuf_vec[h];
    if (p->count >= p->size) lex_ensurebuf(p);
    if (debugging(DEBUG_LEX))
        cc_msg("lex_buffersym: saving $l in [%d]\n", h);
    p->buf[p->count++] = curlex;
    nextsym();
    return curlex.sym;
}

void lex_endbuffering(void)
{   int h = buffersym_bufidx;
    if (h < 0) return;
    lexbuf_vec[h].prev = nextsym_lookaside;
    lexbuf_vec[h].prevsym = curlex;
    nextsym_lookaside = &lexbuf_vec[h];
    if (debugging(DEBUG_LEX))
    {
        cc_msg("lex_endbuffering: begin reading from [%d]\n", h);
        lex_dumpbuf(nextsym_lookaside);
    }
    buffersym_bufidx = -1;
    nextsym();  /* was curlex when buffering started */
    if (had_nextlex)
    {   nextsym();      /* was nextlex when buffering started */
        ungetsym();
    }
    /* resume any recording */
    nextsym_put_handle = save_nextsym_put_handle;
}

Symstr *lex_replaceable_template_sym(Symstr *s)
{
    if (template_old_sv == s)
    {   if (debugging(DEBUG_TEMPLATE))
            cc_msg("lex_replaceable_sym replacing $r -> $r\n", s, template_new_sv);
        return template_new_sv;
    }
    else
        return s;
}

/* check nextsym_for_hashif uses consistent!.                           */
