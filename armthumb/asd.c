/*
 * C compiler file arm/asd.c,  version 6c
 * Copyright (C) Codemist Ltd, 1988
 * Copyright (C) Acorn Computers Ltd., 1988
 * Copyright:   (C) 1991, 1994, Advanced RISC Machines Limited. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

/* This file contains routines to buffer infomation which is required for */
/* Acorn's source-level debugger.                                         */
/* NOTA BENE: at several points this code refers to inadequacies of       */
/* the TopExpress draft specification.  See text file for further details */
/* of these criticisms.                                                   */

/* Memo: 1. redo s_typedefname          DONE?
         2. check spurious tests of DBG_XXX (done).
         4. The filedate field is left as 0 as TopExpress do not use it
            and no format is specified.
         5. Since we only save the file position of the start of each command,
            the end of a command is identified with the start of the
            next one.  Find the word 'sentinel' to check.
         6. Shame that the sectionflags field (see DEB_SECTION) cannot
            distinguish toplevel and local var table entries.
         7. Shame that the 16+16 coding of DEB_FILEINFO + length
            overflows when compiling the compiler.  8+24 better?
            Yes, but TopExpress hack by requiring setting length to 0
            if too big to fit.
*/

#ifdef __STDC__
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <stddef.h>

#include "globals.h"

#ifdef TARGET_HAS_MULTIPLE_DEBUG_FORMATS

#define dbg_tableindex asd_tableindex
#define dbg_notefileline asd_notefileline
#define dbg_addcodep asd_addcodep
#define dbg_scope asd_scope
#define dbg_topvar asd_topvar
#define dbg_type asd_type
#define dbg_proc asd_proc
#define dbg_locvar asd_locvar
#define dbg_locvar1 asd_locvar1
#define dbg_commblock asd_commblock
#define dbg_enterproc asd_enterproc
#define dbg_bodyproc asd_bodyproc
#define dbg_return asd_return
#define dbg_xendproc asd_xendproc
#define dbg_define asd_define
#define dbg_undef asd_undef
#define dbg_include asd_include
#define dbg_notepath asd_notepath
#define dbg_init asd_init
#define dbg_finalise asd_finalise
#define dbg_final_src_codeaddr asd_final_src_codeaddr
#define dbg_setformat asd_setformat
#define dbg_needsframepointer asd_needsframepointer

#define obj_notefpdesc asd_notefpdesc
#define dbg_debugareaexists asd_debugareaexists
#define dbg_writedebug asd_writedebug

#endif

#include "mcdep.h"
#include "mcdpriv.h"
#include "aeops.h"
#include "aetree.h"      /* evaluate */
#include "errors.h"
#include "xrefs.h"
#include "store.h"
#include "codebuf.h"
#include "regalloc.h"
#include "util.h"
#include "sem.h"       /* alignoftype, sizeoftype, structfield */
#include "builtin.h"   /* te_xxx, xxxsegment */
#include "bind.h"
#include "simplify.h"  /* mcrep */

#include "asdfmt.h"

#ifdef TARGET_HAS_DEBUGGER

#ifdef TARGET_HAS_ASD

static bool oldformat;

#define OldAsdTables oldformat

#ifndef TARGET_HAS_MULTIPLE_DEBUG_FORMATS
char dbg_name[] = "ASD";
int usrdbgmask;
#endif

#define DEBUGAREANAME "C$$debug"
#define FPMAPAREANAME "C$$fpmap"

static void dbg_sub_init(void);
static bool dbg_init_done, dbg_sub_init_done;

#define DbgAlloc(n) GlobAlloc(SU_Dbg, n)

/* See also magic numbers coded as required in dbg_typerep() */

typedef struct Dbg_Structelt Dbg_Structelt;
typedef struct Dbg_Enumelt Dbg_Enumelt;
typedef struct Dbg_Return Dbg_Return;

struct Dbg_Structelt {
    Dbg_Structelt *cdr;    /* must be first for dreverse */
    int32 offset;
    int32 type;
    char *name;
};

struct Dbg_Enumelt {
    Dbg_Enumelt *cdr;      /* must be first for dreverse */
    int32 val;
    char *name;
};

struct Dbg_Return {
    Dbg_Return *cdr;
    int32 car;
};

static Dbg_Return *dbg_returnlist;

/* The next macro seems to indicate a lack to do this portably in ANSI-C */
/* Are discriminated unions second class objects?                        */
#define DbgListAlloc(variant) \
    ((DbgList *)DbgAlloc((size_t)(sizeof(dbglist->car.variant)+offsetof(DbgList,car))))

/* The following is the *internal* data structure in which debug info. */
/* is buffered.  It differs from AOF format in keeping pointers to     */
/* strings instead of bcpl-style strings themselves to save space.     */
typedef struct DbgList DbgList;
struct DbgList {
    DbgList *cdr;
    int32 debsort;
    union
    {  struct { char const *name;
                Symstr *codeseg;
                int32 codesize;
              } DEB_SECTION;
       struct { int32 type;
                int32 args;
                int32 sourcepos;
                int32 entryaddr, bodyaddr;   /* see dbg_bodyproc */
                int32 endproc;
/* a (Deb_filecoord *) next line may subsume sourcepos too */
                char const *fileentry;
                char const *name;
                Symstr *codeseg;
              } DEB_PROC;
       struct { int32 sourcepos;
                int32 endaddr;
/* a (Deb_filecoord *) next line may subsume sourcepos too */
                char const *fileentry;
                Dbg_Return *retaddrs;
                Symstr *codeseg;
              } DEB_ENDPROC;
       struct { int32 type;
                int32 sourcepos;
                unsigned32 stgclass;
                int32 location;
                Symstr *sym;
                Symstr *base;
              } DEB_VAR;
       struct { int32 type;
                TypeExpr *typex;
                int32 loc;
                char const *name;
              } DEB_TYPE;
       struct { int32 size;
                int32 arrayflags;
                int32 basetype;
                int32 lowerbound;
                int32 upperbound;
              } DEB_ARRAY;
       struct { int32 typeword;
                int32 space;
                int32 container;
                Dbg_Enumelt *elts;
              } DEB_ENUM;
       struct { int32 typeword;
                int32 space;
                int32 size;
                Dbg_Structelt *elts;
              } DEB_STRUCT;
       struct { DbgList *nextstruct;
                int32 *typeref;
                int32 ptrcnt;
                SET_BITMAP sort;
              } DEB_FREF;
       struct { union { DbgList *next;
                        int32 codeaddr;
                      } s;
                Symstr *codeseg;
              } DEB_SCOPE;
       struct { int32 type;   /* of the extracted value */
                int32 container;
                unsigned8 size, offset, pad1, pad2;
              } DEB_BF;
       struct { int32 space;
                char const *name;
                int32 argcount;
                char const *body;
                dbg_ArgList const *arglist;
                FileLine fl;
              } DEB_DEFINE;
       struct { char const *name;
                FileLine fl;
              } DEB_UNDEF;
    } car;
};

static int32 dbgloc;
static DbgList *dbglist, *dbglistproc;

typedef struct DbgList Dbg_TypeFRef;

#define fr_nextstruct_(p) ((p)->car.DEB_FREF.nextstruct)
#define fr_typeref_(p) ((p)->car.DEB_FREF.typeref)
#define fr_ptrcnt_(p) ((p)->car.DEB_FREF.ptrcnt)
#define fr_sort_(p) ((p)->car.DEB_FREF.sort)

static Dbg_TypeFRef *dbg_structRefs;

typedef struct Dbg_LocList Dbg_LocList;
struct Dbg_LocList
{   Dbg_LocList *cdr;
    Binder *name;
    int32 pos;
    int32 typeref;
    int32 size;
    Dbg_TypeFRef *typefrefs;
};

static Dbg_LocList *dbg_loclist;

static struct {
    Symstr *sym;
    int32 len;
} baseseg;

static DataXref *dbg_relocate(DataXref *xrefs, int32 where, Symstr *symbol)
{   return (DataXref*)global_list3(SU_Xref, xrefs, where, symbol);
}

/* For some strange reason, AOF debugger info has bcpl-format strings */
/* in spite of the fact that the rest of AOF uses C format ones.      */
static size_t bcpl_string(char *b, char const *c)
{   size_t n = strlen(c);
    if (n > 255) n = 255;
    b[0] = n;
    memcpy(b+1, c, n);
    memclr(b+n+1, 3-(n&3));             /* for neatness */
    return (size_t)padstrlen(n);
}

static int32 dbg_check(int32 offset)
{   if (offset > 0x800000L)
        cc_fatalerr(armdbg_fatalerr_toobig);
    return -offset;
}

/* First structs and code for buffering file/line co-ordinates.         */
/* BEWARE: believing the debugging is usually done on small programs,   */
/* and not in production runs, the following data structures are        */
/* naive rather than efficient and often require quadratic algorithms.  */
/* If it worries you, then the solution is obvious, but given the       */
/* complexity of the debugger tables question the need first!           */

/* We have one of these for every file we see.  BUT because a file may  */
/* validly be included more than once in a C program, we had better do  */
/* pointer, not string, equality on names, and rely on pp.c behaviour.  */

typedef struct Deb_filecoord Deb_filecoord;
typedef struct Deb_filelist Deb_filelist;

struct Deb_filelist
{   Deb_filelist *nextfile;
    char const *filename;
    int32 filelistoffset;  /* filename's ->fileentry offset for DEB_PROC */
    unsigned lastline;
    Deb_filecoord *linelist;    /* reverse sorted via nextinfile */
};

/* For putting out TopExpress-style line number info here we have to do */
/* a fair amount of work.  'nextinfile' is the next command start in a  */
/* file and 'nextincode' the chain in order of code addresses.          */
/* 'nextincode' is in ascending order, and 'nextinfile' in descending   */
/* but the latter is 'dreversed' before use.                            */
/* When they coincide, we can group fragments (see TopExpress doc).     */
struct Deb_filecoord
{   Deb_filecoord *nextinfile, *nextincode;   /* @@@ dreverse pun */
    Deb_filelist *file;
    unsigned16 line, col;
    int32 codeaddr;
    Symstr *codeseg;
};

static Deb_filelist *dbg_filelist;
/* The next two vars are (code order) list (->nextincode) & tail pointer */
static Deb_filecoord *dbg_coord_p, *dbg_coord_q;
static Deb_filecoord dbg_coord_sentinel =
  {   0, 0,                 /* nextinfile, nextincode                   */
      0,                    /* file                                     */
      65535, 0, 0x00ffffff /* line, col, codeaddr(set by dbg_fileinfo).*/
  };

static int32 asd_tablesize(void)
{
  return dbgloc;
}

int32 dbg_tableindex(int32 dt_number)
{
  IGNORE(dt_number);
  return 0;
}

VoidStar dbg_notefileline(FileLine fl)
{   Deb_filelist *x;
    if (!dbg_sub_init_done) dbg_sub_init();
    x = dbg_filelist;
    while (x != NULL && x->filename != fl.f) x = x->nextfile;
    if (x == NULL)
    {   x = (Deb_filelist *) DbgAlloc(sizeof(Deb_filelist));
        x->nextfile = dbg_filelist, x->filename = fl.f,
        x->filelistoffset = 0, x->linelist = 0;
        dbg_filelist = x;
    }
    if (usrdbg(DBG_LINE))
    {   Deb_filecoord *l = x->linelist;
        /* There used to be a syserr here if (l != NULL && l->line > fl.l),
           but it can be triggered by #line (though not #line n file, which
           won't give equality of filename): the CVS has an example. Also, it
           fails if functions are taken out of file order, as in the
           out-of-line expansion of inline functions.
         */
        l = (Deb_filecoord *) DbgAlloc(sizeof(Deb_filecoord));
          l->nextinfile = x->linelist, l->nextincode = 0,
          l->file = x, l->line = fl.l, l->col = fl.column,
          l->codeaddr = -1;
          l->codeseg = dbg_init_done ? bindsym_(codesegment) : 0;
        x->linelist = l;
        x->lastline = fl.l;
        return (VoidStar) l;
    }
    return DUFF_ADDR;
}

static DbgList *dbglistscope;

/* The 'dbgaddr' arg has type 'void *' to keep the debugger types local to */
/* this file.  This does not make it any less of a (ANSI approved) hack.   */
void dbg_addcodep(VoidStar dbgaddr, int32 codeaddr)
{
    if (dbgaddr == NULL) { /* J_INFOSCOPE */
        /* c.flowgraf outputs a J_INFOSCOPE immediately after calling
         * dbg_scope, to mark the relevant code address.
         */
        if (debugging(DEBUG_Q)) cc_msg("-- scope at 0x%lx\n", codeaddr);
        {   DbgList *p = dbglistscope;
            while (p != NULL) {
                 DbgList *next = p->car.DEB_SCOPE.s.next;
                 p->car.DEB_SCOPE.s.codeaddr = codeaddr;
                 p = next;
            }
            dbglistscope = NULL;
        }
    } else if (usrdbg(DBG_LINE)) {
        Deb_filecoord *p = (Deb_filecoord *)dbgaddr;
        if (debugging(DEBUG_Q))
            cc_msg("%p ('%s' line %u/%u) @ %.6lx\n", (VoidStar )p,
                    p->file->filename, p->line, p->col, (long)codeaddr);
        /* The following test avoids setting nextincode/codeaddr twice */
        /* This is currently needed in case FileLine's are duplicated. */
        if (p->codeaddr == -1)
        {   p->codeaddr = codeaddr;
            if (dbg_coord_p) dbg_coord_q = dbg_coord_q->nextincode = p;
            else dbg_coord_p = dbg_coord_q = p;
        }
#ifdef never
        else
            syserr(syserr_addcodep);
#endif
    }
}

static int32 dbg_filetooffset(char const *filename)
{   Deb_filelist *x;
    for (x = dbg_filelist; x != NULL; x = x->nextfile)
        if (x->filename == filename) return x->filelistoffset;
    /* The next line should not happen if dbg_notefileline is working. */
    syserr(syserr_filetooffset, filename);
    return 0;
}

static void dbg_hdr(int itemsort, size_t length) {
    int32 w = ((int32)length << 16) | itemsort;
    obj_writedebug(&w, 1+DBG_INTFLAG);
}

typedef struct {
  int32 codebytes;
  int infosize;
  unsigned lines;
  unsigned col;
} Dbg_LineInfoItem;

static bool dbg_GetLineInfoDescription(bool first,
                                       Deb_filecoord *l,
                                       Dbg_LineInfoItem *item)
{
    Deb_filecoord *nc = l->nextincode;
    Deb_filecoord *nf = l->nextinfile;
    if (nf!=NULL && nc!=NULL && nf->file==nc->file) {
        Deb_filecoord *nnf = nf;
        while (nnf!=NULL) {
            if (nnf==nc) { nf=nc; break; }
            if (nnf->codeaddr!=-1 &&
                nnf->codeaddr!=l->codeaddr) break;
            nnf = nnf->nextinfile;
        }
    }
    item->codebytes = nc->codeseg == l->codeseg ? nc->codeaddr - l->codeaddr : 0;
    if (nf == NULL)
        item->lines = l->file->lastline - l->line, item->col = 0;
    else if (nf != nc ? nf->codeaddr != -1 : nc->line < l->line)
        item->lines = 1, item->col = 0;
    else
        item->lines = nf->line - l->line,
        item->col = (item->lines == 0) ? (first ? nf->col - 1 : nf->col - l->col) :
                    (nf->col == 1) ? 0 : nf->col;

    if (item->codebytes==0 && item->lines == 0 && item->col == 0)
        item->infosize = 0;
    else if (item->codebytes<256 &&
              (OldAsdTables ?
                item->lines < 256 :
                 ( (item->lines == 0 && item->col < (255-Asd_LineInfo_Short_MaxLine)) ||
                   (item->col == 0 && item->lines <= Asd_LineInfo_Short_MaxLine))))
        item->infosize = 2;
    else if (OldAsdTables)
        item->infosize = 6;
    else if (item->lines == 0) {
        item->col += l->col;
        if (item->col <= 1)
            item->col = 0, item->infosize = 6;
        else
            item->infosize = 8;
    } else if (item->col == 0)
        item->infosize = 6;
    else
        item->infosize = 8;

    return nf==NULL || nf != nc || nc->line < l->line || nc->codeseg != l->codeseg;
}

static int32 dbg_padtoword(int32 loc) {
    if ((loc & 2) == 0)
        return loc;
    else {
        int w = 0;
        obj_writedebug(&w, 2);
        return loc + 2;
    }
}

/* dbg_fileinfo() writes out the DEB_FILEINFO table.  However, due     */
/* to the poxy requirement to write the length first we have to do two */
/* passes (and ditto for fragments within).                            */
static int32 dbg_fileinfo(bool writing, int32 predictedbytes, int32 loc, DataXref **xrefp)
{   int32 totalbytes = 4;   /* loc and totalbytes can probably merge */
    Deb_filelist *x;
    DataXref *xrefs = *xrefp;
    if (!usrdbg(DBG_ANY)) return 0;   /* nominally totalbytes */
    if (writing)
    {   dbg_hdr(ITEMFILEINFO, predictedbytes < 0x10000 ? (unsigned)predictedbytes : 0);
        loc += 4;
    }
    for (x = dbg_filelist; x != NULL; x = x->nextfile)
    {   Deb_filecoord *l;
        Dbg_LineInfoItem item;
        int32 nfrags = 0, nfragbytes = 0;
        bool discontinuity = YES;
        if (!writing)     /* first pass - do a dreverse (on ->nextinfile). */
            (x->linelist = (Deb_filecoord *)dreverse((List *)x->linelist),
             x->filelistoffset = totalbytes); /* save for DEB_PROC/ENDPROC */
        /* Note that ->nextincode has a sentinel to flush last fileline.   */
        for (l = x->linelist; l != NULL; l = l->nextinfile)
        {   if (l->codeaddr != -1)
            {   /* we generated code for this construct */
                bool nextdiscontinuity = dbg_GetLineInfoDescription(discontinuity, l, &item);
                if (item.infosize!=0) {
                  if (discontinuity) {
                    nfrags++;
                    nfragbytes = (nfragbytes+2)&(-4);  /* round up to word boundary */
                    nfragbytes += (int32)item.infosize+20;
                    discontinuity = NO;
                  } else {
                    nfragbytes += item.infosize;
                  }
                }
                discontinuity = discontinuity | nextdiscontinuity;
            }
        }
        {   char c[256];
            size_t n = bcpl_string(c, x->filename);
            int32 size = (int32)n + 12 + ((nfragbytes+2)&(-4));
            if (writing)
            {   bool discontinuity = YES;
                int32 w[5];
                w[0] = size;
                w[1] = 0;        /* @@@ file date one day */
                obj_writedebug(w, 2+DBG_INTFLAG);
                obj_writedebug(c, n);
                obj_writedebug(&nfrags, 1+DBG_INTFLAG);
                loc += (int32)n + 12;
                for (l = x->linelist; l != NULL; l = l->nextinfile)
                    if (l->codeaddr != -1) {
                      bool nextdiscontinuity = dbg_GetLineInfoDescription(discontinuity, l, &item);
                      if (item.infosize!=0) {
                        if (discontinuity) {
                          int32 codebytes = 0;
                          int32 lines = 0;
                          int32 infosize = 0;
                          loc = dbg_padtoword(loc);
                          { Deb_filecoord *nl = l;
                            bool ended = YES;
                            Dbg_LineInfoItem li;
                            do {
                              ended = dbg_GetLineInfoDescription(ended, nl, &li);
                              codebytes += li.codebytes;
                              lines += li.lines;
                              infosize += li.infosize;
                              nl = nl->nextincode;
                            } while (!ended);
                          }
                          w[0] = 20+infosize;
                          w[1] = l->line;
                          w[2] = l->line+lines;
                          w[3] = l->codeaddr;
                          xrefs = dbg_relocate(xrefs, loc+12, l->codeseg);
                          w[4] = codebytes;
                          obj_writedebug(w, 5+DBG_INTFLAG);
                          loc += 20;
                          discontinuity = NO;
                        }
                        if (item.infosize==2) {
                          c[0] = (char)item.codebytes;
                          c[1] = (item.lines != 0 | OldAsdTables) ?
                                   (char)item.lines :
                                   (char)(item.col+1+Asd_LineInfo_Short_MaxLine);
                          obj_writedebug(c, 2);
                          loc += 2;
                        } else {
                          /* long format (now in two flavours) */
                          unsigned16 s[3];
                          c[0] = c[1] = 0;
                          s[0] = (unsigned16)item.lines;
                          s[1] = (unsigned16)item.codebytes;
                          if (item.infosize == 8) {
                              s[2] = (short)item.col;
                              c[1] = Asd_LineInfo_Short_MaxLine+1;
                          }
                          obj_writedebug(c, 2);
                          obj_writedebug(s, ((int32)item.infosize-2)/2 + DBG_SHORTFLAG);
                          loc += item.infosize;
                        }
                      }
                      discontinuity = discontinuity | nextdiscontinuity;
                    }

                loc = dbg_padtoword(loc);
            }
            totalbytes += size;
        }
    }
    if (writing)
    {   int32 v[1];
        v[0] = 0;
        obj_writedebug(v, 4);
        loc += 4;
    }
    *xrefp = xrefs;
    return totalbytes+4;
}

/* End of file/line co-ordinate code */

static void dbg_typerep(TypeExpr *, int32 *typep);

static int32 dbg_arrayrep(TypeExpr *t, Expr *e)
{   /* e is the array size. Since C arrays start at 0, the upper bound is */
    /* one less                                                           */
    DbgList *p = DbgListAlloc(DEB_ARRAY);
    p->debsort = ITEMARRAY;
    p->car.DEB_ARRAY.arrayflags = (e ? 10:6);
    dbg_typerep(t, &p->car.DEB_ARRAY.basetype);
    p->car.DEB_ARRAY.lowerbound = 0;
    p->car.DEB_ARRAY.upperbound = e ? evaluate(e)-1:0;
    p->car.DEB_ARRAY.size = sizeoftype(t);
    p->cdr = dbglist;              /* do this last (typerep above) */
    dbglist = p;
    dbgloc += 24;
    return dbg_check(dbgloc-24);
}

#define dbg_mk_typerep(code,ptrc) TYPE_TYPEWORD((int32)(code), ptrc)

static int32 dbg_structentry(
  SET_BITMAP sort, ClassMember *members, int32 size, Dbg_TypeFRef *refs,
  TagBinder *b)
{   ClassMember *l;
    DbgList *resultstruct;
    Dbg_Structelt *elts = 0;
    int32 space = 12;
    int32 entry;

    for (l = members; l != 0; l = memcdr_(l))
    {   if (memsv_(l) && is_datamember_(l))
        {   /* note that memsv is 0 for padding bit fields */
            space += 8 + padstrlen(strlen(symname_(memsv_(l))));
        }
    }
    /* The next line notes the struct definition in the debug table, */
    /* thereby avoiding problems with circular types.                */
    dbgloc += space;
    entry = dbg_check(dbgloc-space);
    {   DbgList *p = DbgListAlloc(DEB_STRUCT);
        p->debsort = sort == bitoftype_(s_struct) ? ITEMSTRUCT :
                      sort == bitoftype_(s_union) ? ITEMUNION :
                                                    ITEMCLASS;
        p->car.DEB_STRUCT.typeword = entry;
        p->car.DEB_STRUCT.space = space;
        p->car.DEB_STRUCT.size = 0;    /* filled in later */
        p->car.DEB_STRUCT.elts = 0;    /* filled in later */
        p->cdr = dbglist;
        resultstruct = dbglist = p;
        if (b != NULL) b->tagbinddbg = (IPtr)p;
    }
    {   StructPos sp;
        structpos_init(&sp, b);
        for (l = members; l != 0; l = memcdr_(l)) {
            if (!structfield(l, sort, &sp)) continue;
            if (memsv_(l))
            {   /* note that memsv is 0 for padding bit fields */
                Dbg_Structelt *el =
                  (Dbg_Structelt *) DbgAlloc(sizeof(Dbg_Structelt));
                cdr_(el) = elts;
                el->offset = sp.woffset;
                if (sp.bsize == 0) {
                    dbg_typerep(memtype_(l), &el->type);
                } else {
                    DbgList *p = DbgListAlloc(DEB_BF);
                    TypeExpr te; te = *memtype_(l);
                    typespecmap_(&te) &= ~BITFIELD;
                    dbgloc += 16;
                    el->type = dbg_mk_typerep(dbg_check(dbgloc-16), 0);
                    p->debsort = ITEMBITFIELD;
                    p->car.DEB_BF.offset = (unsigned8)(target_lsbitfirst ?
                                                      memboff_(l) :
                                                      MAXBITSIZE-membits_(l)-memboff_(l));
                    p->car.DEB_BF.size = (unsigned8)membits_(l);
                    p->car.DEB_BF.pad1 = p->car.DEB_BF.pad2 = 0;
                    p->cdr = dbglist;
                    dbglist = p;
                    dbg_typerep(&te, &p->car.DEB_BF.type);
                    dbg_typerep(te_int, &p->car.DEB_BF.container);
                }
                el->name = symname_(memsv_(l));
                elts = el;
            }
        }
    }
    resultstruct->car.DEB_STRUCT.size = size;
    resultstruct->car.DEB_STRUCT.elts = (Dbg_Structelt*)dreverse((List*)elts);
    /* if the structure was forward-referenced, fill in the references */
    for (; refs != 0; refs = cdr_(refs)) {
       *(fr_typeref_(refs)) = dbg_mk_typerep(entry, fr_ptrcnt_(refs));
       fr_typeref_(refs) = 0;    /* mark it as handled */
    }
    return entry;
}

static int32 dbg_enumentry(BindList *members, Dbg_TypeFRef *refs, TagBinder *b)
{   Dbg_Enumelt *elts = 0;
    int32 space = 12;
    int32 entry;
    int32 count = 0;
    bool contiguous = YES;
    int32 last = 0;

    for (; members != 0; members = members->bindlistcdr, count++) {
        Dbg_Enumelt *p = (Dbg_Enumelt *)DbgAlloc(sizeof(Dbg_Enumelt));
        Binder *elt = members->bindlistcar;
        cdr_(p) = elts;
        p->name = symname_(bindsym_(elt));
        p->val = bindenumval_(elt);
        if (contiguous) {
            if (count != 0 && p->val != last+1)
                contiguous = NO;
            last = p->val;
        }
        space += padstrlen(strlen(symname_(bindsym_(elt))));
        elts = p;
    }
    elts = (Dbg_Enumelt *)dreverse((List *)elts);
    if (contiguous)
        space += 4;
    else
        space += count*4;
    dbgloc += space;
    entry = dbg_check(dbgloc-space);
    {   DbgList *p = DbgListAlloc(DEB_ENUM);
        static int32 const c[] =
        {   TYPE_TYPEWORD(TYPESBYTE, 0),
            TYPE_TYPEWORD(TYPESHALF, 0),
            TYPE_TYPEWORD(TYPESWORD, 0),
            TYPE_TYPEWORD(TYPESWORD, 0),
            TYPE_TYPEWORD(TYPEUBYTE, 0),
            TYPE_TYPEWORD(TYPEUHALF, 0),
            TYPE_TYPEWORD(TYPEUWORD, 0),
            TYPE_TYPEWORD(TYPEUWORD, 0)
        };
        p->debsort = contiguous ? ITEMENUMC : ITEMENUMD;
        p->car.DEB_ENUM.typeword = entry;
        p->car.DEB_ENUM.space = space;
        p->car.DEB_ENUM.container =
                    c[(tagbindbits_(b) & TB_CONTAINER) >> TB_CONTAINER_SHIFT];
        p->car.DEB_ENUM.elts = elts;
        p->cdr = dbglist;
        dbglist = p;
        if (b != NULL) b->tagbinddbg = (IPtr)p;
    }
    /* if the structure was forward-referenced, fill in the references */
    for (; refs != 0; refs = cdr_(refs)) {
        *(fr_typeref_(refs)) = dbg_mk_typerep(entry, fr_ptrcnt_(refs));
        fr_typeref_(refs) = 0;    /* mark it as handled */
    }
    return entry;
}

static int32 dbg_structrep(TagBinder *b, int32 size)
{ /*
   * Note that the TopExpress definition cannot cope with undefined
   * structs as required by C.
   */
    if (!(tagbindbits_(b) & TB_DEFD))
        return 0;                      /* presumably a forward reference */
    /* (b->tagbinddbg is a pointer to the structure's forward reference chain
       while it's not defined, and a pointer to its DbgList entry when it is
     */
    {   Dbg_TypeFRef *frefs = (Dbg_TypeFRef *)b->tagbinddbg;
        if (frefs == NULL || frefs->debsort == 0) {
            /* not yet defined */
            if (tagbindbits_(b) & bitoftype_(s_enum))
                dbg_enumentry(tagbindenums_(b), frefs, b);
            else
                dbg_structentry(tagbindbits_(b) & CLASSBITS, tagbindmems_(b),
                                size, frefs, b);
        }
    }
    return ((DbgList *)b->tagbinddbg)->car.DEB_STRUCT.typeword;
}

static Dbg_TypeFRef *addtypefref(Dbg_TypeFRef *next, int32 *typep, int32 ptrcnt) {
    Dbg_TypeFRef *fr = DbgListAlloc(DEB_FREF);
    cdr_(fr) = next;
    fr->debsort = 0;
    fr_typeref_(fr) = typep;
    fr_ptrcnt_(fr) = ptrcnt;
    *typep = 0;
    return fr;
}

static void struct_typerep(TypeExpr *x, int32 *typep, int32 ptrcnt)
{   TagBinder *b = typespectagbind_(x);
    int32 size = tagbindbits_(b) & TB_DEFD ? sizeoftype(x) : 0;
    int32 structrep = dbg_structrep(b, size);
    /* if there's a real definition of the structure, structrep will ensure
       that an entry for it has been generated (and returns the negated offset
       of the entry within the debug data) */
    if (typep != NULL) {
        if (structrep != 0) {
            *typep = dbg_mk_typerep(structrep, ptrcnt);
            return;
        } else {
        /* no real definition of the structure yet.  Add an entry to its forward
           references. */
            Dbg_TypeFRef *p = addtypefref((Dbg_TypeFRef*)b->tagbinddbg, typep, ptrcnt);
            fr_nextstruct_(p) = dbg_structRefs;
            fr_sort_(p) = tagbindbits_(b) & ENUMORCLASSBITS;
            b->tagbinddbg = (IPtr)p;
            dbg_structRefs = p;
            return;
        }
    }
}

static int typename_match(char const *mangled, char const *generic)
{    int l = strlen(generic);
     if (mangled == generic) return 1;
     if (StrnEq(mangled, generic, l) &&
         StrnEq(mangled+l, "__", 2)) return 1;
    return 0;
}

static void dbg_typerep(TypeExpr *x, int32 *typep)
{   /* note that we do NOT call prunetype() here so we still see typedefs */
    /* BEWARE: this routine uses TopExpress magic numbers as type codes.  */
    int32 ptrcnt = 0;
    int32 restype = 0;
    for (;;) {
        switch (h0_(x)) {
        case t_content:
        case t_ref:         /* @@@ OK? */
            ptrcnt++;
            x = typearg_(x);
            /* TopExpress spec cannot express const/volatile pointers */
            continue;
        case t_subscript:
            restype = dbg_mk_typerep(dbg_arrayrep(typearg_(x), typesubsize_(x)),
                                     ptrcnt);
            break;
        case t_fnap:
            restype = dbg_mk_typerep(TYPEFUNCTION, ptrcnt);  /* TopExpress cannot say more */
            break;
        case s_typespec:
          { SET_BITMAP m = typespecmap_(x);
            switch (m & -m) {   /* LSB - unsigned32/long etc. are higher */
            case bitoftype_(s_enum):
                struct_typerep(x, typep, ptrcnt);
                return;
            case bitoftype_(s_struct):
            case bitoftype_(s_class):
            case bitoftype_(s_union):
                struct_typerep(x, typep, ptrcnt);
                return;
            case bitoftype_(s_typedefname):
              { Binder *b = typespecbind_(x);
                /* is there already a table entry for it ? */
                {   DbgList *p;
                    for ( p = dbglist ; p!=NULL ; p = p->cdr )
                        if ( p->debsort==ITEMTYPE &&
                             p->car.DEB_TYPE.typex==bindtype_(b) &&
                             typename_match(p->car.DEB_TYPE.name, symname_(bindsym_(b)))) {
                            restype = dbg_mk_typerep(-(p->car.DEB_TYPE.loc), ptrcnt);
                            break;
                        }
                    /* if not, is there a pending one? */
                    if (p == NULL && typep != NULL) {
                        Dbg_LocList *l;
                        for (l = dbg_loclist; l != NULL; l = cdr_(l))
                            if (l->name == b) {
                                l->typefrefs = addtypefref(l->typefrefs, typep, ptrcnt);
                                return;
                            }
                        syserr("typedef");
                    }
                }
                break;
              }
            case bitoftype_(s_char):
              { int32 mcr = mcrepoftype(x);
                restype = dbg_mk_typerep((mcr & MCR_SORT_MASK) == MCR_SORT_SIGNED ?
                                                            TYPESBYTE : TYPEUBYTE,
                                         ptrcnt);
                break;
              }
            case bitoftype_(s_int):
                if (m & BITFIELD) syserr(syserr_dbg_bitfield);
                {   int32 mcr = mcrepoftype(x);
                    int32 tc;
                    switch (mcr & MCR_SIZE_MASK) {
                    case 2:  tc = TYPEUHALF; break;
                    case 8:  tc = TYPEUDWORD; break;
                    default: tc = TYPEUWORD; break;
                    }
                    if ((mcr & MCR_SORT_MASK) == MCR_SORT_SIGNED)
                        tc += (TYPESBYTE - TYPEUBYTE);
                    restype = dbg_mk_typerep(tc,ptrcnt);
                    break;
                }
            case bitoftype_(s_double):
                restype = dbg_mk_typerep((m & bitoftype_(s_short)) ? TYPEFLOAT : TYPEDOUBLE,
                                         ptrcnt);
                break;
            case bitoftype_(s_void):
                restype = dbg_mk_typerep(TYPEVOID, ptrcnt);
                break;
            default:
                syserr(syserr_dbg_typerep, (VoidStar)x, (long)typespecmap_(x));
                break;
            }
            break;
          }
        default:
            syserr(syserr_dbg_typerep, (VoidStar)x, (long)typespecmap_(x));
            break;
        }
        if (typep != NULL) *typep = restype;
        return;
    }
}

static void dbg_addvar(Symstr *name, int32 t, int32 sourcepos,
                       unsigned32 stgclass, Symstr *base, int32 addr, TypeExpr *type)
{   DbgList *p = DbgListAlloc(DEB_VAR);
    p->debsort = ITEMVAR;
    p->car.DEB_VAR.type = t;
    p->car.DEB_VAR.sourcepos = sourcepos;
    p->car.DEB_VAR.stgclass = stgclass;
    p->car.DEB_VAR.location = addr;
    p->car.DEB_VAR.sym = name;
    p->car.DEB_VAR.base = base;
    p->cdr = dbglist;
    dbglist = p;
    dbgloc += 20 + padstrlen(strlen(symname_(name)));
    if (type != NULL) dbg_typerep(type, &p->car.DEB_VAR.type);
}

void dbg_topvar(Symstr *name, int32 addr, TypeExpr *t, int stgclass,
                FileLine fl)
/* For scoping reasons this only gets called on top-level variables (which */
/* are known to be held in global store).  (Does this matter?)             */
{   if (usrdbg(DBG_PROC))
    { /* nb bss => external here.  The effect is only to cause the table item
         to be 0+symbol, rather than addr+data seg
       */
        DbgList *p;
        Symstr *base = NULL;
        StgClass stg = (stgclass & DS_REG) ? C_REG :
                       (stgclass & DS_EXT) ? C_EXTERN :
                                             C_STATIC;
        if (stgclass & (DS_EXT|DS_BSS))
            base = name, addr = 0;
#ifdef CONST_DATA_IN_CODE
        else if (stgclass & DS_CODE)
            base = bindsym_(constdatasegment);
#endif
        else if (!(stgclass & DS_REG))
            base = bindsym_(datasegment);

        if (debugging(DEBUG_Q))
            cc_msg("top var $r @ %.6lx\n", name, (long)addr);
        if (stgclass != 0 && stg != C_REG)
            for ( p = dbglist ; p!=NULL ; p = p->cdr )
                if ( p->debsort==ITEMVAR &&
                     (p->car.DEB_VAR.stgclass==C_EXTERN ||
                      p->car.DEB_VAR.stgclass==C_STATIC) &&
                     p->car.DEB_VAR.location==0 &&
                     p->car.DEB_VAR.sym == name) {
                    p->car.DEB_VAR.sourcepos = fl.l;
                    p->car.DEB_VAR.location = addr;
                    p->car.DEB_VAR.base = base;
                    return;
                }
        dbg_addvar(name, 0, fl.l, stg, base, addr, t);
    }
}

static int32 dbg_typeinternal(Symstr *name, int32 t, TypeExpr *type)
/* This procedure is called on a type-declaration internal to a procedure
 * (from dbg_scope, after the syntax tree has evaporated), and on a global
 * one, with the syntax tree in place.  The latter therefore goes through
 * dbg_type, which internalises the type.
 */
{
    if (isgensym(name)) {
        dbg_typerep(type, NULL);
        return 0;
    } else {
        DbgList *p = DbgListAlloc(DEB_TYPE);
        if (debugging(DEBUG_Q))
            cc_msg("type $r\n", name);
        p->debsort = ITEMTYPE;
        p->car.DEB_TYPE.type = t;
        p->car.DEB_TYPE.typex = type;
        p->car.DEB_TYPE.loc = dbgloc;
        p->car.DEB_TYPE.name = symname_(name);
        p->cdr = dbglist;
        dbglist = p;
        dbgloc += 8 + padstrlen(strlen(symname_(name)));
        if (type != NULL) dbg_typerep(type, &p->car.DEB_TYPE.type);
        return p->car.DEB_TYPE.loc;
    }
}

void dbg_type(Symstr *name, TypeExpr *t, FileLine fl)
/* This only gets called on top-level types (which are known to be held in
 * global store).
 */
{
    IGNORE(fl);
    (void)dbg_typeinternal(name, 0, t);
}

static Deb_filecoord *cur_proc_coord;

void dbg_proc(Binder *b, TagBinder *parent, bool ext, FileLine fl)
{   IGNORE(ext);   /* ASD has no use for static/external information */
    IGNORE(parent);
    if (usrdbg(DBG_PROC))
    {   Symstr *name = bindsym_(b);
        DbgList *p = DbgListAlloc(DEB_PROC);
        TypeExpr *t = bindtype_(b);
        char *s = symname_(name);
        if (debugging(DEBUG_Q)) cc_msg("startproc $r\n", name);
        t = princtype(t);
        p->debsort = ITEMPROC;
        if (h0_(t) != t_fnap) syserr(syserr_dbg_proc);
        dbg_typerep(typearg_(t), &p->car.DEB_PROC.type);
        p->car.DEB_PROC.args = length((List *)typefnargs_(t));
        p->car.DEB_PROC.sourcepos = fl.l;
        p->car.DEB_PROC.entryaddr = 0;       /* fill in at dbg_enterproc */
        p->car.DEB_PROC.bodyaddr = 0;        /* fill in at dbg_bodyproc */
        p->car.DEB_PROC.endproc = 0;         /* fill in at dbg_xendproc   */
        p->car.DEB_PROC.fileentry = fl.f;
        p->car.DEB_PROC.name = s;
        p->car.DEB_PROC.codeseg = bindsym_(codesegment);
        p->cdr = dbglist;              /* do this last (typerep above) */
        dbglistproc = dbglist = p;       /* so can be filled in */
        dbgloc += 32 + padstrlen(strlen(s));
    }
    if (usrdbg(DBG_LINE))
        cur_proc_coord = (Deb_filecoord *)fl.p;
    dbg_returnlist = 0, dbg_loclist = 0;
}

void dbg_enterproc(void)
{   if (usrdbg(DBG_PROC))
    {   DbgList *p = dbglistproc;

        if (p == 0 || p->debsort != ITEMPROC || p->car.DEB_PROC.entryaddr != 0)
            syserr(syserr_dbg_proc1);
        if (debugging(DEBUG_Q))
            cc_msg("enter '%s' @ %.6lx\n",
                    p->car.DEB_PROC.name, (long)codebase);
        p->car.DEB_PROC.entryaddr = codebase;
    }
    if (usrdbg(DBG_LINE))
       dbg_addcodep(cur_proc_coord, codebase);
}

/* The following routine records the post-entry codeaddr of a proc */
void dbg_bodyproc(void)
{   if (usrdbg(DBG_PROC))
    {   DbgList *p = dbglistproc;
        if (p == 0 || p->debsort != ITEMPROC || p->car.DEB_PROC.bodyaddr != 0)
            syserr(syserr_dbg_proc1);
        if (debugging(DEBUG_Q))
            cc_msg("body '%s' @ %.6lx\n",
                    p->car.DEB_PROC.name, (long)(codebase+codep));
        p->car.DEB_PROC.bodyaddr = codebase+codep;
    }
}

void dbg_return(int32 addr)
{   if (usrdbg(DBG_PROC))
    {   if (debugging(DEBUG_Q))
            cc_msg("return @ %.6lx\n", addr);
        dbg_returnlist = (Dbg_Return*) global_cons2(SU_Dbg, dbg_returnlist, addr);
    }
}

void dbg_xendproc(FileLine fl)
{   if (bindsym_(codesegment) == baseseg.sym) baseseg.len = codebase+codep;
    if (usrdbg(DBG_PROC))
    {   DbgList *q = dbglistproc;
        DbgList *p = DbgListAlloc(DEB_ENDPROC);
        if (q == 0 || q->debsort != ITEMPROC || q->car.DEB_PROC.endproc != 0)
            syserr(syserr_dbg_proc1);
        if (debugging(DEBUG_Q))
            cc_msg("endproc '%s' @ %.6lx\n",
                    q->car.DEB_PROC.name, (long)(codebase+codep));
        q->car.DEB_PROC.endproc = dbgloc;
        p->debsort = ITEMENDPROC;
        p->car.DEB_ENDPROC.sourcepos = fl.l;
        p->car.DEB_ENDPROC.endaddr = codebase+codep;
        p->car.DEB_ENDPROC.fileentry = fl.f;
        p->car.DEB_ENDPROC.retaddrs = dbg_returnlist;
        p->car.DEB_ENDPROC.codeseg = bindsym_(codesegment);
        p->cdr = dbglist;
        dbglist = p;
        dbgloc += 20 + 4*length((List *)dbg_returnlist);
        dbg_loclist = 0, dbg_returnlist = 0;    /* for safety */
    }
}

/* dbg_locvar() registers the name and line of a declaration, and internalises
 * the type.  Location info cannot be added until after register allocation.
 * See also dbg_scope which completes.
 * (Type internalisation cannot be done then, because by that time the tree
 * has evaporated).
 * Also remember that dead code elimination may remove some decls.
 */
void dbg_locvar(Binder *name, FileLine fl)
{   if (usrdbg(DBG_VAR) && !isgensym(bindsym_(name))) {
        /* local to a proc */
        Dbg_LocList *p = (Dbg_LocList*) BindAlloc(sizeof(Dbg_LocList));
        if (debugging(DEBUG_Q))
            cc_msg("note loc var $b\n", name);
        p->cdr = dbg_loclist;
        p->name = name;
        p->pos = fl.l;
        p->typefrefs = NULL;
        p->size = sizeoftypelegal(bindtype_(name)) ? sizeoftype(bindtype_(name)) :
                                                     sizeof_int;
        dbg_typerep(bindtype_(name), &p->typeref);
        dbg_loclist = p;
    }
}

static Dbg_LocList *dbg_findloclist(Binder *b)
{   Dbg_LocList *p;
    for (p = dbg_loclist; p != NULL; p = p->cdr)
        if (p->name == b) return p;
    return NULL;
}

void dbg_locvar1(Binder *b)
{   Symstr *name = bindsym_(b);
    Symstr *base = NULL;
    Dbg_LocList *p = dbg_findloclist(b);
    unsigned32 stgclass;
    int stgclassname;
    int32 addr = bindaddr_(b);
    if (p == NULL || p->pos == -1)
    {   if (debugging(DEBUG_Q))
            cc_msg(" omitted");
        return;   /* invented variable name (e.g. s_let) */
    }
    switch (bindstg_(b) & PRINCSTGBITS) {
    default:
    defolt:
        syserr(syserr_dbg_table, name, (long)bindstg_(b), (long)addr);
        return;
    case bitofstg_(s_typedef):
        if (debugging(DEBUG_Q)) cc_msg(" <typedef>");
        {   int32 loc = dbg_typeinternal(name, p->typeref, NULL);
            Dbg_TypeFRef *ref = p->typefrefs;
            for (; ref != NULL; ref = cdr_(ref))
                *fr_typeref_(ref) = dbg_mk_typerep(-loc, fr_ptrcnt_(ref));
        }
        return;                   /* dbg_type deals with s_typedef vars  */
    case bitofstg_(s_extern):
        if (debugging(DEBUG_Q)) cc_msg(" <extern>");
        return;                   /* local externs do not allocate store */
    case bitofstg_(s_static):
        stgclass = C_STATIC, stgclassname = 'S';
        if (bindstg_(b) & b_fnconst) {
            base = bindsym_(b);
            addr = 0;
        } else
            base =
#ifdef TARGET_HAS_BSS
                   (bindstg_(b) & u_bss) ? bindsym_(bsssegment) :
#endif
#ifdef CONST_DATA_IN_CODE
                   (bindstg_(b) & u_constdata) ? bindsym_(constdatasegment) :
#endif
                                            bindsym_(datasegment);
        break;
    case bitofstg_(s_auto):
        if (bindxx_(b) != GAP) {
            stgclass = C_REG, stgclassname = 'R', addr = register_number(bindxx_(b));
        } else switch (addr & BINDADDR_MASK) {
        case BINDADDR_ARG:
            stgclass = C_AUTO, stgclassname = 'A', addr = local_fpaddress(b);
            if (p->size < 4 && !target_lsbytefirst) addr += 4 - p->size;
            break;
        case BINDADDR_LOC:
            stgclass = C_AUTO, stgclassname = 'P', addr = local_fpaddress(b);
            if (p->size < 4 && !target_lsbytefirst) addr += 4 - p->size;
            break;
        case 0:
          /* probably declared but not used case (where addr is still a bindlist) */
            if ((bindstg_(b) & b_bindaddrlist) != 0) {
               if (debugging(DEBUG_Q)) cc_msg(" unused - omitted");
               return; }
          /* otherwise, fall into internal error case */
        default:
            goto defolt;
        }
        break;
    }
    if (debugging(DEBUG_Q))
        cc_msg(" %c %lx", stgclassname, (long)addr);
    dbg_addvar(name, p->typeref, p->pos, stgclass, base, addr, NULL);
    if (p->typeref == 0) {
      /* (pointer to) an as yet undefined type. We must find and amend the
       * forward reference record
       */
        Dbg_TypeFRef *s = dbg_structRefs;
        DbgList *var = dbglist;
        for (; s != NULL; s = fr_nextstruct_(s)) {
            Dbg_TypeFRef *refs = s;
            for (; refs != NULL; refs = cdr_(refs))
                if (fr_typeref_(refs) == &p->typeref) {
                    fr_typeref_(refs) = &var->car.DEB_VAR.type;
                    return;
                }
        }
    }
}

static bool asd_scope_i(BindListList *newbll, BindListList *oldbll, int32 entering)
{   if (length((List *)newbll) > 1) {
        BindListList *bll = newbll;
        DbgList *last = NULL;
        for (bll = newbll; bll != oldbll; bll = bll->bllcdr) {
            if (oldbll == NULL && bll->bllcdr == NULL)
                break;
            if (bll == NULL) syserr(syserr_dbg_scope);
            if (bll->bllcar != 0) {
                DbgList *p = DbgListAlloc(DEB_SCOPE);
                dbglistscope = p;
                p->cdr = dbglist;
                p->debsort = entering >= 0 ? ITEMSCOPEBEGIN : ITEMSCOPEEND;
                p->car.DEB_SCOPE.s.next = last; /* filled in soon by INFOSCOPE */
                p->car.DEB_SCOPE.codeseg = bindsym_(codesegment);
                dbgloc += 8;
                dbglist = p;
                last = p;
            }
        }
    }
    if (debugging(DEBUG_Q)) cc_msg("scope %ld\n", entering);
    for (; newbll != oldbll; newbll = newbll->bllcdr)
    {   SynBindList *bl;
        if (newbll == NULL) syserr(syserr_dbg_scope);
        for (bl = newbll->bllcar; bl; bl = bl->bindlistcdr)
        {   Binder *b = bl->bindlistcar;
            if (bindstg_(b) & b_dbgbit) continue; /* for this and next line */
            bindstg_(b) |= b_dbgbit;              /* see end of routine cmt */
            if (debugging(DEBUG_Q))
                cc_msg("  %s $b",
                        entering>=0 ? "binding" : "unbinding",
                        b);
            if (entering >= 0)
                dbg_locvar1(b);
            if (debugging(DEBUG_Q))
                cc_msg("\n");
        }
    }
    return YES;
    /* Ask for INFOSCOPE item to get called back more or less immediately */
    /* from the local cg (INFOSCOPE item) to fill in the codeaddr         */
}

bool dbg_scope(BindListList *newbll, BindListList *oldbll)
{   int32 entering = length((List *)newbll) - length((List *)oldbll);
    if (entering == 0 && newbll == oldbll) return NO;
    if (entering < 0)
    {   BindListList *t = newbll;
        newbll = oldbll, oldbll = t;
    }
    if (length((List *)newbll) > 1) {
        BindListList *bll_n = newbll;
        for (bll_n = newbll; bll_n != oldbll; bll_n = bll_n->bllcdr) {
            if (oldbll == NULL && bll_n->bllcdr == NULL)
                break;
            if (bll_n == NULL) {
            /* New is not a superset of old. Can happen if a block containing */
            /* a scope end is deleted                                         */
                for (bll_n = newbll; bll_n != NULL; bll_n = bll_n->bllcdr) {
                    BindListList *bll_o;
                    for (bll_o = oldbll; bll_o != NULL; bll_o = bll_o->bllcdr)
                        if (bll_o == bll_n) {
                        /* Here, we regret having (maybe) swapped new and old */
                            int32 l_o = length((List *)bll_o);
                            if (entering > 0) {
                                asd_scope_i(oldbll, bll_o, l_o - length((List *)oldbll));
                                return asd_scope_i(newbll, bll_o, length((List *)newbll) - l_o);
                            } else {
                                asd_scope_i(newbll, bll_o, l_o - length((List *)newbll));
                                return asd_scope_i(oldbll, bll_o, length((List *)oldbll) - l_o);
                            }
                        }
                }
                syserr(syserr_dbg_scope);
            }
        }
    }
    return asd_scope_i(newbll, oldbll, entering);
}

/* Dummy procedure not yet properly implemented, included here to keep in */
/* step with dbx.c */
void dbg_commblock(Binder *b, SynBindList *members, FileLine fl)
{
    b = b;
    members = members;
    fl = fl;
}

void dbg_define(char const *name, bool objectmacro, char const *body,
                dbg_ArgList const *args, FileLine fl) {
    DbgList *p = DbgListAlloc(DEB_DEFINE);
    size_t space = 24;
    p->debsort = ITEMDEFINE;
    p->car.DEB_DEFINE.name = name;
    p->car.DEB_DEFINE.fl = fl;
    p->car.DEB_DEFINE.arglist = args;
    p->car.DEB_DEFINE.body = body;
    if (objectmacro)
        p->car.DEB_DEFINE.argcount = -1;
    else {
        int32 n = 0;
        p->car.DEB_DEFINE.argcount = n;
        for (; args != NULL; args = args->next, n++)
            space += (size_t)padstrlen(strlen(args->name));
        p->car.DEB_DEFINE.argcount = n;
    }
    space += (size_t)padstrlen(strlen(name))+(size_t)padstrlen(strlen(body));
    p->car.DEB_DEFINE.space = space;
    dbgloc += space;
    p->cdr = dbglist;
    dbglist = p;
    if (debugging(DEBUG_Q)) {
        cc_msg("%s:%d #define %s", fl.f, fl.l, name);
        if (!objectmacro) {
            char const *s = "";
            cc_msg("(");
            for (args = p->car.DEB_DEFINE.arglist; args != NULL; args = args->next) {
                cc_msg("%s%s", s, args->name);
                s = ", ";
            }
            cc_msg(")");
        }
        cc_msg(" %s\n", body);
    }
}

void dbg_undef(char const *name, FileLine fl) {
    DbgList *p = DbgListAlloc(DEB_UNDEF);
    p->debsort = ITEMUNDEF;
    p->car.DEB_UNDEF.name = name;
    p->car.DEB_UNDEF.fl = fl;
    p->cdr = dbglist;
    dbglist = p;
    dbgloc += 12 + padstrlen(strlen(name));
    if (debugging(DEBUG_Q)) {
        cc_msg("%s:%d #undef %s\n", fl.f, fl.l, name);
    }
}

void dbg_include(char const *filename, char const *path, FileLine fl) {
  /* No use for this in generating ASD tables */
    IGNORE(filename); IGNORE(path); IGNORE(fl);
}

void dbg_notepath(char const *pathname) {
  /* No use for this in generating ASD tables */
    IGNORE(pathname);
}

/* armobj.c calls writedebug to generate the debugging tables.   */
/* It must format them and then call obj_writedebug()            */
static void asdtables_write(void)
{   DbgList *p;
    int32 check = 0;
    int32 fileinfobase;
    int32 fileinfosize;
    int32 v[7];
    char c[256];
    DataXref *xrefs = NULL;
    /* produce structure descriptors for forward referenced structures for
     * which there hasn't been a real declaration
     */
    obj_startdebugarea(DEBUGAREANAME);
    while (dbg_structRefs != 0) {
        if (fr_typeref_(dbg_structRefs) != 0)
            dbg_structentry(fr_sort_(dbg_structRefs), NULL, 0, dbg_structRefs, 0);
        dbg_structRefs = fr_nextstruct_(dbg_structRefs);
    }
    {   /* add a sentinel to the ->nextincode list to flush last fileline */
        dbg_coord_sentinel.codeaddr = codebase+codep;
        if (dbg_coord_p) {
            dbg_coord_q = dbg_coord_q->nextincode = &dbg_coord_sentinel;
            dbg_coord_sentinel.codeseg = bindsym_(codesegment);
        } else
            dbg_coord_p = dbg_coord_q = &dbg_coord_sentinel;
    }
    fileinfobase = dbgloc;
    fileinfosize = dbg_fileinfo(NO,0,0,&xrefs);
    dbgloc += fileinfosize;
    for (p = dbglist = (DbgList *)dreverse((List *)dbglist); p != NULL; p = p->cdr) {
        int sort = (int)p->debsort;
        switch (sort) {
        default:
            syserr(syserr_dbg_write, (long)sort);
            break;
        case ITEMSECTION:
          { char h[4];
            size_t n = bcpl_string(c, p->car.DEB_SECTION.name);
            h[0] = LANG_C;
            h[1] = (usrdbg(DBG_LINE) ? 1 : 0) +
                   (usrdbg(DBG_PROC|DBG_VAR) ? 2 : 0);
            h[2] = 0;
            h[3] = OldAsdTables ? ASD_FORMAT_VERSION-1 : ASD_FORMAT_VERSION;
            v[0] = 0, v[2] = baseseg.len;
            if (baseseg.len > 0)
                xrefs = dbg_relocate(xrefs, check+8, p->car.DEB_SECTION.codeseg);
            v[1] = 0, v[3] = data_size();
            if (data_size() != 0)
                xrefs = dbg_relocate(xrefs, check+12, bindsym_(datasegment));
            v[4] = fileinfobase;
            v[5] = dbgloc;
            dbg_hdr(sort, n+32);
            obj_writedebug(h, 4);
            obj_writedebug(v, 6+DBG_INTFLAG);
            obj_writedebug(c, n);
            check += (int32)n+32;
            break;
          }
        case ITEMPROC:
          { size_t n = bcpl_string(c, p->car.DEB_PROC.name);
            v[0] = p->car.DEB_PROC.type;
            v[1] = p->car.DEB_PROC.args;
            v[2] = p->car.DEB_PROC.sourcepos;
            v[3] = p->car.DEB_PROC.entryaddr;
            xrefs = dbg_relocate(xrefs, check+16, p->car.DEB_PROC.codeseg);
            v[4] = p->car.DEB_PROC.bodyaddr;
            xrefs = dbg_relocate(xrefs, check+20, p->car.DEB_PROC.codeseg);
            v[5] = p->car.DEB_PROC.endproc;
            v[6] = fileinfobase + dbg_filetooffset(p->car.DEB_PROC.fileentry);
            dbg_hdr(sort, n+32);
            obj_writedebug(v, 7+DBG_INTFLAG);
            obj_writedebug(c, n);
            check += (int32)n+32;
            break;
          }
        case ITEMENDPROC:
          { Dbg_Return *l = p->car.DEB_ENDPROC.retaddrs;
            int n = (int)length((List *)l);
            v[0] = p->car.DEB_ENDPROC.sourcepos;
            v[1] = p->car.DEB_ENDPROC.endaddr;
            xrefs = dbg_relocate(xrefs, check+8, p->car.DEB_ENDPROC.codeseg);
            v[2] = fileinfobase + dbg_filetooffset(p->car.DEB_ENDPROC.fileentry);
            v[3] = n;
            dbg_hdr(sort, 20+4*n);
            obj_writedebug(v, 4+DBG_INTFLAG);
            check += 20;
            for (; l ; l = l->cdr)
            {   obj_writedebug(&(l->car), 1+DBG_INTFLAG);
                xrefs = dbg_relocate(xrefs, check, p->car.DEB_ENDPROC.codeseg);
                check += 4;
            }
            break;
          }
        case ITEMVAR:
          { size_t n = bcpl_string(c, symname_(p->car.DEB_VAR.sym));
            v[0] = p->car.DEB_VAR.type;
            v[1] = p->car.DEB_VAR.sourcepos;
            v[2] = (int32) p->car.DEB_VAR.stgclass;
            v[3] = p->car.DEB_VAR.location;
            if (p->car.DEB_VAR.stgclass == C_EXTERN ||
                p->car.DEB_VAR.stgclass == C_STATIC) {
            /* Make sure that occurrence in debug tables alone doesn't require
               a symbol to be resolved.  symext_(sym) is non-null if the symbol
               has been otherwise referenced.
             */
                Symstr *s = p->car.DEB_VAR.base;
                obj_symref(s, symext_(s) == NULL ? xr_data|xr_weak : xr_data, 0);
                xrefs = dbg_relocate(xrefs, check+16, p->car.DEB_VAR.base);
            }
            dbg_hdr(sort, n+20);
            obj_writedebug(v, 4+DBG_INTFLAG);
            obj_writedebug(c, n);
            check += (int32)n+20;
            break;
          }
        case ITEMTYPE:
          { size_t n = bcpl_string(c, p->car.DEB_TYPE.name);
            dbg_hdr(sort, n+8);
            obj_writedebug(&p->car.DEB_TYPE.type, 1+DBG_INTFLAG);
            obj_writedebug(c, n);
            check += (int32)n+8;
            break;
          }
        case ITEMCLASS:
        case ITEMUNION:
        case ITEMSTRUCT:
          { Dbg_Structelt *q = p->car.DEB_STRUCT.elts;
            size_t space = (size_t)p->car.DEB_STRUCT.space;
            v[0] = length((List *)q);
            v[1] = p->car.DEB_STRUCT.size;
            dbg_hdr(sort, space);
            obj_writedebug(v, 2+DBG_INTFLAG);
            check += 12, space -= 12;
            for (; q; q = cdr_(q))
            {   size_t n = bcpl_string(c, q->name);
                obj_writedebug(&q->offset, 2+DBG_INTFLAG);
                obj_writedebug(c, n);
                check += (int32)n+8, space -= n+8;
            }
            if (space != 0) syserr(syserr_dbg_struct);
            break;
          }
        case ITEMENUMC:
          { Dbg_Enumelt *q = p->car.DEB_ENUM.elts;
            size_t space = (size_t)p->car.DEB_ENUM.space;
            v[0] = p->car.DEB_ENUM.container;
            v[1] = length((List *)q);
            v[2] = q == NULL ? 0 : q->val;
            /* An enumeration with no enumerators is a bizarre thing, but */
            /* legal in C++.                                              */
            dbg_hdr(sort, space);
            obj_writedebug(v, 3+DBG_INTFLAG);
            check += 16, space -= 16;
            for (; q != NULL; q = cdr_(q))
            {   size_t n = bcpl_string(c, q->name);
                obj_writedebug(c, n);
                check += n, space -= n;
            }
            if (space != 0) syserr(syserr_dbg_struct);
            break;
          }
        case ITEMENUMD:
          { Dbg_Enumelt *q = p->car.DEB_ENUM.elts;
            size_t space = (size_t)p->car.DEB_ENUM.space;
            v[0] = p->car.DEB_ENUM.container;
            v[1] = length((List *)q);
            dbg_hdr(sort, space);
            obj_writedebug(v, 2+DBG_INTFLAG);
            check += 12, space -= 12;
            for (; q != NULL; q = cdr_(q))
            {   size_t n = bcpl_string(c, q->name);
                obj_writedebug(&q->val, 1+DBG_INTFLAG);
                obj_writedebug(c, n);
                check += (int32)n+4, space -= n+4;
            }
            if (space != 0) syserr(syserr_dbg_struct);
            break;
          }
        case ITEMARRAY:
            dbg_hdr(sort, 24);
            obj_writedebug(&p->car.DEB_ARRAY.size, 5+DBG_INTFLAG);
            check += 24;
            break;
        case ITEMBITFIELD:
            dbg_hdr(sort, 16);
            obj_writedebug(&p->car.DEB_BF.type, 2+DBG_INTFLAG);
            obj_writedebug(&p->car.DEB_BF.size, 4);
            check += 16;
            break;
        case ITEMSCOPEBEGIN:
        case ITEMSCOPEEND:
            xrefs = dbg_relocate(xrefs, check+4, p->car.DEB_SCOPE.codeseg);
            dbg_hdr(sort, 8);
            obj_writedebug(&p->car.DEB_SCOPE.s.codeaddr, 1+DBG_INTFLAG);
            check += 8;
            break;
        case ITEMUNDEF:
          { size_t n = bcpl_string(c, p->car.DEB_UNDEF.name);
            dbg_hdr(sort, n+12);
            v[0] = fileinfobase + dbg_filetooffset(p->car.DEB_UNDEF.fl.f);
            v[1] = p->car.DEB_UNDEF.fl.l;
            obj_writedebug(v, 2+DBG_INTFLAG);
            obj_writedebug(c, n);
            check += (int32)n+12;
            break;
          }
        case ITEMDEFINE:
          { size_t n1 = bcpl_string(c, p->car.DEB_DEFINE.name);
            size_t n2 = strlen(p->car.DEB_DEFINE.body);
            size_t n2p = (size_t)padstrlen(n2);
            int32 space = 0;
            dbg_hdr(sort, (size_t)p->car.DEB_DEFINE.space);
            v[0] = fileinfobase + dbg_filetooffset(p->car.DEB_DEFINE.fl.f);
            v[1] = p->car.DEB_DEFINE.fl.l;
            v[2] = check + 24 + n1;
            v[3] = p->car.DEB_DEFINE.argcount;
            v[4] = p->car.DEB_DEFINE.argcount <= 0 ? 0 : check + 24 + n1 + n2p;
            obj_writedebug(v, 5+DBG_INTFLAG);
            obj_writedebug(c, n1);
            obj_writedebug(p->car.DEB_DEFINE.body, n2);
            v[0] = 0;
            obj_writedebug(v, (int32)n2p-n2);
            space = 24 + (int32)n1 + n2p;
            {   dbg_ArgList const *ap = p->car.DEB_DEFINE.arglist;
                for (; ap != NULL; ap = ap->next) {
                    n1 = bcpl_string(c, ap->name);
                    obj_writedebug(c, n1);
                    space += n1;
                }
            }
            check += space;
            break;
          }
        }
    }
    if (check != fileinfobase)
        syserr(syserr_dbg_fileinfobase, (long)fileinfobase, (long)check);
    check += dbg_fileinfo(YES, fileinfosize, fileinfobase, &xrefs);
    if (check != dbgloc)
        syserr(syserr_dbgloc, (long)dbgloc, (long)check);

    obj_enddebugarea(DEBUGAREANAME, xrefs);
}


#ifdef TARGET_HAS_FP_OFFSET_TABLES

typedef struct FPFragmentList FPFragmentList;
struct FPFragmentList {
  FPFragmentList *cdr;
  Symstr *codeseg;
  int32 n;
  ItemFPMapFragment frag;
};

static struct {
  int32 size;
  FPFragmentList *frags, **frags_tail;
} fpmap;

#define NoSaveAddr ((asd_Address)-1)

void obj_notefpdesc(ProcFPDesc const *fpd) {
  FPList *p = fpd->fplist,
         *lastp;
  int32 addr = fpd->startaddr;
  int32 offset = fpd->initoffset;
  int32 saveaddr = fpd->saveaddr;
  for (; (lastp = p) != NULL; p = cdr_(p)) {
    int32 bytes = 0;
    int32 startaddr = addr;
    for (; p != NULL; p = cdr_(p)) {
      unsigned32 addrdiff = p->addr - addr;
      addr = p->addr;
      if (addrdiff < 0x100 && -0x80 < p->change && p->change < 0x80) {
        if (bytes+4 >= 0x10000-6*4) break;
        bytes += 2;
      } else if (addrdiff < 0x10000 && -0x8000 < p->change && p->change < 0x8000) {
        if (bytes+8 >= 0x10000-6*4) break;
        bytes += 6;
      } else
        break;
    }
    { int32 n = (bytes + 3) & ~3L;
      int32 w = (int32)ITEMFPMAPFRAG;
      FPFragmentList *fl = (FPFragmentList *)DbgAlloc(sizeof(FPFragmentList)+n-1);
      char *fp = fl->frag.b;
      fl->frag.bytes = bytes;
      fl->codeseg = fpd->codeseg;
      fl->n = n;
      fl->frag.marker = w | ((n + 6*4) << 16);
      fl->frag.codestart = startaddr;
      /* ECN: function may not have a trailing stack adjust if the final
       * return is by a branch to an earlier return so use endaddr in
       * this case.
       */
      if (p == NULL)
        fl->frag.codesize = fpd->endaddr - startaddr;
      else
        fl->frag.codesize = addr - startaddr;
      fl->frag.saveaddr = saveaddr;
      fl->frag.initoffset = offset;
      cdr_(fl) = NULL;
      *fpmap.frags_tail = fl; fpmap.frags_tail = &cdr_(fl);
      fpmap.size += (n + 6*4);
      for (; lastp != p; lastp = cdr_(lastp)) {
        unsigned32 addrdiff = lastp->addr - startaddr;
        startaddr = lastp->addr;
        if (addrdiff < 0x100 && -0x80 < lastp->change && lastp->change < 0x80) {
          fp[0] = (char)addrdiff;
          fp[1] = (char)lastp->change;
          fp += 2;
        } else {
          fp[0] = fp[1] = 0;
          if (target_lsbytefirst) {
            fp[2] = (char)addrdiff; fp[3] = (char)(addrdiff >> 8);
            fp[4] = (char)lastp->change; fp[5] = (char)(lastp->change >> 8);
          } else {
            fp[2] = (char)(addrdiff >> 8); fp[3] = (char)addrdiff;
            fp[4] = (char)(lastp->change >> 8); fp[5] = (char)lastp->change;
          }
          fp += 6;
        }
        offset += lastp->change;
      }
      for (; bytes < n; bytes++) fl->frag.b[bytes] = 0;
    }
    if (p == NULL) break;
    saveaddr = NoSaveAddr;
    addr = p->addr;
    offset += p->change;
  }
}

static void fpmap_write(void) {
    DataXref *xrefs = NULL;
    if (fpmap.size != 0) {
      char h[4];
      int32 v[7];
      int32 offset = 0;
      obj_startdebugarea(FPMAPAREANAME);
      h[0] = LANG_C;
      h[1] = 4;
      h[2] = 0;
      h[3] = ASD_FORMAT_VERSION;
      v[0] = 0, v[2] = baseseg.len;
      if (baseseg.len > 0)
        xrefs = dbg_relocate(xrefs, offset+8, baseseg.sym);
      v[1] = v[3] = 0;
      v[4] = 0;
      v[5] = fpmap.size;
      v[6] = 0;
      dbg_hdr(ITEMSECTION, 9*4);
      obj_writedebug(h, 4);
      obj_writedebug(v, 7+DBG_INTFLAG);
      offset += 9*4;
      { FPFragmentList *fp = fpmap.frags;
        for (; fp != NULL; fp = cdr_(fp)) {
          xrefs = dbg_relocate(xrefs, offset+8, fp->codeseg);
          if (fp->frag.saveaddr == NoSaveAddr)
            fp->frag.saveaddr = 0;
          else
            xrefs = dbg_relocate(xrefs, offset+12, fp->codeseg);
          obj_writedebug(&fp->frag, 6+DBG_INTFLAG);
          obj_writedebug(fp->frag.b, fp->n);
          offset += fp->n + offsetof(ItemFPMapFragment, b);
        }
      }
      obj_enddebugarea(FPMAPAREANAME, xrefs);
    }
}

static int32 fpmap_size(void) {
    return fpmap.size != 9*4 ? fpmap.size : 0;
}

#endif

void dbg_finalise(void)
{
    dbg_init_done = dbg_sub_init_done = NO;
}

void dbg_final_src_codeaddr(int32 a, int32 b) {
    IGNORE(a); IGNORE(b);
}

bool dbg_debugareaexists(char const *name) {
    if (StrEq(name, DEBUGAREANAME))
        return asd_tablesize() != 0;
#ifdef TARGET_HAS_FP_OFFSET_TABLES
    else if (StrEq(name, FPMAPAREANAME))
        return fpmap_size() != 0;
#endif
    return NO;
}

void dbg_writedebug(void) {
    if (asd_tablesize() != 0) asdtables_write();
#ifdef TARGET_HAS_FP_OFFSET_TABLES
    if (fpmap_size() != 0) fpmap_write();
#endif
}

static void dbg_sub_init(void)
{
    dbglist = 0;
    baseseg.len = 0;
    dbglistproc = 0;
    dbglistscope = 0;
    dbg_filelist = 0, dbg_coord_p = 0;
    dbg_loclist = 0, dbg_returnlist = 0;    /* for safety */
    dbg_structRefs = 0;
    dbgloc = 0;
    dbg_sub_init_done = YES;
#ifdef TARGET_HAS_FP_OFFSET_TABLES
    fpmap.size = 9*4;
    fpmap.frags = NULL; fpmap.frags_tail = &fpmap.frags;
#endif
}

void dbg_setformat(char const *form) {
    oldformat = (form[0] != 0);
}

bool dbg_needsframepointer(void) {
#ifdef TARGET_HAS_FP_OFFSET_TABLES
    return 0;
#else
    return 1;
#endif
}

void dbg_init(void)
{
    obj_notedebugarea(DEBUGAREANAME);
#ifdef TARGET_HAS_FP_OFFSET_TABLES
    obj_notedebugarea(FPMAPAREANAME);
#endif
    if (!dbg_sub_init_done) dbg_sub_init();
    baseseg.sym = bindsym_(codesegment);
    if (usrdbg(DBG_ANY))
    {
        /* leave space for the DEB_SECTION item */
        DbgList *p = DbgListAlloc(DEB_SECTION);
        p->debsort = ITEMSECTION;
        p->car.DEB_SECTION.name = objectfile == NULL ? "" : objectfile;
        p->car.DEB_SECTION.codeseg = bindsym_(codesegment);
        p->cdr = NULL;
        {   DbgList *q, **pp = &dbglist;
            for (; (q = *pp) != NULL; pp = &q->cdr) continue;
            *pp = p;
        }
        dbgloc += 32 + padstrlen(strlen(p->car.DEB_SECTION.name));
        if (usrdbg(DBG_LINE))
        {   Deb_filelist *x = dbg_filelist;
            for (; x != NULL; x = x->nextfile) {
                Deb_filecoord *l = x->linelist;
                for (; l != NULL; l = l->nextinfile)
                    l->codeseg = bindsym_(codesegment);
            }
        }
    }
    dbg_init_done = YES;
}

#endif /* TARGET_HAS_ASD */

#endif /* TARGET_HAS_DEBUGGER */

/* End of arm/asd.c */
