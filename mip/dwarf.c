/*
 * C compiler file mip/dwarf.c
 * Copyright:   (C) 1995, Advanced RISC Machines Limited. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

/* The debug table formatter for DWARF debug tables, for embedding in ELF
   or other object files.
 */

#ifdef __STDC__
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <stddef.h>

#include "globals.h"

#if defined(TARGET_HAS_DEBUGGER) && defined(TARGET_HAS_DWARF)

#include "syn.h" /* for syn_note_generated_fn */

#ifdef TARGET_HAS_MULTIPLE_DEBUG_FORMATS

#define dbg_tableindex dwarf_tableindex
#define dbg_notefileline dwarf_notefileline
#define dbg_addcodep dwarf_addcodep
#define dbg_scope dwarf_scope
#define dbg_topvar dwarf_topvar
#define dbg_type dwarf_type
#define dbg_proc dwarf_proc
#define dbg_locvar dwarf_locvar
#define dbg_locvar1 dwarf_locvar1
#define dbg_commblock dwarf_commblock
#define dbg_enterproc dwarf_enterproc
#define dbg_bodyproc dwarf_bodyproc
#define dbg_return dwarf_return
#define dbg_xendproc dwarf_xendproc
#define dbg_define dwarf_define
#define dbg_undef dwarf_undef
#define dbg_include dwarf_include
#define dbg_notepath dwarf_notepath
#define dbg_init dwarf_init
#define dbg_finalise dwarf_finalise
#define dbg_setformat dwarf_setformat
#define dbg_final_src_codeaddr dwarf_final_src_codeaddr
#define dbg_needsframepointer dwarf_needsframepointer

#define obj_notefpdesc dwarf_notefpdesc
#define dbg_debugareaexists dwarf_debugareaexists
#define dbg_writedebug dwarf_writedebug

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
#include "builtin.h"   /* te_xxx, xxxsegment, thissym etc */
#include "bind.h"
#include "simplify.h"  /* mcrep */
#include "dwarf.h"
#include "unmangle.h"  /* unmangle() */

#include "dw_int.h"

Symstr *dw_debug_sym, *dw_lineinfo_sym;
Symstr *dw_macro_sym, *dw_abbrev_sym, *dw_location_sym;

static int dw_version;
static uint32 dw_nameindex_size;
static Dw_ItemList *dw_section;

#ifndef TARGET_HAS_MULTIPLE_DEBUG_FORMATS

char dbg_name[] = "DWARF";
int usrdbgmask;

#endif

static void dw_sub_init(void);
static bool dw_init_done, dw_sub_init_done;

#define DbgAlloc(n) GlobAlloc(SU_Dbg, n)
#define DbgNew(type) ((type *)DbgAlloc(sizeof(type)))

Dw_ItemList *dw_list, *dw_listproc;
Dw_ItemList *dw_basetypes;

Dw_LocList *dw_loclist;
Dw_MacroList *dw_macrolist;

struct Dw_BaseSeg dw_baseseg;

typedef struct Dw_Scope Dw_Scope;
struct Dw_Scope {
    Dw_Scope *cdr;
    Dw_ItemList *item;
    Dw_ItemList **childlist;
    Dw_ItemList **childp;
};

static Dw_Scope *freescopes,
                *scopestack;

#define Dw_ItemAlloc(variant, tag) \
  Dw_ItemAlloc_S((size_t)(sizeof(dw_list->car.variant)+offsetof(Dw_ItemList,car)), tag)

static Dw_ItemList *Dw_ItemAlloc_S(size_t size, unsigned tag) {
  Dw_ItemList *p = (Dw_ItemList *)DbgAlloc(size);
  debsort_(p) = tag;
  dbgloc_(p) = 0;
  return p;
}

#define dw_additem(sort, tag) \
  dw_additem_s((size_t)(sizeof(dw_list->car.sort)+offsetof(Dw_ItemList,car)), tag)

static Dw_ItemList *dw_additem_s(size_t size, unsigned tag) {
  Dw_ItemList *p = (Dw_ItemList *)DbgAlloc(size);
  debsort_(p) = tag;
  sibling_(p) = NULL; *scopestack->childp = p; scopestack->childp = &sibling_(p);
  cdr_(p) = dw_list; dw_list = p;
  dbgloc_(p) = 0;
  return p;
}

static Dw_ItemList *dw_addtoitemlist(Dw_ItemList *p) {
  sibling_(p) = NULL; *scopestack->childp = p; scopestack->childp = &sibling_(p);
  cdr_(p) = dw_list; dw_list = p;
  return p;
}

static void PushScope(Dw_ItemList *item, Dw_ItemList **childp) {
  Dw_Scope *p = freescopes;
  if (p != NULL)
    freescopes = cdr_(freescopes);
  else
    p = DbgNew(Dw_Scope);
  cdr_(p) = scopestack; p->item = item; p->childp = childp; p->childlist = childp;
  *childp = NULL;
  scopestack = p;
}

static void PopScope(bool force) {
  Dw_Scope *p = scopestack;
  scopestack = cdr_(p);
  cdr_(p) = freescopes;
  freescopes = p;
  if (*p->childlist != NULL || force) {
  /* Normally, if the current scope has no child debug items, no end scope
   * item is generated here. This works happily for DWARF1, where the
   * criterion for an item to have children is that its sibling isn't the
   * next item. For DWARF2, it works only if there are two sets of abbreviation
   * entries for the parent item (one declaring there are children, one not).
   * For items which almost always have children, less output is produced if
   * there's only the 'has children' one, in which case an end of scope
   * item must always be produced.
   */
    Dw_ItemList *terminator = Dw_ItemAlloc(DEB_NULL, TAG_padding);
    cdr_(terminator) = dw_list; dw_list = terminator;
    sibling_(terminator) = NULL;
    null_parent_(terminator) = p->childlist;
    *p->childp = terminator;
  }
}

Dw_PathList *dw_pathlist;
static Uint dw_pathindex;
Dw_FileList *dw_filelist;
static Uint dw_fileindex;
/* The next two vars are (code order) list & tail pointer */
Dw_FileCoord *dw_coord_p, **dw_coord_q;
Dw_FileCoord dw_coord_sentinel =
  {   0,                   /* cdr                                      */
      0, 0,                /* file, nextinfile                         */
      0, 0xffff, 0         /* line, col, codeaddr(set by dbg_fileinfo).*/
  };

int32 dbg_tableindex(int32 dt_number)
{
  IGNORE(dt_number);
  return 0;
}

static Dw_FileList *Dw_FindFile(char const *name) {
  Dw_FileList *p = dw_filelist;
  for (; p != NULL; p = cdr_(p))
    if (p->filename == name) return p;

  p = DbgNew(Dw_FileList);
  cdr_(p) = dw_filelist; p->filename = name; p->linelist = 0;
  p->index = (name != NULL && StrEq(name, "<command line>")) ? 0 : ++dw_fileindex;
  p->dir = NULL; p->timestamp = 0; p->filelength = 0;
  dw_filelist = p;
  return p;
}

void *dbg_notefileline(FileLine fl) {
  Dw_FileList *x;
  if (!dw_sub_init_done) dw_sub_init();
  x = Dw_FindFile(fl.f);
  if (usrdbg(DBG_LINE)) {
    Dw_FileCoord *l = x->linelist;
    /* There used to be a syserr here if (l != NULL && l->line > fl.l),
       but it can be triggered by #line (though not #line n file, which
       won't give equality of filename): the CVS has an example. Also, it
       fails if functions are taken out of file order, as in the
       out-of-line expansion of inline functions.
     */
    l = DbgNew(Dw_FileCoord);
      cdr_(l) = NULL,
      l->nextinfile = x->linelist, x->linelist = l,
      l->file = x, l->line = fl.l, l->col = fl.column,
      l->codeaddr = -1;
      l->codeseg = dw_init_done ? bindsym_(codesegment) : 0;
    x->lastline = fl.l;
    return (void *)l;
  }
  return DUFF_ADDR;
}

static Dw_ItemList *dw_listscope;

/* The 'dbgaddr' arg has type 'void *' to keep the debugger types local to */
/* this file.  This does not make it any less of a (ANSI approved) hack.   */
void dbg_addcodep(void *dbgaddr, int32 codeaddr) {
  if (dbgaddr == NULL) { /* J_INFOSCOPE */
    /* c.flowgraf outputs a J_INFOSCOPE immediately after calling
     * dbg_scope, to mark the relevant code address.
     */
    if (debugging(DEBUG_Q)) cc_msg("-- scope at 0x%lx\n", codeaddr);
    { Dw_ItemList *p = dw_listscope, *next;
      for (; p != NULL; p = next) {
        next = startscope_next_(p);
        startscope_codeaddr_(p) = codeaddr;
      }
      dw_listscope = NULL;
    }
  } else if (usrdbg(DBG_LINE)) {
    Dw_FileCoord *p = (Dw_FileCoord *)dbgaddr;
    if (debugging(DEBUG_Q))
      cc_msg("%p ('%s' line %u/%u) @ %.6lx\n", p,
             p->file->filename, p->line, p->col, (long)codeaddr);
    /* The following test avoids setting nextincode/codeaddr twice */
    /* This is currently needed in case FileLine's are duplicated. */
    if (p->codeaddr == -1) {
      p->codeaddr = codeaddr;
      *dw_coord_q = p;
      dw_coord_q = &cdr_(p);
    }
  }
}

DataXref *dw_xrefs;

DataXref *Dw_Relocate(DataXref *xrefs, int32 where, Symstr const *symbol) {
  return (DataXref*)global_list3(SU_Xref, xrefs, where, symbol);
}

uint32 Dw_WriteInlineString(char const *s, uint32 offset) {
  uint32 n = strlen(s);
  obj_writedebug(s, n+1);
  return offset + n + 1;
}

uint32 Dw_WriteB(unsigned u, uint32 offset) {
  char b[1];
  b[0] = u; obj_writedebug(b, 1);
  return offset + 1;
}

uint32 Dw_WriteBN(uint8 const *p, int32 n, uint32 offset) {
  obj_writedebug(p, n);
  return offset + n;
}

uint32 Dw_WriteH(uint32 u, uint32 offset) {
  uint16 h[1];
  h[0] = (uint16)u; obj_writedebug(h, 1+DBG_SHORTFLAG);
  return offset + 2;
}

uint32 Dw_WriteW(uint32 u, uint32 offset) {
  obj_writedebug(&u, 1+DBG_INTFLAG);
  return offset + 4;
}

uint32 Dw_WriteL(uint32 const *d, uint32 offset) {
  obj_writedebug(d, 2+DBG_INTFLAG);
  return offset + 8;
}

uint32 Dw_WriteW_Relocated(uint32 w, Symstr const *sym, uint32 offset) {
  dw_xrefs = Dw_Relocate(dw_xrefs, offset, sym);
  return Dw_WriteW(w, offset);
}

/* End of file/line co-ordinate code */

static Dw_TypeRep *Dw_PrimType(int typecode) {
  Dw_TypeRep *p = dw_basetypes;
  for (; p != NULL; p = basetype_next_(p))
    if (basetype_typecode_(p) == typecode)
      return p;
  p = dw_additem(DEB_BASETYPE, DW_TAG_base_type);
  basetype_next_(p) = dw_basetypes; dw_basetypes = p;
  basetype_typecode_(p) = typecode;
  basetype_qual_(p) = NULL;
  return p;
}

static void dw_typerep(TypeExpr *, Dw_TypeRep **typep);

typedef struct Dw_fmlList  Dw_fmlList;
struct Dw_fmlList {
  Dw_fmlList *cdr;
  Binder *fb;
  Dw_ItemList *p;
};

static Dw_fmlList *dw_fmllist;
static FileLine dbg_invented_fl = {0, 0, 0};

static void dw_generate_default_fn(Expr *e, Dw_ItemList *fml) {
  Symstr *fname = sym_insert_id(symname_(gensymval(YES)));
  TypeExprFnAux s;
  Cmd *p = (Cmd *)GlobAlloc(SU_Other, (int32)offsetof(Cmd, cmd2));
  TypeExpr *ftype = g_mkTypeExprfn(t_fnap, typeofexpr(e), 0, 0,
                                   packTypeExprFnAux(s, 0, 0, 0, 0, 0));
  DeclRhsList *d = mkDeclRhsList(fname, ftype, bitofstg_(s_static)|b_fnconst);
  Binder *fbind = instate_declaration(d, TOPLEVEL);
  binduses_(fbind) |= u_referenced;
  p->fileline = dbg_invented_fl;
  h0_(p) = s_return;
  cmd1e_(p) = optimise0(mk_expr1(s_return, typeofexpr(e), e));
  syn_note_generated_fn(mkTopDeclFnDef(s_fndef, fbind, NULL,
        mk_cmd_block(dbg_invented_fl, 0, mkCmdList(0, p)), 0));
  { Dw_fmlList *l = DbgNew(Dw_fmlList);
    cdr_(l) = dw_fmllist, l->fb = fbind, l->p = fml, dw_fmllist = l;
  }
}

static void dw_formalparameterlistrep(TypeExpr *x) {
  FormTypeList *ft = typefnargs_(x);
  for (; ft != NULL; ft = ft->ftcdr) {
    Dw_ItemList *p = Dw_ItemAlloc(DEB_FORMAL, DW_TAG_proctype_formal);
    formal_invented_(p) = NO;
    formal_name_(p) = ft->ftname;
    if (LanguageIsCPlusPlus && ft->ftname != NULL && ft->ftname == thissym)
      formal_invented_(p) = YES;

    dw_typerep(ft->fttype, &formal_type_(p));
    dw_addtoitemlist(p);
    { Expr *e = ft->ftdefault;
      if (e != NULL)
        switch (h0_(e)) {
        default:
          dw_generate_default_fn(e, p);
          /* drop through */
        case s_integer:
        case s_floatcon:
        case s_string:
          formal_defltexpr_(p) = e;
          break;
        }
      else
        formal_defltexpr_(p) = NULL;
    }
  }
  if (fntypeisvariadic(x))
    dw_additem(DEB_REST, DW_TAG_unspecified_parameters);
}

/*
static bool dw_hasdefaultvalues(FormTypeList *ft) {
  for (; ft != NULL; ft = ft->ftcdr)
    if (ft->ftdefault != NULL)
      return YES;
  return NO;
}
*/

static Dw_TypeRep *dw_arrayrep(TypeExpr *t, Expr *e)
{ /* e is the array size. Since C arrays start at 0, the upper bound is */
  /* one less                                                           */
  Dw_ItemList *p = Dw_ItemAlloc(DEB_ARRAY, DW_TAG_array_type);
  if (e && h0_(e) == s_binder) e = NULL;
  array_open_(p) = e == NULL;
  dw_typerep(t, &array_basetype_(p));
  array_lowerbound_(p) = 0;
  array_upperbound_(p) = e ? evaluate(e)-1:0;
  array_size_(p) = sizeoftype(t);
  array_qual_(p) = NULL;
  dw_addtoitemlist(p);            /* do this last (typerep above) */
  PushScope(p, &array_children_(p));
  { Dw_ItemList *bound = dw_additem(DEB_ARRAYBOUND, DW_TAG_array_bound);
    arraybound_open_(bound) = array_open_(p);
    arraybound_upperbound_(bound) = array_upperbound_(p);
  }
  PopScope(NO);
  return p;
}

static void move_children(Dw_ItemList *p, Dw_ItemList *prevlist) {
  /* Move the child entries of p, currently at the head of dw_list (up to   */
  /* but not including prevlist) to immediately before p (after it when the */
  /* list is reversed)                                                      */
  Dw_ItemList *end = dw_list,
              *before;
  for (; cdr_(end) != prevlist; end = cdr_(end)) continue;
  for (before = prevlist; cdr_(before) != p; before = cdr_(before)) continue;
  cdr_(before) = dw_list; cdr_(end) = p;
  dw_list = prevlist;
}

/* bit like dw_additem(); makes move_children() redundant
   for structs. maybe enum too, but that's less important.
 */
static void move_to_top(Dw_ItemList *p)
{   Dw_ItemList *before = cdr_(p);
    for (; before && sibling_(before) != p; before = cdr_(before));
    if (before) sibling_(before) = sibling_(p);
    if (!LanguageIsCPlusPlus)
    {   /* how could this be possible??? */
        for (before = cdr_(p); before; before = cdr_(before))
            if ((debsort_(before) == DW_TAG_structure_type ||
                 debsort_(before) == DW_TAG_union_type ||
                 debsort_(before) == DW_TAG_class_type) &&
                struct_children_(before) == p)
            {   struct_children_(before) = sibling_(p);
                break;
            }
    }
    if (sect_children_(dw_section) == p)
        sect_children_(dw_section) = sibling_(p);
    cdr_(sibling_(p)) = cdr_(p);
    sibling_(p) = NULL;
    cdr_(p) = dw_list; dw_list = p;
    *scopestack->childp = p; scopestack->childp = &sibling_(p);
}

static void dw_typeinternal(Symstr *name, Dw_TypeRep *t, TypeExpr *type);

static Dw_ItemList *Dw_ProcDeclRep(Symstr *name, Binder *b, Dw_ItemList *parent, int ext) {
  Dw_ItemList *d = Dw_ItemAlloc(DEB_PROCDECL, DW_TAG_procdecl);
  TypeExpr *t = princtype(bindtype_(b));
  dw_typerep(typearg_(t), &procdecl_type_(d));
  procdecl_name_(d) = symname_(name);
  procdecl_variadic_(d) = fntypeisvariadic(t);
  procdecl_global_(d) = ext;
  procdecl_stg_(d) = bindstg_(b);
  procdecl_parent_(d) = parent;
  procdecl_voffset_(d) = (bindstg_(b) & bitofstg_(s_virtual)) ? bindxx_(b) : -1;
  dw_addtoitemlist(d); /* do this last (typerep above) */
  PushScope(d, &procdecl_children_(d));
  dw_formalparameterlistrep(t);
  PopScope(dw_version == 2);
  binddbg_(b) = (IPtr)d;
  return d;
}

static Dw_TypeRep *dw_structentry(Dw_TypeRep *p, TagBinder *b, TypeExpr *x) {
  SET_BITMAP sort = tagbindbits_(b) & CLASSBITS;
  int itemsort = sort == bitoftype_(s_struct) ? DW_TAG_structure_type :
                 sort == bitoftype_(s_union)  ? DW_TAG_union_type :
                                                DW_TAG_class_type;
#if 0
  Dw_ItemList *prev_dw_list = NULL;
#endif
  if (p == NULL) {
    p = dw_additem(DEB_STRUCT, itemsort);
    struct_size_(p) = 0;    /* filled in later */
    struct_name_(p) = isgensym(tagbindsym_(b)) ? NULL : symname_(tagbindsym_(b));
    struct_children_(p) = NULL;
    struct_qual_(p) = NULL;
    struct_friends_(p) = NULL;
    if (b != NULL) tagbinddbg_(b) = (IPtr)p;
    if (!(tagbindbits_(b) & TB_DEFD)) {
      debsort_(p) = DW_TAG_fref;
      struct_undefsort_(p) = itemsort;
      struct_size_(p) = 0;
      return p;
    }
  } else {
    debsort_(p) = itemsort;
    if (LanguageIsCPlusPlus) struct_friends_(p) = b->friends;
#if 0
    if (p != dw_list) prev_dw_list = dw_list;
#else
    if (p != dw_list) move_to_top(p);
#endif
  }
  struct_size_(p) = sizeoftype(x);

  { StructPos sp;
    Dw_ItemList **pp = &struct_children_(p);
    ClassMember *l;
    PushScope(p, pp);
    structpos_init(&sp, b);
    for (l = tagbindmems_(b); l != 0; l = memcdr_(l))
      if (memsv_(l) != NULL &&
            (structfield(l, sort, &sp) || LanguageIsCPlusPlus)) {

        if (LanguageIsCPlusPlus && attributes_(l) & (CB_BASE|CB_VBASE)) {
          Dw_ItemList *base = dw_additem(DEB_INHERIT, DW_TAG_inheritance);
          TagBinder *tb_base = typespectagbind_(princtype(memtype_(l)));
          inherit_type_(base) = (Dw_TypeRep *)tagbinddbg_(tb_base);
          inherit_offset_(base) = sp.woffset;
          inherit_virt_(base) = (attributes_(l) & CB_VBASE) != 0;
        } else if (h0_(memtype_(l)) == t_ovld || attributes_(l) & CB_ANON ||
                   h0_(l) == s_tagbind) {
          /* nothing */

        } else if (isfntype(memtype_(l))) {
          Dw_ItemList *d = Dw_ProcDeclRep(memsv_(l), realbinder_(l), p, 0);
          IGNORE(d);

        } else if (bindstg_(l) & bitofstg_(s_typedef)) {
          dw_typeinternal(bindsym_(l), 0, bindtype_(l));

        } else if (h0_(l) == s_member || bindstg_(l) & b_pseudonym) {
          Dw_ItemList *mem;
          Dw_TypeRep *type;
          if (sp.bsize == 0) {
            dw_typerep(memtype_(l), &type);
          } else {
            TypeExpr te; te = *memtype_(l);
            typespecmap_(&te) &= ~BITFIELD;
            dw_typerep(&te, &type);
          }
          /* note that memsv is 0 for padding bit fields */
          mem = dw_additem(DEB_MEMBER, DW_TAG_member);
          member_offset_(mem) = (bindstg_(l) & b_pseudonym) ? -1 : sp.woffset;
          member_name_(mem) = symname_(memsv_(l));
          member_boffset_(mem) = (uint8)sp.boffset;
          member_bsize_(mem) = (uint8)sp.bsize;
          member_type_(mem) = type;
        }
      }
    PopScope(NO);
  }
#if 0
  if (prev_dw_list != NULL && prev_dw_list != dw_list)
    move_children(p, prev_dw_list);
#endif
  return p;
}

static Dw_TypeRep *dw_enumentry(Dw_TypeRep *p, TagBinder *b) {
  static unsigned const c[] =
  {   FT_signed_char,   FT_signed_short,   FT_signed_integer,   FT_signed_integer,
      FT_unsigned_char, FT_unsigned_short, FT_unsigned_integer, FT_unsigned_integer
  };
  static char const s[] = { 1, 2, 4, 4, 1, 2, 4, 4};
  Dw_ItemList *prev_dw_list = NULL;
  if (p == NULL) {
    p = dw_additem(DEB_ENUM, DW_TAG_enumeration_type);
    enum_children_(p) = NULL;
    enum_name_(p) = isgensym(tagbindsym_(b)) ? NULL : symname_(tagbindsym_(b));
    enum_qual_(p) = NULL;
    if (b != NULL) tagbinddbg_(b) = (IPtr)p;
    if (!(tagbindbits_(b) & TB_DEFD)) {
      debsort_(p) = DW_TAG_fref;
      return p;
    }
  } else {
    debsort_(p) = DW_TAG_enumeration_type;
    prev_dw_list = dw_list;
  }

  { int32 container = (tagbindbits_(b) & TB_CONTAINER) >> TB_CONTAINER_SHIFT;
    enum_container_(p) = Dw_PrimType(c[container]);
    enum_size_(p) = s[container];
  }
  PushScope(p, &enum_children_(p));
  { BindList *members = tagbindenums_(b);
    for (; members != 0; members = members->bindlistcdr) {
      Dw_ItemList *mem = dw_additem(DEB_ENUMERATOR, DW_TAG_enumerator);
      Binder *elt = members->bindlistcar;
      enumerator_name_(mem) = symname_(bindsym_(elt));
      enumerator_val_(mem) = bindenumval_(elt);
    }
  }
  PopScope(NO);
  if (prev_dw_list != NULL) move_children(p, prev_dw_list);
  return p;
}

static Dw_TypeRep *struct_typerep(TypeExpr *x) {
  TagBinder *b = typespectagbind_(x);
  Dw_TypeRep *t = (Dw_TypeRep *)tagbinddbg_(b);
  if (t == NULL ||                 /* not yet seen */
      (debsort_(t) == DW_TAG_fref && (tagbindbits_(b) & TB_DEFD))
                           /* previously seen, (undefined), now defined */
      ) {
    if (tagbindbits_(b) & bitoftype_(s_enum))
      t = dw_enumentry(t, b);
    else
      t = dw_structentry(t, b, x);
  }
  return t;
}

static int typename_match(char *mangled, char *generic)
{ int l = strlen(generic);
  if (mangled == generic) return 1;
  if (StrnEq(mangled, generic, l) &&
      StrnEq(mangled+l, "__", 2)) return 1;
  return 0;
}

typedef struct Dw_QualType Dw_QualType;
struct Dw_QualType {
  Dw_QualType *cdr;
  int qual;
};
#define dw_mk_qualtype(t, p) ((Dw_QualType *)syn_list2(p, t))

static Dw_QualType *CVQualType(SET_BITMAP m, Dw_QualType *p) {
  if (m & bitoftype_(s_const)) p = dw_mk_qualtype(DW_TAG_const_type, p);
  if (m & bitoftype_(s_volatile)) p = dw_mk_qualtype(DW_TAG_volatile_type, p);
  return p;
}

typedef struct Dw_ftList Dw_ftList;
struct Dw_ftList {
  Dw_ftList *cdr;
  TypeExpr *t;
  Dw_ItemList *p;
};

static Dw_ftList *dw_ftlist;

static Dw_ItemList *find_ftlist(TypeExpr *te)
{ Dw_ftList *ft = dw_ftlist;
  for (; ft != NULL; ft = cdr_(ft))
    if (ft->t == te) return ft->p;
  return NULL;
}

static Dw_TypeRep *dw_fnrep(TypeExpr *x) {
  Dw_ItemList *t = find_ftlist(x);
  if (t != NULL) return t;
  t = Dw_ItemAlloc(DEB_PROCTYPE, DW_TAG_subroutine_type);
  dw_typerep(typearg_(x), &proctype_type_(t));
  proctype_qual_(t) = NULL;
  dw_addtoitemlist(t);
  PushScope(t, &proctype_children_(t));
  dw_formalparameterlistrep(x);
  PopScope(dw_version == 2);
  { Dw_ftList *p = DbgNew(Dw_ftList);
    cdr_(p) = dw_ftlist;
    p->t = x; p->p = t; dw_ftlist = p;
  }
  return t;
}

static Dw_TypeRep *dw_ptrtomemrep(TypeExpr *x)
{   Dw_ItemList *t = Dw_ItemAlloc(DEB_ARRAY, DW_TAG_ptr_to_member_type);
    ptrtomem_container_(t) = (Dw_TypeRep *)tagbinddbg_(typespectagbind_(x));
    dw_typerep(typearg_(x), &ptrtomem_type_(t));
    ptrtomem_qual_(t) = NULL;
    dw_addtoitemlist(t);
    return t;
}

static void dw_typerep(TypeExpr *x, Dw_TypeRep **typep)
{   /* note that we do NOT call prunetype() here so we still see typedefs */
  Dw_QualType *quals = NULL;
  Dw_TypeRep *restype = NULL;
  for (;;) {
    switch (h0_(x)) {
    case t_content:
      quals = CVQualType(typeptrmap_(x), quals);
      quals = dw_mk_qualtype(DW_TAG_pointer_type, quals);
      x = typearg_(x);
      continue;
    case t_ref:
      quals = CVQualType(typeptrmap_(x), quals);
      quals = dw_mk_qualtype(DW_TAG_reference_type, quals);
      x = typearg_(x);
      continue;
    case t_coloncolon:
      restype = dw_ptrtomemrep(x);
      break;
    case t_subscript:
      restype = dw_arrayrep(typearg_(x), typesubsize_(x));
      break;
    case t_fnap:
      restype = dw_fnrep(x);
      break;
    case s_typespec:
      { SET_BITMAP m = typespecmap_(x);
        quals = CVQualType(m, quals);
        switch (m & -m) {   /* LSB - unsigned/long etc. are higher */
        case bitoftype_(s_enum):
          restype = struct_typerep(x);
          break;
        case bitoftype_(s_struct):
        case bitoftype_(s_class):
        case bitoftype_(s_union):
          restype = struct_typerep(x);
          break;
        case bitoftype_(s_typedefname):
          { Binder *b = typespecbind_(x);
            /* is there already a table entry for it ? */
            { Dw_ItemList *l;
              for ( l = dw_list ; l != NULL ; l = cdr_(l))
                if ( debsort_(l)==DW_TAG_typedef &&
                     type_typex_(l)==bindtype_(b) &&
                     typename_match(type_name_(l), symname_(bindsym_(b)))) {
                  restype = l;
                  break;
                }
              if (l == NULL) syserr("typerep $b", b);
            }
            break;
          }
        case bitoftype_(s_char):
          { int32 mcr = mcrepoftype(x);
            restype = Dw_PrimType((mcr & MCR_SORT_MASK) == MCR_SORT_SIGNED ?
                                                    FT_signed_char : FT_unsigned_char);
            break;
          }
        case bitoftype_(s_int):
          if (m & BITFIELD) syserr(syserr_dbg_bitfield);
          { int32 mcr = mcrepoftype(x);
            int32 size = mcr & MCR_SIZE_MASK;
            int tc;
            if ((mcr & MCR_SORT_MASK) == MCR_SORT_SIGNED)
              tc = size == 2 ? FT_signed_short :
                   size == 8 ? FT_signed_long_long :
                               FT_signed_integer;
            else
              tc = size == 2 ? FT_unsigned_short :
                   size == 8 ? FT_unsigned_long_long :
                               FT_unsigned_integer;
            restype = Dw_PrimType(tc);
            break;
          }
        case bitoftype_(s_double):
          restype = Dw_PrimType((m & bitoftype_(s_short)) ? FT_float : FT_dbl_prec_float);
          break;
        case bitoftype_(s_bool):
          restype = Dw_PrimType(FT_boolean);
          break;
        case bitoftype_(s_void):
          restype = Dw_PrimType(FT_void);
          break;
        default:
          syserr(syserr_dbg_typerep, x, (long)m);
          restype = NULL;
          return;
        }
        break;
      }
        /* drop through for now */
    default:
      syserr(syserr_dbg_typerep, x, (long)typespecmap_(x));
      restype = NULL;
      return;
    }
    break;
  }
  if (quals != NULL) {
    Dw_TypeRep *basetype = restype;
    Dw_TypeRep *q, **qp;
    int n = 1;
    int maxn = (int)length((List *)quals);
    uint8 *qualmap = (uint8 *)SynAlloc(maxn);
    uint8 *qualp = &qualmap[maxn];

    switch (debsort_(restype)) {
    case DW_TAG_ptr_to_member_type: qp = &ptrtomem_qual_(restype); break;
    case DW_TAG_array_type:         qp = &array_qual_(restype); break;
    case DW_TAG_subroutine_type:    qp = &proctype_qual_(restype); break;
    case DW_TAG_enumeration_type:   qp = &enum_qual_(restype); break;

    case DW_TAG_fref:
    case DW_TAG_class_type:
    case DW_TAG_union_type:
    case DW_TAG_structure_type:     qp = &struct_qual_(restype); break;

    case DW_TAG_typedef:
    case DW_TAG_base_type:          qp = &type_qual_(restype); break;

    default:                        syserr("dw_typerep: %d", debsort_(restype));
                                    qp = NULL;
    }

    for (; quals != NULL; quals = cdr_(quals), n++) {
      *--qualp = quals->qual == DW_TAG_pointer_type ? MOD_pointer_to :
               quals->qual == DW_TAG_reference_type ? MOD_reference_to :
                   quals->qual == DW_TAG_const_type ? MOD_const :
                                                      MOD_volatile;
      for (; (q = *qp) != NULL; qp = &qualtype_next_(q))
        if (qualtype_n_(q) > n) {
          q = NULL;
          break;
        } else if (qualtype_n_(q) == n
                   && memcmp(qualtype_map_(q), qualp, n) == 0) {
          break;
        }
      if (q == NULL) {
        q = dw_additem(DEB_QUALTYPE, quals->qual);
        qualtype_type_(q) = restype;
        qualtype_n_(q) = n;
        qualtype_map_(q) = (uint8 *)DbgAlloc(n);
        memcpy(qualtype_map_(q), qualp, n);
        qualtype_next_(q) = *qp; *qp = q;
        qualtype_basetype_(q) = basetype;
        qualtype_qualifiedtype_(q) = restype;
      }
      restype = q;
    }
  }
  if (typep != NULL) *typep = restype;
}

static void dw_addvar(Symstr *name, Dw_TypeRep *t,
                      StgClass stgclass, SymOrReg base, int32 addr, TypeExpr *type)
{ unsigned tag = stgclass >= Stg_ArgReg ? DW_TAG_formal_parameter: DW_TAG_variable;
  Dw_ItemList *p = Dw_ItemAlloc(DEB_VAR, tag);
  var_type_(p) = t;
  var_stgclass_(p) = stgclass;
  var_loc_(p) = addr;
  var_sym_(p) = name;
  var_base_(p) = base;
  if (type != NULL) dw_typerep(type, &var_type_(p));
  dw_addtoitemlist(p);
}

void dbg_topvar(Symstr *name, int32 addr, TypeExpr *t, int stgclass,
                FileLine fl)
/* For scoping reasons this only gets called on top-level variables (which */
/* are known to be held in global store).  (Does this matter?)             */
{ if (usrdbg(DBG_PROC))
  { /* nb bss => external here.  The effect is only to cause the table item
       to be 0+symbol, rather than addr+data seg
     */
    Dw_ItemList *p;
    SymOrReg base;
    StgClass stg = (stgclass & DS_REG) ? Stg_Reg :
                   (stgclass & DS_EXT) ? Stg_Extern :
                                         Stg_Static;
    IGNORE(fl);
    base.sym = NULL;
    if (stgclass & (DS_EXT|DS_BSS))
      base.sym = name, addr = 0;
#ifdef CONST_DATA_IN_CODE
    else if (stgclass & DS_CODE)
      base.sym = bindsym_(constdatasegment);
#endif
    else if (!(stgclass & DS_REG))
      base.sym = bindsym_(datasegment);

    if (debugging(DEBUG_Q))
      cc_msg("top var $r @ %.6lx\n", name, (long)addr);
    if (stgclass != 0 && stg != Stg_Reg)
      for ( p = dw_list ; p != NULL ; p = cdr_(p))
        if ( debsort_(p) == DW_TAG_variable &&
             (var_stgclass_(p) == Stg_Extern ||
              var_stgclass_(p) == Stg_Static) &&
             var_loc_(p) == 0 &&
             var_sym_(p) == name) {
          var_loc_(p) = addr;
          var_base_(p) = base;
          return;
        }
    dw_addvar(name, 0, stg, base, addr, t);
  }
}

static void dw_typeinternal(Symstr *name, Dw_TypeRep *t, TypeExpr *type)
/* This procedure is called on a type-declaration internal to a procedure
 * (from dbg_scope, after the syntax tree has evaporated), and on a global
 * one, with the syntax tree in place.  The latter therefore goes through
 * dbg_type, which internalises the type.
 */
{ if (isgensym(name))
    dw_typerep(type, NULL);
  else {
    Dw_ItemList *p = Dw_ItemAlloc(DEB_TYPE, DW_TAG_typedef);
    if (debugging(DEBUG_Q))
        cc_msg("type $r\n", name);
    /* If there isn't already an internal representation ... */
    if (t == NULL && type != NULL)
        dw_typerep(type, &type_type_(p));
    else
        type_type_(p) = t;
    type_typex_(p) = type;
    type_name_(p) = symname_(name);
    type_qual_(p) = NULL;
    dw_addtoitemlist(p);
  }
}

void dbg_type(Symstr *name, TypeExpr *t, FileLine fl)
/* This only gets called on top-level types (which are known to be held in
 * global store).
 */
{ IGNORE(fl);
  dw_typeinternal(name, 0, t);
}

static Dw_FileCoord *cur_proc_coord;

void dbg_proc(Binder *b, TagBinder *parent, bool ext, FileLine fl)
{ if (usrdbg(DBG_PROC))
  { Symstr *name = bindsym_(b);
    TypeExpr *t = princtype(bindtype_(b));
    Dw_ItemList *p = Dw_ItemAlloc(DEB_PROC, DW_TAG_subprogram);
    if (debugging(DEBUG_Q)) cc_msg("startproc $r\n", name);
    if (h0_(t) != t_fnap) syserr(syserr_dbg_proc);
    /*
    if ((Dw_ItemList *)binddbg_(b) == NULL
        && dw_hasdefaultvalues(typefnargs_(t)))
      Dw_ProcDeclRep(name, b, NULL, ext);
      */

    dw_typerep(typearg_(t), &proc_type_(p));
    proc_entry_(p) = 0;           /* fill in at dbg_enterproc */
    proc_body_(p) = 0;            /* fill in at dbg_bodyproc  */
    proc_endproc_(p) = 0;         /* fill in at dbg_xendproc  */
    proc_name_(p) = symname_(name);
    proc_codeseg_(p) = bindsym_(codesegment);
    proc_global_(p) = ext;
    proc_variadic_(p) = fntypeisvariadic(t);
    proc_parent_(p) = NULL;
    if (parent != NULL)
      dw_typerep(tagbindtype_(parent), &proc_parent_(p));
    proc_decl_(p) = (Dw_ItemList *)binddbg_(b);
    dw_listproc = dw_addtoitemlist(p); /* do this last (typerep above) */
    PushScope(p, &proc_children_(p));
    if (dw_fmllist != NULL && dw_fmllist->fb == bind_global_(name)) {
      formal_defltfn_(dw_fmllist->p) = p;
      dw_fmllist = cdr_(dw_fmllist);
    }
  }
  if (usrdbg(DBG_LINE))
    cur_proc_coord = (Dw_FileCoord *)fl.p;
  dw_loclist = 0;
}

void dbg_enterproc(void)
{ if (usrdbg(DBG_PROC))
  { Dw_ItemList *p = dw_listproc;

    if (p == 0 || debsort_(p) != DW_TAG_subprogram || proc_entry_(p) != 0)
      syserr(syserr_dbg_proc1);
    if (debugging(DEBUG_Q))
      cc_msg("enter '%s' @ %.6lx\n", proc_name_(p), (long)codebase);
    proc_entry_(p) = codebase;
  }
  if (usrdbg(DBG_LINE))
    dbg_addcodep(cur_proc_coord, codebase);
}

/* The following routine records the post-entry codeaddr of a proc */
void dbg_bodyproc(void)
{ if (usrdbg(DBG_PROC))
  { Dw_ItemList *p = dw_listproc;
    if (p == 0 || debsort_(p) != DW_TAG_subprogram || proc_body_(p) != 0)
      syserr(syserr_dbg_proc1);
    if (debugging(DEBUG_Q))
      cc_msg("body '%s' @ %.6lx\n", proc_name_(p), (long)(codebase+codep));
    proc_body_(p) = codebase+codep;
  }
}

void dbg_return(int32 addr)
{ if (usrdbg(DBG_PROC))
  { if (debugging(DEBUG_Q))
        cc_msg("return @ %.6lx\n", addr);
    /* No way to represent this in DWARF */
  }
}

void dbg_xendproc(FileLine fl)
{ IGNORE(fl);
  if (bindsym_(codesegment) == dw_baseseg.sym) dw_baseseg.len = codebase+codep;
  if (usrdbg(DBG_PROC))
  { Dw_ItemList *q = dw_listproc;
    Dw_ItemList *p = Dw_ItemAlloc(DEB_ENDPROC, DW_TAG_endproc);
    if (q == 0 || debsort_(q) != DW_TAG_subprogram || proc_endproc_(q) != 0)
      syserr(syserr_dbg_proc1);
    /* ... for nested fns */
    for (dw_listproc = cdr_(dw_list); dw_listproc != NULL; dw_listproc = cdr_(dw_listproc))
      if (debsort_(dw_listproc) == DW_TAG_subprogram
          && proc_endproc_(dw_listproc) == 0
          && proc_body_(dw_listproc) == 0
          && proc_entry_(dw_listproc) == 0)
         break;
    if (debugging(DEBUG_Q))
      cc_msg("endproc '%s' @ %.6lx\n", proc_name_(q), (long)(codebase+codep));
    proc_endproc_(q) = p;
    endproc_endaddr_(p) = codebase+codep;
    cdr_(p) = dw_list; dw_list = p;
    dw_loclist = 0;
    PopScope(dw_version == 2);
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
{ if (usrdbg(DBG_VAR) /* && !isgensym(bindsym_(name))*/) {
    /* local to a proc */
    Dw_LocList *p = (Dw_LocList*) BindAlloc(sizeof(Dw_LocList));
    if (debugging(DEBUG_Q))
      cc_msg("note loc var $b\n", name);
    cdr_(p) = dw_loclist;
    p->name = name;
    p->pos = fl.l;
    p->size = sizeoftypelegal(bindtype_(name)) ? sizeoftype(bindtype_(name)) :
                                                sizeof_int;
    dw_typerep(bindtype_(name), &p->typeref);
    dw_loclist = p;
    if (bindstg_(name) & bitofstg_(s_typedef))
      dw_typeinternal(bindsym_(name), p->typeref, bindtype_(name));
  }
}

static Dw_LocList *dbg_findloclist(Binder *b)
{ Dw_LocList *p;
  for (p = dw_loclist; p != NULL; p = cdr_(p))
    if (p->name == b) return p;
  return NULL;
}

void dbg_locvar1(Binder *b) {
  Symstr *name = bindsym_(b);
  SymOrReg base;
  Dw_LocList *p = dbg_findloclist(b);
  StgClass stgclass;
  int stgclassname;
  int32 addr = bindaddr_(b);
  base.sym = NULL;
  if (p == NULL || p->pos == -1) {
    if (debugging(DEBUG_Q)) cc_msg(" omitted");
    return;   /* invented variable name (e.g. s_let) */
  }
  switch (bindstg_(b) & PRINCSTGBITS) {
  default:
  defolt:
    syserr(syserr_dbg_table, name, (long)bindstg_(b), (long)addr);
    return;
  case bitofstg_(s_typedef):
    if (debugging(DEBUG_Q)) cc_msg(" <typedef>");
    return;                   /* dbg_type deals with s_typedef vars  */
  case bitofstg_(s_extern):
    if (debugging(DEBUG_Q)) cc_msg(" <extern>");
    return;                   /* local externs do not allocate store */
  case bitofstg_(s_static):
    stgclass = Stg_Static, stgclassname = 'S';
    if (bindstg_(b) & b_fnconst) {
      base.sym = bindsym_(b);
      addr = 0;
    } else
      base.sym =
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
      stgclass = (addr & BINDADDR_MASK) == BINDADDR_ARG ? Stg_ArgReg : Stg_Reg;
      stgclassname = 'R', addr = register_number(bindxx_(b));
    } else switch (addr & BINDADDR_MASK) {
    case BINDADDR_ARG:
      stgclass = Stg_ArgAuto, stgclassname = 'A', addr = local_fpaddress(b);
      if (p->size < 4 && !target_lsbytefirst) addr += 4 - p->size;
      base.r = local_fpbase(b);
      break;
    case BINDADDR_LOC:
      stgclass = Stg_Auto, stgclassname = 'P', addr = local_fpaddress(b);
      if (p->size < 4 && !target_lsbytefirst) addr += 4 - p->size;
      base.r = local_fpbase(b);
      break;
    case 0:
      /* probably declared but not used case (where addr is still a bindlist) */
      if ((bindstg_(b) & b_bindaddrlist) != 0) {
        if (debugging(DEBUG_Q)) cc_msg(" unused - omitted");
        return;
      }
      /* otherwise, fall into internal error case */
    default:
      goto defolt;
    }
    break;
  }
  if (debugging(DEBUG_Q)) cc_msg(" %c %#lx", stgclassname, (long)addr);
  dw_addvar(name, p->typeref, stgclass, base, addr, NULL);
}

static void dwarf_scope_2(int entering, BindListList *newbll, BindListList *oldbll)
{ if (oldbll != NULL) {
    BindListList *bll = newbll;
    Dw_ItemList *last = NULL;
    for (bll = newbll; bll != oldbll; bll = bll->bllcdr) {
      if (bll == NULL) syserr(syserr_dbg_scope);
      if (bll->bllcar != NULL) {
        Dw_ItemList *p;
        if (entering > 0) {
          p = dw_additem(DEB_STARTSCOPE, DW_TAG_lexical_block);
          startscope_next_(p) = last; /* filled in soon by INFOSCOPE */
          startscope_codeseg_(p) = bindsym_(codesegment);
          PushScope(p, &startscope_children_(p));
        } else {
          p = Dw_ItemAlloc(DEB_ENDSCOPE, DW_TAG_end_lexical_block);
          startscope_next_(p) = last; /* filled in soon by INFOSCOPE */
          startscope_end_(scopestack->item) = p;
          PopScope(NO);
          cdr_(p) = dw_list; dw_list = p;
        }
        last = p;
        dw_listscope = p;
      }
    }
  }
  if (debugging(DEBUG_Q)) cc_msg("scope %ld\n", entering);
  for (; newbll != oldbll; newbll = newbll->bllcdr)
  { SynBindList *bl;
    if (newbll == NULL) syserr(syserr_dbg_scope);
    for (bl = newbll->bllcar; bl; bl = bl->bindlistcdr)
    { Binder *b = bl->bindlistcar;
      if (bindstg_(b) & b_dbgbit) continue; /* for this and next line */
      bindstg_(b) |= b_dbgbit;              /* see end of routine cmt */
      if (debugging(DEBUG_Q))
        cc_msg("  %s $b",
                entering>=0 ? "binding" : "unbinding",
                b);
      if (entering > 0)
        dbg_locvar1(b);
      if (debugging(DEBUG_Q))
        cc_msg("\n");
    }
  }
}

static bool dwarf_scope_1(BindListList *newbll, BindListList *oldbll) {
  int32 entering = length((List *)newbll) - length((List *)oldbll);
  if (entering == 0) return NO;
  /* A bodge here to ensure that there's no START/ENDSCOPE pair around  */
  /* the variable items for the outermost BindList (arguments)          */
  /* XRAY for one is unhappy if there is.                               */
  /* (dwarf_scope_i copes with the case where there's just one BindList */
  /* being added or removed : code is needed here for the case there's  */
  /* more than one because of removal of an empty block with a          */
  /* different debenv from its predecessor                              */
  if (entering > 0) {
    if (oldbll == NULL && entering > 1) {
      BindListList *p = newbll;
      do
        p = p->bllcdr;
      while (p->bllcdr != NULL);
      dwarf_scope_2(1, p, oldbll);
      oldbll = p; --entering;
    }
    dwarf_scope_2(entering, newbll, oldbll);
  } else {
    if (newbll == NULL && entering < -1) {
      BindListList *p = oldbll;
      do
        p = p->bllcdr;
      while (p->bllcdr != NULL);
      dwarf_scope_2(1, p, newbll);
      newbll = p; ++entering;
    }
    dwarf_scope_2(entering, oldbll, newbll);
  }
  /* Ask for INFOSCOPE item to get called back more or less immediately */
  /* from the local cg (INFOSCOPE item) to fill in the codeaddr         */
  return YES;
}

bool dbg_scope(BindListList *newbll, BindListList *oldbll) {
  int32 entering = length((List *)newbll) - length((List *)oldbll);
  if (oldbll == newbll)
    return NO;
  if (newbll == NULL || oldbll == NULL)
    return dwarf_scope_1(newbll, oldbll);

  if (entering > 0) {
    BindListList *bll;
    for (bll = newbll; bll != 0; bll = bll->bllcdr)
      if (bll == oldbll)
        return dwarf_scope_1(newbll, oldbll);
  } else if (entering < 0) {
    BindListList *bll;
    for (bll = oldbll; bll != 0; bll = bll->bllcdr)
      if (bll == newbll)
        return dwarf_scope_1(newbll, oldbll);
  }
  /* Neither list is a subset of the other (can happen thanks to dead   */
  /* block elimination). Find the common tail.                          */
  { BindListList *bll_n, *bll_o;
    for (bll_n = newbll->bllcdr; bll_n != NULL; bll_n = bll_n->bllcdr)
      for (bll_o = oldbll->bllcdr; bll_o != NULL; bll_o = bll_o->bllcdr)
        if (bll_o == bll_n) {
          dwarf_scope_1(bll_o, oldbll);
          return dwarf_scope_1(newbll, bll_o);
        }
  }
  /* There is no common tail to oldbll and newbll                       */
  syserr(syserr_dbg_scope);
  return NO;
}

/* Dummy procedure not yet properly implemented, included here to keep in */
/* step with dbx.c */
void dbg_commblock(Binder *b, SynBindList *members, FileLine fl) {
  IGNORE(b); IGNORE(members); IGNORE(fl);
}

static Dw_MacroList *Dw_NewMacroList(FileLine const *fl, int sort) {
  Dw_MacroList *p = DbgNew(Dw_MacroList);
  Dw_FileList *fp = Dw_FindFile(fl->f);
  p->fl.index = fp->index; p->fl.l = fl->l; p->fl.column = fl->column;
  p->sort = sort;
  cdr_(p) = dw_macrolist;
  dw_macrolist = p;
  return p;
}

void dbg_define(char const *name, bool objectmacro, char const *body,
                dbg_ArgList const *args, FileLine fl) {
  if (dw_version == 2) {
    Dw_MacroList *p = Dw_NewMacroList(&fl, DW_MACINFO_define);
    size_t namelen = strlen(name);
    size_t len = namelen;
    if (!objectmacro) {
      dbg_ArgList const *p;
      for (p = args; p != NULL; p = p->next)
        len += strlen(p->name) + 1;
      len += args == NULL ? 2 : 1;
    }
    len += strlen(body) + 2;
    { char *value = (char *)DbgAlloc(len);
      p->data.s = value;
      memcpy(value, name, namelen); value += namelen;
      if (!objectmacro) {
        dbg_ArgList const *p;
        *value++ = '(';
        for (p = args; p != NULL; p = p->next) {
          namelen = strlen(p->name);
          if (p != args) *value++ = ',';
          memcpy(value, p->name, namelen);
          value += namelen;
        }
        *value++ = ')';
      }
      *value++ = ' ';
      strcpy(value, body);
    }
    if (debugging(DEBUG_Q))
      cc_msg("%s:%d #define %s\n", fl.f, fl.l, p->data.s);
  }
}

void dbg_undef(char const *name, FileLine fl) {
  if (dw_version == 2) {
    Dw_MacroList *p = Dw_NewMacroList(&fl, DW_MACINFO_undef);
    p->data.s = name;
    if (debugging(DEBUG_Q))
      cc_msg("%s:%d #undef %s\n", fl.f, fl.l, name);
  }
}

static Dw_PathList *FindPath(char const *pathname) {
  size_t len = strlen(pathname);
  Dw_PathList *p = dw_pathlist;
  for (; p != NULL; p = cdr_(p))
    if (len == p->len && memcmp(p->name, pathname, len) == 0)
      return p;

  p = (Dw_PathList *)DbgAlloc(sizeof(Dw_PathList)+len);
  cdr_(p) = dw_pathlist; dw_pathlist = p;
  p->index = ++dw_pathindex;
  p->len = len;
  memcpy(p->name, pathname, len+1);
  return p;
}

void dbg_include(char const *filename, char const *path, FileLine fl) {
  if (dw_version == 2) {
    if (filename == NULL) {
      Dw_MacroList *p = Dw_NewMacroList(&fl, DW_MACINFO_end_file);
      p->data.i = 0;
      if (debugging(DEBUG_Q))
        cc_msg("%s:%d #end include\n", fl.f, fl.l);
    } else {
      Dw_MacroList *p = Dw_NewMacroList(&fl, DW_MACINFO_start_file);
      Dw_FileList *fp = Dw_FindFile(filename);
      p->data.i = fp->index;
      fp->dir = path == NULL || path[0] == 0 ? NULL : FindPath(path);
      if (debugging(DEBUG_Q)) {
        cc_msg("%s:%d #include %s", fl.f, fl.l, filename);
        if (path != NULL) cc_msg(" %s", path);
        cc_msg("\n");
      }
    }
  }
}

void dbg_notepath(char const *pathname) {
  if (dw_version == 2) {
    if (!dw_sub_init_done) dw_sub_init();
    { Uint ix = dw_pathindex;
      if (FindPath(pathname)->index > ix &&
          debugging(DEBUG_Q))
        cc_msg("note path %s\n", pathname);
    }
  }
}

int32 dw_mapped_codebase, dw_mapped_codep;

void dbg_final_src_codeaddr(int32 code_base, int32 code_p)
{   dw_mapped_codebase = code_base,
    dw_mapped_codep = code_p;
}

char const *Dw_Unmangle(char const *s)
{
static char unmangle_buf[256];
static char const sstring[] = "static";
  size_t len = strlen(s), slen = strlen(sstring);
  if (!StrnEq(s, "__ct__F", 7) &&
      !StrnEq(s, "__dt__F", 7) &&
      !StrnEq(s, "__as__F", 7) &&
      (len < 4 || !StrEq(s+len-3, "__C")))
  { const char *name = unmangle2(s, unmangle_buf, sizeof unmangle_buf);
    if (name != s)
    { char *t = strchr(unmangle_buf, '(');
      if (t != NULL && t < &unmangle_buf[sizeof unmangle_buf])
          *t = '\0';
      return (StrnEq(unmangle_buf, sstring, slen)) ? &unmangle_buf[slen+1] :
          unmangle_buf;
    }
  }
  return s;
}

static uint32 Dw_1or2_NameindexSize(void) {
  Dw_ItemList const *p = sect_children_(dw_section);
  uint32 n = 0;
  for (; p != NULL; p = sibling_(p))
    switch (debsort_(p)) {
    case DW_TAG_subprogram:
      n += (uint32)strlen(Dw_Unmangle(proc_name_(p))) + 5;
      break;

    case DW_TAG_variable:
      n += (uint32)strlen(Dw_Unmangle(symname_(var_sym_(p)))) + 5;
      break;

    case DW_TAG_typedef:
      n += (uint32)strlen(type_name_(p)) + 5;
      break;

    case DW_TAG_fref:
    case DW_TAG_class_type:
    case DW_TAG_union_type:
    case DW_TAG_structure_type:
      if (struct_name_(p) != NULL)
        n += (uint32)strlen(struct_name_(p)) + 5;
      break;

    case DW_TAG_enumeration_type:
      if (enum_name_(p) != NULL)
        n += (uint32)strlen(enum_name_(p)) + 5;
      { Dw_ItemList *elts = enum_children_(p);
        for (; elts != NULL; elts = sibling_(elts))
          if (debsort_(elts) == DW_TAG_enumerator)
            n += (uint32)strlen(enumerator_name_(elts)) + 5;
      }
      break;

    default:
      break;
    }
  return n;
}

static void Dw_1or2_WriteNameindexEntry(Dw_ItemList const *p, char const *s) {
  Dw_WriteW(dbgloc_(p), 0);
  Dw_WriteInlineString(s, 0);
}

static void Dw_1or2_WriteNameindex(void) {
  Dw_ItemList const *p = sect_children_(dw_section);
  DataXref *xrefs = NULL;
  uint32 size = dw_nameindex_size + 9 + /* header (excluding length word) */
                                    4;  /* terminator */
  uint32 roundup = (size & 3) ? 4 - (size & 3) : 0;
  obj_startdebugarea(NameIndexAreaName);
  Dw_WriteW(size + roundup, 0);
  Dw_WriteB(1, 0);
  Dw_WriteW(0, 0);
  Dw_WriteW(dw_baseseg.len, 0);
  xrefs = Dw_Relocate(xrefs, 5, dw_debug_sym);
  for (; p != NULL; p = sibling_(p))
    switch (debsort_(p)) {
    case DW_TAG_subprogram:
      Dw_1or2_WriteNameindexEntry(p, Dw_Unmangle(proc_name_(p)));
      break;

    case DW_TAG_variable:
      Dw_1or2_WriteNameindexEntry(p, Dw_Unmangle(symname_(var_sym_(p))));
      break;

    case DW_TAG_typedef:
      Dw_1or2_WriteNameindexEntry(p, type_name_(p));
      break;

    case DW_TAG_fref:
    case DW_TAG_class_type:
    case DW_TAG_union_type:
    case DW_TAG_structure_type:
      if (struct_name_(p) != NULL)
        Dw_1or2_WriteNameindexEntry(p, struct_name_(p));
      break;

    case DW_TAG_enumeration_type:
      if (enum_name_(p) != NULL)
        Dw_1or2_WriteNameindexEntry(p, enum_name_(p));
      { Dw_ItemList *elts = enum_children_(p);
        for (; elts != NULL; elts = sibling_(elts))
          if (debsort_(elts) == DW_TAG_enumerator)
            Dw_1or2_WriteNameindexEntry(p, enumerator_name_(elts));
      }
      break;

    default:
      break;
    }
  Dw_WriteW(0, 0);
  if (roundup) {
    uint32 w = 0;
    obj_writedebug(&w, roundup);
  }
  obj_enddebugarea(NameIndexAreaName, xrefs);
}

#ifdef TARGET_HAS_FP_OFFSET_TABLES

void obj_notefpdesc(ProcFPDesc const *fpd) {
  /* Only representable in DWARF version 2 */
  IGNORE(fpd);
  return;
}

#endif

void dbg_finalise(void) {
  dw_init_done = dw_sub_init_done = NO;
}

bool dbg_debugareaexists(char const *name) {
  if (StrEq(name, DebugAreaName))
    return dw_list != NULL;
  else if (StrEq(name, LineInfoAreaName))
    return dw_coord_p != NULL;
  else if (StrEq(name, NameIndexAreaName))
    return (dw_nameindex_size = Dw_1or2_NameindexSize()) != 0;
  else if (StrEq(name, RangeIndexAreaName))
    return NO;
  else if (StrEq(name, MacroAreaName))
    return dw_macrolist != NULL;
  else if (StrEq(name, AbbrevAreaName))
    return dw_list != NULL;
  else if (StrEq(name, LocationAreaName))
    return NO;
  return NO;
}

void dbg_writedebug(void) {
  Dw_ItemList *p;
  if (dw_list != NULL) {
    PopScope(NO);
    dw_list = (Dw_ItemList *)dreverse((List *)dw_list);
  }
  dw_macrolist = (Dw_MacroList *)dreverse((List *)dw_macrolist);

  for (p = dw_list; p != NULL; p = cdr_(p))
    if (debsort_(p) == DW_TAG_subprogram && proc_variadic_(p)) {
      Dw_ItemList *q,
              **prevp = &cdr_(p),
              **prevsibling = &proc_children_(p);
      for (q = proc_children_(p); q != NULL; q = sibling_(q))
        if (debsort_(q) == DW_TAG_formal_parameter) {
          prevsibling = &sibling_(q);
          prevp = &cdr_(q);
        }

      q = (Dw_ItemList *)DbgAlloc(sizeof(dw_list->car.DEB_REST)+offsetof(Dw_ItemList,car));
      debsort_(q) = DW_TAG_unspecified_parameters;
      sibling_(q) = *prevsibling; cdr_(q) = *prevp;
      *prevsibling = q; *prevp = q;
    }

  if (dw_version == 1) {
    if (dw_list != NULL) Dw_1_WriteInfo();
    if (dw_coord_p != NULL) Dw_1_WriteLineinfo();
    if (dw_nameindex_size != 0) Dw_1or2_WriteNameindex();
  } else {
    if (dw_list != NULL) Dw_2_WriteInfo();
    if (dw_coord_p != NULL) Dw_2_WriteLineinfo();
    if (dw_nameindex_size != 0) Dw_1or2_WriteNameindex();
    if (dw_macrolist != NULL) Dw_2_WriteMacros();
    if (dw_list != NULL) Dw_2_WriteAbbrevs();
  }
  if (usrdbg(DBG_PROC) && dw_fmllist != NULL) syserr("dw_fmllist non empty!");
}

static void dw_sub_init(void) {
  dw_list = NULL; dw_basetypes = NULL;
  dw_baseseg.len = 0;
  dw_listproc = NULL;
  dw_listscope = NULL;
  dw_filelist = NULL; dw_fileindex = 0;
  dw_pathlist = NULL; dw_pathindex = 0;
  dw_coord_p = NULL; dw_coord_q = &dw_coord_p;
  dw_loclist = NULL;
  dw_sub_init_done = YES;
  dw_nameindex_size =0;
  dw_ftlist = NULL;
  dw_fmllist = NULL;
  dw_macrolist = NULL;
}

bool dbg_needsframepointer(void) {
    return dw_version == 1;
}

void dbg_setformat(char const *format) {
  int form = format[0];
  if (form == 0)
    form = 1;  /* -dwarf on its own defaults to dwarf version 1 */
  else
    form = form - '0';
  dw_version = form;
}

void dbg_init(void) {
  if (!dw_sub_init_done) dw_sub_init();
  dw_baseseg.sym = bindsym_(codesegment);
  if (usrdbg(DBG_ANY))
  { Dw_ItemList *p = Dw_ItemAlloc(DEB_SECTION, DW_TAG_compile_unit);
    dw_section = p;
    dw_debug_sym = obj_notedebugarea(DebugAreaName);
    dw_lineinfo_sym = obj_notedebugarea(LineInfoAreaName);
    obj_notedebugarea(NameIndexAreaName);
    obj_notedebugarea(RangeIndexAreaName);
    if (dw_version == 2) {
      dw_macro_sym = obj_notedebugarea(MacroAreaName);
      dw_abbrev_sym = obj_notedebugarea(AbbrevAreaName);
      dw_location_sym = obj_notedebugarea(LocationAreaName);
    }
    sect_name_(p) = sourcefile;
    sect_codeseg_(p) = bindsym_(codesegment);
    cdr_(p) = NULL;
    freescopes = NULL;
    scopestack = NULL; PushScope(p, &sect_children_(p));
    { Dw_ItemList *q, **pp = &dw_list;
      for (; (q = *pp) != NULL; pp = &cdr_(q)) continue;
      *pp = p;
    }
    if (usrdbg(DBG_LINE))
    { Dw_FileList *x = dw_filelist;
      for (; x != NULL; x = cdr_(x)) {
        Dw_FileCoord *l = x->linelist;
        for (; l != NULL; l = l->nextinfile)
          l->codeseg = bindsym_(codesegment);
      }
    }
  }
  dw_init_done = YES;
}

#else

typedef int dummy; /* prevent translation unit from being empty */

#endif /* defined(TARGET_HAS_DWARF) && defined(TARGET_HAS_DEBUGGER) */

/* End of mip/dwarf.c */
