/*
 * C compiler file mip/dwarf1.c
 * Copyright:   (C) 1995, Advanced RISC Machines Limited. All rights reserved.
 * Writer for DWARF version 1 debug tables.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <string.h>

#include "globals.h"

#if defined(TARGET_HAS_DEBUGGER) && defined(TARGET_HAS_DWARF)

#include "errors.h"
#include "aeops.h"
#include "cgdefs.h"
#include "version.h"
#include "xrefs.h"
#include "codebuf.h"
#include "builtin.h"   /* te_xxx, xxxsegment */
#include "simplify.h"  /* mcrep */

#include "mcdep.h"
#include "dwarf.h"
#include "bind.h"  /* for isgensym() */
#include "dw_int.h"

typedef union {
  uint32 u;
  char const *s;
  struct {
    uint32 n;
    Symstr *sym;
  } ref;
  uint32 const *d;
} AttribArg;

static int32 dw_1_lineinfo_size(void) {
  int32 n;
  Dw_FileCoord *p = dw_coord_p, *prev = NULL;
  if (p == NULL)
    return 0;
  for (n = 18; p != NULL; prev = p, p = cdr_(p))
    if (prev == NULL
        || !(p->line == prev->line
             && p->col == prev->col
             && p->codeaddr == prev->codeaddr))
      n += 10;
  return n;
}

void Dw_1_WriteLineinfo(void) {
  Dw_FileCoord *p = dw_coord_p, *prev = NULL;
  DataXref *xrefs = NULL;
  int32 size = dw_1_lineinfo_size();
  int32 roundup = (size & 2) ? 2 : 0;
  int32 sectionbase = p->codeaddr;
  dw_coord_sentinel.codeseg = bindsym_(codesegment);
  *dw_coord_q = &dw_coord_sentinel;
  dw_coord_sentinel.codeaddr = dw_mapped_codebase+dw_mapped_codep;
  obj_startdebugarea(Dwarf1LineInfoAreaName);
  Dw_WriteW(size + roundup, 0);
  Dw_WriteW(sectionbase, 4);
  xrefs = Dw_Relocate(xrefs, 4, p->codeseg);
  for (; p != NULL; prev = p, p = cdr_(p))
  /* Include the sentinel (to mark the end of info, and to hold
     the address of end of code-segment)
     Note that, despite the DWARF spec talking about deltas for
     the code addresses, it appears to mean from the section start,
     not from the previous statement.
   */
    if (prev == NULL
        || !(p->line == prev->line
             && p->col == prev->col
             && p->codeaddr == prev->codeaddr)) {
      Dw_WriteW(p->line, 0);
      Dw_WriteH(p->col, 0);
      Dw_WriteW(p->codeaddr-sectionbase, 0);
    }
  /* round up to 4-byte multiple */
  if (roundup) Dw_WriteH(0, 0);
  obj_enddebugarea(Dwarf1LineInfoAreaName, xrefs);
}

/* armobj.c calls writedebug to generate the debugging tables.   */
/* It must format them and then call obj_writedebug()            */
static unsigned32 dw_1_writeattribute(int attr, unsigned32 offset, AttribArg arg) {
  offset = Dw_WriteH(attr, offset);
  switch (attr & 15) {
  case FORM_ADDR:
  case FORM_REF:
    return Dw_WriteW_Relocated(arg.ref.n, arg.ref.sym, offset);

  case FORM_BLOCK2:
  case FORM_DATA2:
    return Dw_WriteH(arg.ref.n, offset);

  case FORM_DATA4:
    if (arg.ref.sym != NULL) dw_xrefs = Dw_Relocate(dw_xrefs, offset, arg.ref.sym);
    return Dw_WriteW(arg.ref.n, offset);

  case FORM_BLOCK4:
    return Dw_WriteW(arg.ref.n, offset);

  case FORM_DATA8:
    return Dw_WriteL(arg.d, offset);

  case FORM_STRING:
    return Dw_WriteInlineString(arg.s, offset);

  default:
    syserr("dw_1_writeattribute(%x)", attr);
    return offset;
  }
}

static unsigned32 dw_1_writeattrib_u(int attr, unsigned32 offset, unsigned32 u) {
  AttribArg arg;
  arg.ref.n = u;
  arg.ref.sym = ((attr & 15) == FORM_REF) ? dw_debug_sym : NULL;
  return dw_1_writeattribute(attr, offset, arg);
}

static unsigned32 dw_1_writeattrib_l(int attr, unsigned32 offset, uint32 const *d) {
  AttribArg arg;
  arg.d = d;
  return dw_1_writeattribute(attr, offset, arg);
}

static unsigned32 dw_1_writeattrib_str(int attr, unsigned32 offset, char const *s) {
  AttribArg arg;
  arg.s = s;
  return dw_1_writeattribute(attr, offset, arg);
}

static unsigned32 dw_1_writeattrib_ref(int attr, unsigned32 offset, unsigned32 n, Symstr *sym) {
  AttribArg arg;
  arg.ref.n = n; arg.ref.sym = sym;
  return dw_1_writeattribute(attr, offset, arg);
}

static unsigned32 dw_1_attrib_size(int attr) {
  switch (attr & 15) {
  case FORM_ADDR:
  case FORM_REF:
  case FORM_DATA4:
  case FORM_BLOCK4: return 6;

  case FORM_BLOCK2:
  case FORM_DATA2:  return 4;

  case FORM_DATA8:  return 10;

  default:          syserr("dw_1_attrib_size(%x)", attr);
                    return 0;
  }
}

static unsigned32 dw_1_attrib_str_size(char const *s) {
  return (uint32)strlen(s) + 2 + 1;
}

static unsigned32 dw_1_hdr_size(void) {
  return 6 + dw_1_attrib_size(AT_sibling);
}

static unsigned32 dw_1_attrib_friend_size(Friend *amigos)
{ int32 n = 0;
  for (; amigos != NULL; amigos = amigos->friendcdr)
    if (h0_(amigos->u.friendfn) == s_binder)
      n += (bindstg_(amigos->u.friendfn) & b_undef) ? 0 : 1;
    else
      n++;
  return 4 * n + dw_1_attrib_size(AT_friends);
}

static unsigned32 dw_1_typeref_size(Dw_ItemList *typep) {
/* For DWARF version 1, qualified types do not have information items, nor do
   base types (the qualifiers and distinction between base and user types are
   encoded in the reference). It's uncertain whether pointer and reference
   types should be separate types or not: there are items for them, but also
   there are modifier values. For now, we make them items.
  */
  switch (debsort_(typep)) {
  case DW_TAG_base_type:
      return dw_1_attrib_size(AT_fund_type);

  case DW_TAG_fref:
  case DW_TAG_structure_type:
  case DW_TAG_union_type:
  case DW_TAG_class_type:
  case DW_TAG_enumeration_type:
  case DW_TAG_subroutine_type:
  case DW_TAG_array_type:
  case DW_TAG_typedef:
    return dw_1_attrib_size(AT_user_def_type);

  case DW_TAG_reference_type:
  case DW_TAG_pointer_type:
  case DW_TAG_const_type:
  case DW_TAG_volatile_type:
    { unsigned32 n = qualtype_n_(typep);
      Dw_ItemList *basetype = qualtype_basetype_(typep);
      if (debsort_(basetype) == DW_TAG_base_type) {
        if (basetype_typecode_(basetype) == FT_void && n == 1 &&
            qualtype_map_(typep)[0] == MOD_pointer_to)
          /* special representation for void * */
          return dw_1_attrib_size(AT_fund_type);
        else
          return dw_1_attrib_size(AT_mod_fund_type) + n + 2; /* fundamental type code */
      } else
        return dw_1_attrib_size(AT_mod_u_d_type) + n + 4;  /* type ref */
    }

  default:
    syserr("dw_1_typeref_size %d", debsort_(typep));
    return 0;
  }
}

#define MAXEXPRSTRINGSIZE       256
static uint32 Dw_Expr2CPntr(Expr *e, char *buf)
{   StringSegList *p = ((String *)e)->strseg;
    uint32 n = 0;
    for (; p != NULL && (n+p->strseglen) < MAXEXPRSTRINGSIZE;
                n += p->strseglen, p = p->strsegcdr)
        memcpy(&buf[n], p->strsegbase, (size_t)p->strseglen);
    buf[n] = 0;
    return n;
}

#define at_default_value_code   0x01e
#define AT_default_value_addr   AT_ADDR(at_default_value_code)
#define AT_default_value_short  AT_DATA2(at_default_value_code)
#define AT_default_value_int    AT_DATA4(at_default_value_code)
#define AT_default_value_dble   AT_DATA8(at_default_value_code)
#define AT_default_value_string AT_STRING(at_default_value_code)

static unsigned32 Dw_1_InfoItemSize(Dw_ItemList *p) {
/* must be kept in step with WriteInfo below */
  unsigned32 n;
  switch (debsort_(p)) {
  case DW_TAG_compile_unit:
    n = dw_1_hdr_size() +
        dw_1_attrib_size(AT_language) +
        dw_1_attrib_str_size(sect_name_(p));
    if (dw_baseseg.len > 0) {
      n += dw_1_attrib_size(AT_low_pc) +
           dw_1_attrib_size(AT_high_pc);
    }
    if (dw_1_lineinfo_size() != 0) {
      n += dw_1_attrib_size(AT_stmt_list);
    }
    return n + dw_1_attrib_str_size(version_banner());

  case DW_TAG_subprogram:
    n = dw_1_hdr_size() +
        dw_1_attrib_str_size(Dw_Unmangle(proc_name_(p)));
    { Dw_ItemList *restype = proc_type_(p);
      if (!is_void_type_(restype))
        n += dw_1_typeref_size(restype);
    }
    if (proc_parent_(p) != NULL)
        n += dw_1_attrib_size(AT_member);
    return n + ((proc_endproc_(p) != NULL)
               ? dw_1_attrib_size(AT_low_pc) + dw_1_attrib_size(AT_high_pc)
                 + dw_1_attrib_size(AT_proc_body)
               : 0);

  case DW_TAG_procdecl:
    n = dw_1_hdr_size() +
        dw_1_attrib_str_size(Dw_Unmangle(procdecl_name_(p)));
    { Dw_ItemList *restype = procdecl_type_(p);
      if (!is_void_type_(restype))
        n += dw_1_typeref_size(restype);
    }
    if (procdecl_parent_(p) != NULL)
      n += dw_1_attrib_size(AT_member);
    if (procdecl_stg_(p) & bitofstg_(s_virtual)) {
      n += dw_1_attrib_size(AT_vtable_offset);
      n += dw_1_attrib_str_size("");
    }
    return n;

  case DW_TAG_endproc:
    return 0;

  case DW_TAG_proctype_formal:
    n = dw_1_hdr_size() +
        dw_1_typeref_size(formal_type_(p));
    if (formal_name_(p)!= NULL)
      n += dw_1_attrib_str_size(symname_(formal_name_(p)));
    if (formal_defltexpr_(p) != NULL)
    { Expr *e = formal_defltexpr_(p);
      int32 mcsize = mcrepofexpr(e) & MCR_SIZE_MASK;
      switch (h0_(e)) {
      case s_integer:
          n += dw_1_attrib_size((mcsize < 4) ? AT_default_value_short :
                        AT_default_value_int);
          break;
      case s_floatcon:
        n += dw_1_attrib_size((mcsize < 8) ? AT_default_value_int :
                        AT_default_value_dble);
        break;
      case s_string:
        { char buf[MAXEXPRSTRINGSIZE];
          n += Dw_Expr2CPntr(e, buf) + 2 + 1;
          break;
        }
      default:
        n += dw_1_attrib_size(AT_default_value_addr);
      }
    }
    return n;

  case DW_TAG_subroutine_type:
    n = dw_1_hdr_size();
    { Dw_ItemList *restype = proctype_type_(p);
      if (!is_void_type_(restype))
        n += dw_1_typeref_size(restype);
    }
    return n;

  case DW_TAG_variable:
  case DW_TAG_formal_parameter:
    n = dw_1_hdr_size() +
        dw_1_typeref_size(var_type_(p)) +
        dw_1_attrib_size(AT_location);
    if (!isgensym(var_sym_(p)))
      n += dw_1_attrib_str_size(Dw_Unmangle(symname_(var_sym_(p))));
    switch (var_stgclass_(p)) {
    case Stg_Extern:
    case Stg_Static:  return n + 5;
    case Stg_Reg:
    case Stg_ArgReg:  return n + 5;
    case Stg_Auto:
    case Stg_ArgAuto: if (var_base_(p).r == R_NOFPREG) return 0;
                      return n + 11;
    default:          syserr("Dw_1_InfoItemSize: stg %p = %d", p, var_stgclass_(p));
    }

  case DW_TAG_unspecified_parameters:
    return dw_1_hdr_size();

  case DW_TAG_typedef:
    return dw_1_hdr_size() +
           dw_1_attrib_str_size(type_name_(p)) +
           dw_1_typeref_size(type_type_(p));

  case DW_TAG_fref:
  case DW_TAG_class_type:
  case DW_TAG_union_type:
  case DW_TAG_structure_type:
    n = dw_1_hdr_size();
    if (struct_name_(p) != NULL)
        n += dw_1_attrib_str_size(struct_name_(p));
    if (struct_size_(p) != 0)  n += dw_1_attrib_size(AT_byte_size);
    if (struct_friends_(p) != NULL)
        n += dw_1_attrib_friend_size(struct_friends_(p));
    return n;

  case DW_TAG_volatile_type:
  case DW_TAG_const_type:
  case DW_TAG_pointer_type:
  case DW_TAG_reference_type:
    return 0;

  case DW_TAG_ptr_to_member_type:
    n = dw_1_hdr_size() +
        dw_1_typeref_size(ptrtomem_container_(p)) +
        dw_1_typeref_size(ptrtomem_type_(p));
    return n;

  case DW_TAG_member:
    n = dw_1_hdr_size() +
        dw_1_attrib_str_size(member_name_(p)) +
        dw_1_typeref_size(member_type_(p));

    if (member_offset_(p) != -1)
      n += dw_1_attrib_size(AT_location) +
          6  /* op_const(n) op_add */;
    if (member_bsize_(p) != 0) {
      n += dw_1_attrib_size(AT_bit_size) +
           dw_1_attrib_size(AT_bit_offset);
    }
    return n;

  case DW_TAG_inheritance:
    n = dw_1_hdr_size() +
        dw_1_typeref_size(inherit_type_(p)) +
        dw_1_attrib_size(AT_location) +
        6;
    if (inherit_virt_(p))
      n += dw_1_attrib_str_size("");
    return n;

  case DW_TAG_enumeration_type:
    n = dw_1_hdr_size();
    if (enum_name_(p) != NULL)
      n += dw_1_attrib_str_size(enum_name_(p));
    n += dw_1_attrib_size(AT_byte_size);
    { Dw_ItemList *elts = enum_children_(p);
      n += dw_1_attrib_size(AT_element_list);
      for (; elts != NULL; elts = sibling_(elts))
        if (debsort_(elts) == DW_TAG_enumerator) {
          n += 4 + (uint32)strlen(enumerator_name_(elts)) + 1;
        }
    }
    return n;

  case DW_TAG_enumerator:
    return 0;   /* DWARF version 2 only */

  case TAG_padding: /* null entry to terminate sibling chains */
    { Dw_ItemList *l = *null_parent_(p);
      for (; l != p; l = sibling_(l))
        if (dbgloc_(l) != 0) return 4;
      debsort_(p) = DW_TAG_ignore;
      return 0;
    }

  case DW_TAG_array_type:
    n = dw_1_hdr_size() +
        dw_1_attrib_size(AT_subscr_data);
    n += array_open_(p) ? 10 : 12;
    return n + dw_1_typeref_size(array_basetype_(p));

  case DW_TAG_lexical_block:
    return dw_1_hdr_size() +
           dw_1_attrib_size(AT_low_pc) +
           dw_1_attrib_size(AT_high_pc);

  case DW_TAG_ignore:
  case DW_TAG_end_lexical_block:
  case DW_TAG_base_type:
  case DW_TAG_array_bound:
    return 0;

  default:
    syserr("Dw_1_InfoItemSize %d", debsort_(p));
    return 0;
  }
}

static unsigned32 dw_this_itemsize;

static unsigned32 dw_1_hdr(Dw_ItemList *p, int itemsort, unsigned32 offset) {
  dw_this_itemsize = Dw_1_InfoItemSize(p);
  offset = Dw_WriteW(dw_this_itemsize, offset);
  offset = Dw_WriteH(itemsort, offset);
  { Dw_ItemList *sib = sibling_(p);
    for (; sibling_(sib) != 0; sib = sibling_(sib))
      if (dbgloc_(sib) != 0) break;
    return dw_1_writeattrib_u(AT_sibling, offset, dbgloc_(sib));
  }
}

static unsigned32 dw_1_writetyperef(Dw_ItemList *typep, unsigned32 offset) {
/* For DWARF version 1, qualified types do not have information items, nor do
   base types (the qualifiers and distinction between base and user types are
   encoded in the reference). It's uncertain whether pointer and reference
   types should be separate types or not: there are items for them, but also
   there are modifier values. For now, we use modifiers.
  */
  switch (debsort_(typep)) {
  case DW_TAG_base_type:
    return dw_1_writeattrib_u(AT_fund_type, offset, basetype_typecode_(typep));

  case DW_TAG_structure_type:
  case DW_TAG_union_type:
  case DW_TAG_class_type:
  case DW_TAG_enumeration_type:
  case DW_TAG_subroutine_type:
  case DW_TAG_array_type:
  case DW_TAG_typedef:
    return dw_1_writeattrib_u(AT_user_def_type, offset, dbgloc_(typep));

  case DW_TAG_reference_type:
  case DW_TAG_pointer_type:
  case DW_TAG_const_type:
  case DW_TAG_volatile_type:
    { int n = qualtype_n_(typep);
      Dw_TypeRep *basetype = qualtype_basetype_(typep);
      if (debsort_(basetype) == DW_TAG_base_type) {
        if (basetype_typecode_(basetype) == FT_void && n == 1 &&
            qualtype_map_(typep)[0] == MOD_pointer_to)
          /* special representation for void * */
          return dw_1_writeattrib_u(AT_fund_type, offset, FT_pointer);
        else {
          offset = dw_1_writeattrib_u(AT_mod_fund_type, offset, n+2L);
          offset = Dw_WriteBN(qualtype_map_(typep), n, offset);
          return Dw_WriteH(basetype_typecode_(basetype), offset);
        }
      } else {
        offset = dw_1_writeattrib_u(AT_mod_u_d_type, offset, n+4L);
        offset = Dw_WriteBN(qualtype_map_(typep), n, offset);
        return Dw_WriteW_Relocated(dbgloc_(basetype), dw_debug_sym, offset);
      }
    }

  default:
    syserr("dw_1_writetype %d", debsort_(typep));
    return 0;
  }
}

static unsigned32 dw_1_write_friends(unsigned32 offset, Friend *amigos)
{ Dw_ItemList *p;
  offset = Dw_WriteH(AT_friends, offset);
  offset = Dw_WriteH(dw_1_attrib_friend_size(amigos)-dw_1_attrib_size(AT_friends),
                        offset);
  for (; amigos != NULL ; amigos = amigos->friendcdr)
  { if (h0_(amigos->u.friendclass) == s_tagbind) {
      dw_xrefs = Dw_Relocate(dw_xrefs, offset, dw_debug_sym);
      offset = Dw_WriteW(dbgloc_((Dw_TypeRep *)((TagBinder *)amigos->u.friendclass)->tagbinddbg), offset);
    } else
      if (!(bindstg_(amigos->u.friendfn) & b_undef))
        for (p = dw_list; p != NULL; p = cdr_(p))
          if (debsort_(p) == DW_TAG_subprogram &&
              strncmp(proc_name_(p), symname_(bindsym_(amigos->u.friendfn)),
                       strlen(proc_name_(p))) == 0)
          { dw_xrefs = Dw_Relocate(dw_xrefs, offset, dw_debug_sym);
            offset = Dw_WriteW(dbgloc_(p), offset);
            break;
          }
  }
  return offset;
}

void Dw_1_WriteInfo(void)
{ Dw_ItemList *p;
  unsigned32 infosize, offset, roundup;
  dw_xrefs = NULL;
  obj_startdebugarea(Dwarf1DebugAreaName);
  for (infosize = 0, p = dw_list; p != NULL; p = cdr_(p)) {
    uint32 n;
    if (debsort_(p) == DW_TAG_fref) debsort_(p) = struct_undefsort_(p);
    n = Dw_1_InfoItemSize(p);
    dbgloc_(p) = n == 0 ? 0 : infosize;
    infosize += n;
  }

  if (infosize & 3)
    roundup = 8 - (infosize & 3);
  else
    roundup = 0;

  for (offset = 0, p = dw_list; p != NULL; p = cdr_(p)) {
    int sort = debsort_(p);
    unsigned32 start = offset;
    switch (sort) {
    default:
      syserr(syserr_dbg_write, (long)sort);
      break;

    case DW_TAG_compile_unit:
      { Dw_ItemList dummy;
        dummy.dbgloc = infosize + roundup;
        sibling_(p) = &dummy;
        offset = dw_1_hdr(p, TAG_compile_unit, offset);
      }
      offset = dw_1_writeattrib_u(AT_language, offset,
                (LanguageIsCPlusPlus) ? LANG_C_PLUS_PLUS : LANG_C89);
      offset = dw_1_writeattrib_str(AT_name, offset, sect_name_(p));
      if (dw_baseseg.len > 0) {
        offset = dw_1_writeattrib_ref(AT_low_pc, offset, 0, sect_codeseg_(p));
        offset = dw_1_writeattrib_ref(AT_high_pc, offset, dw_baseseg.len, sect_codeseg_(p));
      }
      if (dw_1_lineinfo_size() != 0) {
        offset = dw_1_writeattrib_ref(AT_stmt_list, offset, 0, dw_lineinfo_sym);
      }
      offset = dw_1_writeattrib_str(AT_producer, offset, version_banner());
      break;

    case DW_TAG_subprogram:
      offset = dw_1_hdr(p, proc_global_(p) ? TAG_global_subroutine : TAG_subroutine, offset);
      offset = dw_1_writeattrib_str(AT_name, offset, Dw_Unmangle(proc_name_(p)));
      { Dw_ItemList *restype = proc_type_(p);
        if (!is_void_type_(restype))
          offset = dw_1_writetyperef(restype, offset);
      }
      if (proc_parent_(p) != NULL)
        offset = dw_1_writeattrib_ref(AT_member, offset,
                dbgloc_(proc_parent_(p)), dw_debug_sym);
      if (proc_endproc_(p) != NULL)
      {
        offset = dw_1_writeattrib_ref(AT_low_pc, offset, proc_entry_(p), proc_codeseg_(p));
        offset = dw_1_writeattrib_ref(AT_proc_body, offset, proc_body_(p), proc_codeseg_(p));
        offset = dw_1_writeattrib_ref(AT_high_pc, offset, high_pc_(p), proc_codeseg_(p));
      }
      break;

    case DW_TAG_procdecl:
      offset = dw_1_hdr(p, procdecl_global_(p) ? TAG_global_subroutine : TAG_subroutine, offset);
      offset = dw_1_writeattrib_str(AT_name, offset, Dw_Unmangle(procdecl_name_(p)));
      { Dw_ItemList *restype = procdecl_type_(p);
        if (!is_void_type_(restype))
          offset = dw_1_writetyperef(restype, offset);
      }
      if (procdecl_parent_(p) != NULL)
        offset = dw_1_writeattrib_ref(AT_member, offset,
                dbgloc_(procdecl_parent_(p)), dw_debug_sym);
      if (procdecl_stg_(p) & bitofstg_(s_virtual)) {
        offset = dw_1_writeattrib_u(AT_vtable_offset, offset, procdecl_voffset_(p));
        offset = dw_1_writeattrib_str(AT_virtual, offset, "");
      }
      break;

    case DW_TAG_endproc:
      break;

    case DW_TAG_unspecified_parameters:
      offset = dw_1_hdr(p, TAG_unspecified_parameters, offset);
      break;

    case DW_TAG_proctype_formal:
      offset = dw_1_hdr(p, TAG_formal_parameter, offset);
      if (formal_name_(p) != NULL)
        offset = dw_1_writeattrib_str(AT_name, offset, symname_(formal_name_(p)));
      offset = dw_1_writetyperef(formal_type_(p), offset);
      { Expr *e = formal_defltexpr_(p);
        if (e != NULL)
          switch (h0_(e)) {
          case s_integer:
            offset = dw_1_writeattrib_u(((mcrepofexpr(e) & MCR_SIZE_MASK) < 4) ?
                AT_default_value_short : AT_default_value_int, offset, intval_(e));
            break;
          case s_string:
            { char buf[MAXEXPRSTRINGSIZE];
              (void)Dw_Expr2CPntr(e, buf);
              offset = dw_1_writeattrib_str(AT_default_value_string, offset, buf);
              break;
            }
          case s_floatcon:
            if (mcrepofexpr(e) & MCR_SIZE_MASK < 8)
              offset = dw_1_writeattrib_u(AT_default_value_int, offset,
                        ((FloatCon *)e)->floatbin.fb.val);
            else
              offset = dw_1_writeattrib_l(AT_default_value_dble, offset,
                        (uint32 *)((FloatCon *)e)->floatbin.irep);
            break;
          default:
            offset = dw_1_writeattrib_ref(AT_default_value_addr, offset,
                        dbgloc_(formal_defltfn_(p)), dw_debug_sym);
          }
      }
      break;

    case DW_TAG_subroutine_type:
      offset = dw_1_hdr(p, TAG_subroutine_type, offset);
      { Dw_ItemList *restype = proctype_type_(p);
        if (!is_void_type_(restype))
          offset = dw_1_writetyperef(restype, offset);
      }
      break;

    case DW_TAG_ptr_to_member_type:
      offset = dw_1_hdr(p, TAG_ptr_to_member_type, offset);
      offset = dw_1_writetyperef(ptrtomem_container_(p), offset);
      offset = dw_1_writetyperef(ptrtomem_type_(p), offset);
      break;

    case DW_TAG_variable:
      sort = var_stgclass_(p) == Stg_Extern ? TAG_global_variable : TAG_local_variable;
    case DW_TAG_formal_parameter:
      if ((var_stgclass_(p) == Stg_Auto || var_stgclass_(p) == Stg_ArgAuto)
          && var_base_(p).r == R_NOFPREG)
        break;
      offset = dw_1_hdr(p, sort, offset);
      if (!isgensym(var_sym_(p)))
        offset = dw_1_writeattrib_str(AT_name, offset,
                                      Dw_Unmangle(symname_(var_sym_(p))));
      offset = dw_1_writetyperef(var_type_(p), offset);
      offset = Dw_WriteH(AT_location, offset);
      switch (var_stgclass_(p)) {
      case Stg_Extern:
      case Stg_Static:
        offset = Dw_WriteH(5, offset);
        offset = Dw_WriteB(OP_ADDR, offset);
        { Symstr *s = var_base_(p).sym;
          obj_symref(s, symext_(s) == NULL ? xr_data|xr_weak : xr_data, 0);
          offset = Dw_WriteW_Relocated(var_loc_(p), s, offset);
        }
        break;

      case Stg_Reg:
      case Stg_ArgReg:
        offset = Dw_WriteH(5, offset);
        offset = Dw_WriteB(OP_REG, offset);
        offset = Dw_WriteW(var_loc_(p), offset);
        break;

      case Stg_Auto:
      case Stg_ArgAuto:
        offset = Dw_WriteH(11, offset);
        offset = Dw_WriteB(OP_BASEREG, offset);
        offset = Dw_WriteW(var_base_(p).r, offset);
        offset = Dw_WriteB(OP_CONST, offset);
        offset = Dw_WriteW(var_loc_(p), offset);
        offset = Dw_WriteB(OP_ADD, offset);
        break;
      }
      break;

    case DW_TAG_typedef:
      offset = dw_1_hdr(p, TAG_typedef, offset);
      offset = dw_1_writeattrib_str(AT_name, offset, type_name_(p));
      offset = dw_1_writetyperef(type_type_(p), offset);
      break;

    case DW_TAG_class_type:
    case DW_TAG_union_type:
    case DW_TAG_structure_type:
      offset = dw_1_hdr(p, debsort_(p), offset);
      if (struct_name_(p) != NULL)
        offset = dw_1_writeattrib_str(AT_name, offset, struct_name_(p));
      if (struct_size_(p) != 0)
        offset = dw_1_writeattrib_u(AT_byte_size, offset, struct_size_(p));
      if (struct_friends_(p) != NULL)
        offset = dw_1_write_friends(offset, struct_friends_(p));
      break;

    case DW_TAG_volatile_type:
    case DW_TAG_const_type:
    case DW_TAG_pointer_type:
    case DW_TAG_reference_type:
      break;

    case DW_TAG_member:
      offset = dw_1_hdr(p, TAG_member, offset);
      offset = dw_1_writeattrib_str(AT_name, offset, member_name_(p));
      offset = dw_1_writetyperef(member_type_(p), offset);
      if (member_bsize_(p) != 0) {
        offset = dw_1_writeattrib_u(AT_bit_size, offset, member_bsize_(p));
        offset = dw_1_writeattrib_u(AT_bit_offset, offset, member_boffset_(p));
      }
      if (member_offset_(p) != -1) {
        offset = dw_1_writeattrib_u(AT_location, offset, 6);
        offset = Dw_WriteB(OP_CONST, offset);
        offset = Dw_WriteW(member_offset_(p), offset);
        offset = Dw_WriteB(OP_ADD, offset);
      }
      break;

    case DW_TAG_inheritance:
      offset = dw_1_hdr(p, TAG_inheritance, offset);
      offset = dw_1_writetyperef(inherit_type_(p), offset);
      offset = dw_1_writeattrib_u(AT_location, offset, 6);
      offset = Dw_WriteB(OP_CONST, offset);
      offset = Dw_WriteW(inherit_offset_(p), offset);
      offset = Dw_WriteB(OP_ADD, offset);
      if (inherit_virt_(p))
        offset = dw_1_writeattrib_str(AT_virtual, offset, "");
      break;

    case DW_TAG_enumeration_type:
      offset = dw_1_hdr(p, TAG_enumeration_type, offset);
      if (enum_name_(p) != NULL)
        offset = dw_1_writeattrib_str(AT_name, offset, enum_name_(p));
      offset = dw_1_writeattrib_u(AT_byte_size, offset, enum_size_(p));
      { Dw_ItemList *elts = enum_children_(p);
        Dw_ItemList *prev = NULL, *next;
        /* DWARF version 1 requires the enumeration members in reverse order.
           Pre-reverse them. (And reverse back on writing)
         */
        for (; elts != NULL; prev = elts, elts = next) {
          next = sibling_(elts);
          sibling_(elts) = prev;
        }
        elts = prev;
        offset = dw_1_writeattrib_u(AT_element_list, offset,
                                    dw_this_itemsize - (offset + 6 - start));
        prev = NULL;
        for (; elts != NULL; prev = elts, elts = next) {
          if (debsort_(elts) == DW_TAG_enumerator) {
          /* This just ignores the sibling list terminator */
            offset = Dw_WriteW(enumerator_val_(elts), offset);
            offset = Dw_WriteInlineString(enumerator_name_(elts), offset);
          }
          next = sibling_(elts);
          sibling_(elts) = prev;
        }
      }
      break;

    case DW_TAG_enumerator:
      break;   /* DWARF version 2 only */

    case TAG_padding: /* null entry to terminate sibling chains */
      offset = Dw_WriteW(4, offset);
      break;

    case DW_TAG_array_type:
      offset = dw_1_hdr(p, TAG_array_type, offset);
      offset = dw_1_writeattrib_u(AT_subscr_data, offset,
                                  dw_1_typeref_size(array_basetype_(p)) + (array_open_(p) ? 10 : 12));
      offset = Dw_WriteB(array_open_(p) ? FMT_FT_C_X : FMT_FT_C_C, offset);
      offset = Dw_WriteH(FT_signed_integer, offset);
      offset = Dw_WriteW(array_lowerbound_(p), offset);
      if (array_open_(p))
        offset = Dw_WriteH(0, offset);
      else
        offset = Dw_WriteW(array_upperbound_(p), offset);
      offset = Dw_WriteB(FMT_ET, offset);
      offset = dw_1_writetyperef(array_basetype_(p), offset);
      break;

    case DW_TAG_lexical_block:
      offset = dw_1_hdr(p, TAG_lexical_block, offset);
      offset = dw_1_writeattrib_ref(AT_low_pc, offset, startscope_codeaddr_(p), startscope_codeseg_(p));
      offset = dw_1_writeattrib_ref(AT_high_pc, offset, endscope_codeaddr_(startscope_end_(p))+4, startscope_codeseg_(p)); /* +4 is bodge for XRAY */
      break;

    case DW_TAG_ignore:
    case DW_TAG_end_lexical_block:
    case DW_TAG_base_type:
    case DW_TAG_array_bound:
      break;
    }
  }
  if (roundup != 0) {
    unsigned32 w = 0;
    Dw_WriteW(roundup, offset);
    obj_writedebug(&w, roundup - 4);
  }
  obj_enddebugarea(Dwarf1DebugAreaName, dw_xrefs);
}

#else

typedef int dummy; /* prevent translation unit from being empty */

#endif /* defined(TARGET_HAS_DWARF) && defined(TARGET_HAS_DEBUGGER) */

/* end of mip/dwarf1.c */
