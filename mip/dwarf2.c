/*
 * C compiler file mip/dwarf1.c
 * Copyright:   (C) 1995, Advanced RISC Machines Limited. All rights reserved.
 * Writer for DWARF version 2 debug tables.
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
#include "dw_int.h"

typedef struct {
  Uint attrib;
  Uint form;
} AttribDesc;

typedef struct {
  Uint tag;
  bool children;
  AttribDesc const *attribs;
} AbbrevEntry;

#define DW_FORM_tref 0xff
#define DW_FORM_string_expr 0xfe

static AttribDesc const attr_null[] = {
  { 0, 0 }
};

static AbbrevEntry const abbr_null = { 0, NO, attr_null };

static AttribDesc const attr_arraybound[] = {
  { DW_AT_upper_bound, DW_FORM_udata },
  { 0, 0 }
};

static AbbrevEntry const abbr_arraybound = { DW_TAG_subrange_type, NO, attr_arraybound };
static AbbrevEntry const abbr_arraybound_open = { DW_TAG_subrange_type, NO, attr_null };

static AttribDesc const attr_arraytype[] = {
  { DW_AT_type, DW_FORM_ref_udata },
  { 0, 0 }
};

static AbbrevEntry const abbr_arraytype = { DW_TAG_array_type, YES, attr_arraytype };

static AttribDesc const attr_basetype[] = {
  { DW_AT_byte_size, DW_FORM_data1 },
  { DW_AT_encoding, DW_FORM_data1 },
  { 0, 0 }
};

static AbbrevEntry const abbr_basetype = { DW_TAG_base_type, NO, attr_basetype };

static AttribDesc const attr_classtype[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_byte_size, DW_FORM_udata },
  { 0, 0 }
};

static AttribDesc const attr_classtype_nodata[] = {
  { DW_AT_name, DW_FORM_string },
  { 0, 0 }
};

static AttribDesc const attr_classtype_anon[] = {
  { DW_AT_byte_size, DW_FORM_udata },
  { 0, 0 }
};

static AbbrevEntry const abbr_classtype = { DW_TAG_class_type, YES, attr_classtype };
static AbbrevEntry const abbr_classtype_nodata = { DW_TAG_class_type, YES, attr_classtype_nodata };
static AbbrevEntry const abbr_classtype_anon = { DW_TAG_class_type, YES, attr_classtype_anon };
static AbbrevEntry const abbr_classtype_nodata_anon = { DW_TAG_class_type, YES, attr_null };
static AbbrevEntry const abbr_classtype_fref = { DW_TAG_class_type, NO, attr_classtype_nodata };

static AttribDesc const attr_compileunit[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_producer, DW_FORM_string },
  { DW_AT_language, DW_FORM_data1 },
  { DW_AT_low_pc, DW_FORM_addr },
  { DW_AT_high_pc, DW_FORM_addr },
  { DW_AT_stmt_list, DW_FORM_tref },
  { 0, 0 }
};

static AttribDesc const attr_compileunit_nostmt[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_producer, DW_FORM_string },
  { DW_AT_language, DW_FORM_data1},
  { DW_AT_low_pc, DW_FORM_addr },
  { DW_AT_high_pc, DW_FORM_addr },
  { 0, 0 }
};

static AttribDesc const attr_compileunit_nocode[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_producer, DW_FORM_string },
  { DW_AT_language, DW_FORM_data1 },
  { 0, 0 }
};

static AbbrevEntry const abbr_compileunit = { DW_TAG_compile_unit, YES, attr_compileunit };
static AbbrevEntry const abbr_compileunit_nostmt = { DW_TAG_compile_unit, YES, attr_compileunit_nostmt };
static AbbrevEntry const abbr_compileunit_nocode = { DW_TAG_compile_unit, YES, attr_compileunit_nocode };

static AttribDesc const attr_qualtype[] = {
  { DW_AT_type, DW_FORM_ref_udata },
  { 0, 0 }
};

static AbbrevEntry const abbr_consttype = { DW_TAG_const_type, NO, attr_qualtype };

static AttribDesc const attr_enum[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_byte_size, DW_FORM_data1 },
  { 0, 0 }
};

static AttribDesc const attr_enum_anon[] = {
  { DW_AT_byte_size, DW_FORM_data1 },
  { 0, 0 }
};

static AbbrevEntry const abbr_enum = { DW_TAG_enumeration_type, YES, attr_enum };
static AbbrevEntry const abbr_enum_anon = { DW_TAG_enumeration_type, YES, attr_enum_anon };

static AttribDesc const attr_enumerator[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_const_value, DW_FORM_udata },
  { 0, 0 }
};

static AttribDesc const attr_enumerator_signed[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_const_value, DW_FORM_sdata },
  { 0, 0 }
};

static AbbrevEntry const abbr_enumerator = { DW_TAG_enumerator, NO, attr_enumerator };
static AbbrevEntry const abbr_enumerator_signed = { DW_TAG_enumerator, NO, attr_enumerator_signed };

static AttribDesc const attr_friend[] = {
  { DW_AT_friend, DW_FORM_ref_udata },
  { 0, 0 }
};

static AbbrevEntry const abbr_friend = { DW_TAG_friend, NO, attr_friend };

static AttribDesc const attr_inheritance[] = {
  { DW_AT_type, DW_FORM_ref_udata },
  { DW_AT_data_member_location, DW_FORM_block },
  { DW_AT_virtuality, DW_FORM_data1 },
  { 0, 0 }
};

static AbbrevEntry const abbr_inheritance = { DW_TAG_inheritance, NO, attr_inheritance };

static AttribDesc const attr_lexicalblock[] = {
  { DW_AT_low_pc, DW_FORM_addr },
  { DW_AT_high_pc, DW_FORM_addr },
  { 0, 0 }
};

static AbbrevEntry const abbr_lexicalblock = { DW_TAG_lexical_block, YES, attr_lexicalblock };
static AbbrevEntry const abbr_emptylexicalblock = { DW_TAG_lexical_block, NO, attr_lexicalblock };

static AttribDesc const attr_member[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_type, DW_FORM_ref_udata },
  { DW_AT_data_member_location, DW_FORM_block },
  { 0, 0 }
};

static AttribDesc const attr_member_bitfield[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_type, DW_FORM_ref_udata },
  { DW_AT_data_member_location, DW_FORM_block },
  { DW_AT_bit_size, DW_FORM_data1 },
  { DW_AT_bit_offset, DW_FORM_data1 },
  { 0, 0 }
};

static AbbrevEntry const abbr_member = { DW_TAG_member, NO, attr_member };
static AbbrevEntry const abbr_member_bitfield = { DW_TAG_member, NO, attr_member_bitfield };

static AbbrevEntry const abbr_pointertype = { DW_TAG_pointer_type, NO, attr_qualtype };

static AttribDesc const attr_proctypeformal[] = {
  { DW_AT_type, DW_FORM_ref_udata },
  { DW_AT_name, DW_FORM_string },
  { 0, 0 }
};

static AttribDesc const attr_proctypeformal_invented[] = {
  { DW_AT_type, DW_FORM_ref_udata },
  { DW_AT_name, DW_FORM_string },
  { DW_AT_artificial, DW_FORM_flag },
  { 0, 0 }
};

static AttribDesc const attr_proctypeformal_anon[] = {
  { DW_AT_type, DW_FORM_ref_udata },
  { 0, 0 }
};

static AttribDesc const attr_proctypeformal_default[] = {
  { DW_AT_type, DW_FORM_ref_udata },
  { DW_AT_name, DW_FORM_string },
  { DW_AT_default_value, DW_FORM_indirect },
  { 0, 0 }
};

static AbbrevEntry const abbr_proctypeformal = { DW_TAG_formal_parameter, NO, attr_proctypeformal };
static AbbrevEntry const abbr_proctypeformal_invented = { DW_TAG_formal_parameter, NO, attr_proctypeformal_invented };
static AbbrevEntry const abbr_proctypeformal_anon = { DW_TAG_formal_parameter, NO, attr_proctypeformal_anon };
static AbbrevEntry const abbr_proctypeformal_default = { DW_TAG_formal_parameter, NO, attr_proctypeformal_default };

static AttribDesc const attr_ptrtomembertype[] = {
  { DW_AT_containing_type, DW_FORM_ref_udata },
  { DW_AT_type, DW_FORM_ref_udata },
  { 0, 0 }
};

static AbbrevEntry const abbr_ptrtomembertype = { DW_TAG_ptr_to_member_type, NO, attr_ptrtomembertype };

static AbbrevEntry const abbr_referencetype = { DW_TAG_reference_type, NO, attr_qualtype };

static AttribDesc const attr_structtype[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_byte_size, DW_FORM_udata },
  { 0, 0 }
};

static AttribDesc const attr_structtype_nodata[] = {
  { DW_AT_name, DW_FORM_string },
  { 0, 0 }
};

static AttribDesc const attr_structtype_anon[] = {
  { DW_AT_byte_size, DW_FORM_udata },
  { 0, 0 }
};

static AbbrevEntry const abbr_structtype = { DW_TAG_structure_type, YES, attr_structtype };
static AbbrevEntry const abbr_structtype_nodata = { DW_TAG_structure_type, YES, attr_structtype_nodata };
static AbbrevEntry const abbr_structtype_anon = { DW_TAG_structure_type, YES, attr_structtype_anon };
static AbbrevEntry const abbr_structtype_nodata_anon = { DW_TAG_structure_type, YES, attr_null };
static AbbrevEntry const abbr_structtype_fref = { DW_TAG_structure_type, NO, attr_structtype_nodata };

static AttribDesc const attr_subprogram[] = {
  { DW_AT_low_pc, DW_FORM_addr },
  { DW_AT_proc_body, DW_FORM_udata },
  { DW_AT_high_pc, DW_FORM_addr },
  { DW_AT_name, DW_FORM_string },
  { DW_AT_external, DW_FORM_flag },
  { DW_AT_type, DW_FORM_ref_udata },
  { 0, 0 }
};

static AttribDesc const attr_subprogram_void[] = {
  { DW_AT_low_pc, DW_FORM_addr },
  { DW_AT_proc_body, DW_FORM_udata },
  { DW_AT_high_pc, DW_FORM_addr },
  { DW_AT_name, DW_FORM_string },
  { DW_AT_external, DW_FORM_flag },
  { 0, 0 }
};

static AttribDesc const attr_subprogram_ref[] = {
  { DW_AT_low_pc, DW_FORM_addr },
  { DW_AT_proc_body, DW_FORM_udata },
  { DW_AT_high_pc, DW_FORM_addr },
  { DW_AT_specification, DW_FORM_ref_udata },
  { 0, 0 }
};

static AttribDesc const attr_procdecl[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_declaration, DW_FORM_flag },
  { DW_AT_type, DW_FORM_ref_udata },
  { DW_AT_external, DW_FORM_flag },
  { 0, 0 }
};

static AttribDesc const attr_procdecl_void[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_declaration, DW_FORM_flag },
  { DW_AT_external, DW_FORM_flag },
  { 0, 0 }
};

static AttribDesc const attr_procdecl_virtual[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_declaration, DW_FORM_flag },
  { DW_AT_type, DW_FORM_ref_udata },
  { DW_AT_virtuality, DW_FORM_data1 },
  { DW_AT_vtable_elem_location, DW_FORM_data1 },
  { 0, 0 }
};

static AttribDesc const attr_procdecl_virtual_void[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_declaration, DW_FORM_flag },
  { DW_AT_virtuality, DW_FORM_data1 },
  { DW_AT_vtable_elem_location, DW_FORM_data1 },
  { 0, 0 }
};

static AbbrevEntry const abbr_subprogram = { DW_TAG_subprogram, YES, attr_subprogram };
static AbbrevEntry const abbr_subprogram_void = { DW_TAG_subprogram, YES, attr_subprogram_void };
static AbbrevEntry const abbr_subprogram_ref = { DW_TAG_subprogram, YES, attr_subprogram_ref };
static AbbrevEntry const abbr_procdecl = { DW_TAG_subprogram, YES, attr_procdecl };
static AbbrevEntry const abbr_procdecl_void = { DW_TAG_subprogram, YES, attr_procdecl_void };
static AbbrevEntry const abbr_procdecl_virtual = { DW_TAG_subprogram, YES, attr_procdecl_virtual };
static AbbrevEntry const abbr_procdecl_virtual_void = { DW_TAG_subprogram, YES, attr_procdecl_virtual_void };

static AttribDesc const attr_subroutinetype[] = {
  { DW_AT_type, DW_FORM_ref_udata },
  { 0, 0 }
};

static AbbrevEntry const abbr_subroutinetype = { DW_TAG_subroutine_type, YES, attr_subroutinetype };
static AbbrevEntry const abbr_subroutinetype_void = { DW_TAG_subroutine_type, YES, attr_null };

static AttribDesc const attr_typedef[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_type, DW_FORM_ref_udata },
  { 0, 0 }
};

static AbbrevEntry const abbr_typedef = { DW_TAG_typedef, NO, attr_typedef };

static AttribDesc const attr_uniontype[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_byte_size, DW_FORM_udata },
  { 0, 0 }
};

static AttribDesc const attr_uniontype_anon[] = {
  { DW_AT_byte_size, DW_FORM_udata },
  { 0, 0 }
};

static AttribDesc const attr_uniontype_nodata[] = {
  { DW_AT_name, DW_FORM_string },
  { 0, 0 }
};

static AbbrevEntry const abbr_uniontype = { DW_TAG_union_type, YES, attr_uniontype };
static AbbrevEntry const abbr_uniontype_anon = { DW_TAG_union_type, YES, attr_uniontype_anon };
static AbbrevEntry const abbr_uniontype_nodata = { DW_TAG_union_type, YES, attr_uniontype_nodata };
static AbbrevEntry const abbr_uniontype_fref = { DW_TAG_union_type, NO, attr_uniontype_nodata };

static AbbrevEntry const abbr_unspecifiedparameters = { DW_TAG_unspecified_parameters, NO, attr_null };

static AttribDesc const attr_variable[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_type, DW_FORM_ref_udata },
  { DW_AT_location, DW_FORM_block },
  { 0, 0 }
};

static AttribDesc const attr_variable_extern[] = {
  { DW_AT_name, DW_FORM_string },
  { DW_AT_type, DW_FORM_ref_udata },
  { DW_AT_location, DW_FORM_block },
  { DW_AT_external, DW_FORM_flag },
  { 0, 0 }
};

static AbbrevEntry const abbr_variable = { DW_TAG_variable, NO, attr_variable };
static AbbrevEntry const abbr_formalparameter = { DW_TAG_formal_parameter, NO, attr_variable };
static AbbrevEntry const abbr_variable_extern = { DW_TAG_variable, NO, attr_variable_extern };

static AbbrevEntry const abbr_volatiletype = { DW_TAG_volatile_type, NO, attr_qualtype };

static AbbrevEntry const * const abbrevlist_c[] = {
  &abbr_null,
  &abbr_arraybound,
  &abbr_arraybound_open,
  &abbr_arraytype,
  &abbr_basetype,
  &abbr_compileunit,
  &abbr_compileunit_nostmt,
  &abbr_compileunit_nocode,
  &abbr_consttype,
  &abbr_enum,
  &abbr_enum_anon,
  &abbr_enumerator,
  &abbr_enumerator_signed,
  &abbr_formalparameter,
  &abbr_lexicalblock,
  &abbr_emptylexicalblock,
  &abbr_member,
  &abbr_member_bitfield,
  &abbr_pointertype,
  &abbr_proctypeformal,
  &abbr_proctypeformal_anon,
  &abbr_referencetype,
  &abbr_structtype,
  &abbr_structtype_nodata,
  &abbr_structtype_anon,
  &abbr_structtype_nodata_anon,
  &abbr_structtype_fref,
  &abbr_subprogram,
  &abbr_subprogram_void,
  &abbr_subprogram_ref,
  &abbr_procdecl,
  &abbr_procdecl_void,
  &abbr_subroutinetype,
  &abbr_subroutinetype_void,
  &abbr_typedef,
  &abbr_uniontype,
  &abbr_uniontype_anon,
  &abbr_uniontype_fref,
  &abbr_unspecifiedparameters,
  &abbr_variable,
  &abbr_variable_extern,
  &abbr_volatiletype,
  NULL
};

static AbbrevEntry const * const abbrevlist_cpp[] = {
  &abbr_null,
  &abbr_arraybound,
  &abbr_arraybound_open,
  &abbr_arraytype,
  &abbr_basetype,
  &abbr_classtype,
  &abbr_classtype_nodata,
  &abbr_classtype_anon,
  &abbr_classtype_nodata_anon,
  &abbr_classtype_fref,
  &abbr_compileunit,
  &abbr_compileunit_nostmt,
  &abbr_compileunit_nocode,
  &abbr_consttype,
  &abbr_enum,
  &abbr_enum_anon,
  &abbr_enumerator,
  &abbr_enumerator_signed,
  &abbr_formalparameter,
  &abbr_friend,
  &abbr_inheritance,
  &abbr_lexicalblock,
  &abbr_emptylexicalblock,
  &abbr_member,
  &abbr_member_bitfield,
  &abbr_pointertype,
  &abbr_ptrtomembertype,
  &abbr_proctypeformal,
  &abbr_proctypeformal_invented,
  &abbr_proctypeformal_anon,
  &abbr_proctypeformal_default,
  &abbr_referencetype,
  &abbr_structtype,
  &abbr_structtype_nodata,
  &abbr_structtype_anon,
  &abbr_structtype_nodata_anon,
  &abbr_structtype_fref,
  &abbr_subprogram,
  &abbr_subprogram_void,
  &abbr_subprogram_ref,
  &abbr_procdecl,
  &abbr_procdecl_void,
  &abbr_procdecl_virtual,
  &abbr_procdecl_virtual_void,
  &abbr_subroutinetype,
  &abbr_subroutinetype_void,
  &abbr_typedef,
  &abbr_uniontype,
  &abbr_uniontype_anon,
  &abbr_uniontype_fref,
  &abbr_unspecifiedparameters,
  &abbr_variable,
  &abbr_variable_extern,
  &abbr_volatiletype,
  NULL
};

static uint8 const block_reg[] = { 1, DW_FORM_data1, 0 };
static uint8 const block_extern[] = { 2, DW_FORM_data1, DW_FORM_addr, 0 };
static uint8 const block_auto_fb[] = { 4, DW_FORM_data1, DW_FORM_data1, DW_FORM_sdata, DW_FORM_data1, 0 };
static uint8 const block_auto[] = { 2, DW_FORM_data1, DW_FORM_sdata, 0 };
static uint8 const block_mem[] = { 2, DW_FORM_data1, DW_FORM_udata, 0 };

#define ATTRIBBUFSIZE 20

static uint32 Dw_WriteLEB128_U_3(uint32 u, uint32 offset) {
  uint8 b[3];
  u &= ~0x80000000;
  b[0] = ((Uint)u & 0x7f) | 0x80;
  u = u >> 7;
  b[1] = ((Uint)u & 0x7f) | 0x80;
  u = u >> 7;
  b[2] = ((Uint)u & 0x7f);
  if ((u & ~0x7f) != 0) syserr("Dw_WriteLEB128_U_3");
  return Dw_WriteBN(b, 3, offset);
}

static uint32 Dw_WriteLEB128_U(uint32 u, uint32 offset) {
  uint8 b[5];
  uint32 n = 0;
  for (; ; n++) {
    b[n] = (Uint)u & 0x7f;
    u = u >> 7;
    if (u == 0) break;
    b[n] |= 0x80;
  }
  return Dw_WriteBN(b, ++n, offset);
}

static uint32 Dw_WriteLEB128_S(int32 u, uint32 offset) {
  uint8 b[5];
  uint32 n = 0;
  for (; ; n++) {
    b[n] = (Uint)u & 0x7f;
    u = signed_rightshift_(u, 7);
    if (b[n] & 0x40) {
      if (u == -1) break;
    } else
      if (u == 0) break;
    b[n] |= 0x80;
  }
  return Dw_WriteBN(b, ++n, offset);
}

static uint32 Dw_SizeLEB128_U(uint32 u) {
  return u < 0x80 ? 1 :
         u < 0x80 << 7 ? 2 :
         u < 0x80 << 14 ? 3 :
         u < 0x80 << 21 ? 4 :
                          5;
}

static uint32 Dw_SizeLEB128_S(int32 u) {
  uint32 n = 1;
  for (; ; n++) {
    int32 k = u & 0x7f;
    u = signed_rightshift_(u, 7);
    if (k & 0x40) {
      if (u == -1) break;
    } else
      if (u == 0) break;
  }
  return n;
}

typedef struct { Uint form; uint32 i; void const *p; } AttribVal;
#define blocksize_(p) ((p) & 0xff)
#define blockix_(p) ((p) >> 8)

static uint32 Dw_2_BlockArgs(AttribVal const *w, Uint n, uint8 const *block_mem) {
  uint32 size = 0;
  for (w += n; ; w++)
    switch (*++block_mem) {
    case 0:             return size | ((uint32)n << 8);
    case DW_FORM_data1: size += 1; break;
    case DW_FORM_data2: size += 2; break;
    case DW_FORM_addr:
    case DW_FORM_data4: size += 4; break;
    case DW_FORM_udata: size += Dw_SizeLEB128_U(w->i); break;
    case DW_FORM_sdata: size += Dw_SizeLEB128_S(w->i); break;
    }
}

static AbbrevEntry const *Dw_2_ItemEntry(Dw_ItemList const *p, AttribVal *w) {
  switch (debsort_(p)) {
  case DW_TAG_compile_unit:
    w[0].p = sect_name_(p);
    w[1].p = version_banner();
    w[2].i = (LanguageIsCPlusPlus) ? LANG_C_PLUS_PLUS : LANG_C89;
    if (dw_baseseg.len == 0)
      return &abbr_compileunit_nocode;
    else {
      w[3].i = 0; w[3].p = sect_codeseg_(p);
      w[4].i = dw_baseseg.len; w[4].p = sect_codeseg_(p);
      if (dw_coord_p == NULL)
        return &abbr_compileunit_nostmt;
      else {
        w[5].i = 0; w[5].p = dw_lineinfo_sym;
        return &abbr_compileunit;
      }
    }
  case DW_TAG_subprogram:
    w[0].i = proc_entry_(p); w[0].p = proc_codeseg_(p);
    w[1].i = proc_body_(p) - proc_entry_(p);
    w[2].i = high_pc_(p); w[2].p = proc_codeseg_(p);
    if (proc_decl_(p) != NULL) {
      w[3].i = dbgloc_(proc_decl_(p));
      return &abbr_subprogram_ref;
    }
    w[3].p = Dw_Unmangle(proc_name_(p));
    w[4].i = proc_global_(p);
    { Dw_ItemList *restype = proc_type_(p);
      if (is_void_type_(restype))
        return &abbr_subprogram_void;
      else {
        w[5].i = dbgloc_(restype);
        return &abbr_subprogram;
      }
    }

  case DW_TAG_procdecl:
    { uint32 sort = 0, i = 2;
      static AbbrevEntry const * const pdsort[] = {
        &abbr_procdecl_void, &abbr_procdecl,
        &abbr_procdecl_virtual_void, &abbr_procdecl_virtual
      };

      w[0].p = Dw_Unmangle(procdecl_name_(p));
      w[1].i = YES;
      if (!is_void_type_(procdecl_type_(p))) {
        w[i++].i = dbgloc_(procdecl_type_(p));
        sort |= 1;
      }
      if (procdecl_stg_(p) & bitofstg_(s_virtual)) {
        w[i++].i = DW_VIRTUALITY_virtual;
        w[i++].i = procdecl_voffset_(p);
        sort |= 2;
      } else
        w[i++].i = procdecl_global_(p);
      return pdsort[sort];
    }

  case DW_TAG_unspecified_parameters:
    return &abbr_unspecifiedparameters;

  case DW_TAG_subroutine_type:
    { Dw_ItemList *restype = proctype_type_(p);
      if (is_void_type_(restype))
        return &abbr_subroutinetype_void;
      else {
        w[0].i = dbgloc_(restype);
        return &abbr_subroutinetype;
      }
    }

  case DW_TAG_proctype_formal:
    w[0].i = dbgloc_(formal_type_(p));
    if (formal_name_(p) == NULL)
      return &abbr_proctypeformal_anon;
    else {
      Expr *dflt = formal_defltexpr_(p);
      w[1].p = symname_(formal_name_(p));
      if (formal_invented_(p)) {
        w[2].i = YES;
        return &abbr_proctypeformal_invented;
      }
      if (dflt == NULL)
        return &abbr_proctypeformal;

      switch (h0_(dflt)) {
      case s_integer:
        w[2].form = DW_FORM_udata;
        w[2].i = intval_(dflt);
        break;

      case s_string:
        w[2].form = DW_FORM_string_expr;
        w[2].p = ((String *)dflt)->strseg;
        break;

      case s_floatcon:
        if (mcrepofexpr(dflt) & MCR_SIZE_MASK < 8) {
          w[2].form = DW_FORM_data4;
          w[2].i = ((FloatCon *)dflt)->floatbin.fb.val;
        } else {
          w[2].form = DW_FORM_data8;
          w[2].p = ((FloatCon *)dflt)->floatbin.irep;
        }
        break;

      default:
        w[2].form = DW_FORM_ref_udata;
        w[2].i = dbgloc_(formal_defltfn_(p));
        break;
      }
      return &abbr_proctypeformal_default;
    }

  case DW_TAG_ptr_to_member_type:
    w[0].i = dbgloc_(ptrtomem_container_(p));
    w[1].i = dbgloc_(ptrtomem_type_(p));
    return &abbr_ptrtomembertype;

  case DW_TAG_typedef:
    w[0].p = type_name_(p);
    w[1].i = dbgloc_(type_type_(p));
    return &abbr_typedef;

  case DW_TAG_lexical_block:
    w[0].i = startscope_codeaddr_(p); w[0].p = startscope_codeseg_(p);
    w[1].i = endscope_codeaddr_(startscope_end_(p)); w[1].p = startscope_codeseg_(p);
    if (debsort_(cdr_(p)) == DW_TAG_end_lexical_block)
      return &abbr_emptylexicalblock;
    else
      return &abbr_lexicalblock;

  case DW_TAG_ignore:
  case DW_TAG_end_lexical_block:
  case DW_TAG_endproc:
    return NULL;

  case DW_TAG_enumerator:
    w[0].p = enumerator_name_(p);
    w[1].i = enumerator_val_(p);
    return &abbr_enumerator;

  case DW_TAG_enumeration_type:
    if (enum_name_(p) != NULL) {
      w[0].p = enum_name_(p);
      w[1].i = enum_size_(p);
      return &abbr_enum;
    } else {
      w[1].i = enum_size_(p);
      return &abbr_enum_anon;
    }

  case TAG_padding: /* null entry to terminate sibling chains */
    return &abbr_null;

  case DW_TAG_member:
    w[0].p = member_name_(p);
    w[1].i = dbgloc_(member_type_(p));
    w[2].p = block_mem;
    { Uint n = ATTRIBBUFSIZE - block_mem[0];
      w[n].i = DW_OP_plus_uconst;
      w[n+1].i = member_offset_(p);
      w[2].i = Dw_2_BlockArgs(w, n, block_mem);
    }
    if (member_bsize_(p) == 0)
      return &abbr_member;
    else {
      w[3].i = member_bsize_(p);
      w[4].i = member_boffset_(p);
      return &abbr_member_bitfield;
    }

  case DW_TAG_variable:
  case DW_TAG_formal_parameter:
    { Uint n;
      uint8 const *b;
      w[0].p = symname_(var_sym_(p));
      w[1].i = dbgloc_(var_type_(p));
      switch (var_stgclass_(p)) {
      case Stg_Extern:
      case Stg_Static:
        b = block_extern;
        n = ATTRIBBUFSIZE - b[0];
        w[n].i = DW_OP_addr;
        { Symstr *sym = var_base_(p).sym;
          obj_symref(sym, symext_(sym) == NULL ? xr_data|xr_weak : xr_data, 0);
          w[n+1].i = 0; w[n+1].p = sym;
        }
        break;

      case Stg_Reg:
      case Stg_ArgReg:
        if (var_base_(p).r == R_NOFPREG) return NULL;
        b = block_reg;
        n = ATTRIBBUFSIZE - b[0];
        w[n].i = DW_OP_REG0 + var_loc_(p);
        break;

      case Stg_Auto:
      case Stg_ArgAuto:
        b = block_auto;
        n = ATTRIBBUFSIZE - b[0];
        w[n].i = DW_OP_BREG0 + var_base_(p).r;
        w[n+1].i = var_loc_(p);
        break;

      default:
        syserr("Dw_2_ItemEntry var stgclass %d", var_stgclass_(p));
        n = 0; b = NULL;
      }
      w[2].p = b;
      w[2].i = Dw_2_BlockArgs(w, n, b);
    }
    if (var_stgclass_(p) == Stg_Extern) {
      w[3].i = YES;
      return &abbr_variable_extern;
    } else
      return (debsort_(p) == DW_TAG_formal_parameter) ? &abbr_formalparameter :
                                                        &abbr_variable;

  case DW_TAG_base_type:
    switch (basetype_typecode_(p)) {
    case FT_signed_char:      w[0].i = 1; w[1].i = DW_ATE_signed; break;
    case FT_unsigned_char:    w[0].i = 1; w[1].i = DW_ATE_unsigned; break;
    case FT_signed_short:     w[0].i = sizeof_short; w[1].i = DW_ATE_signed; break;
    case FT_unsigned_short:   w[0].i = sizeof_short; w[1].i = DW_ATE_unsigned; break;
    case FT_signed_integer:   w[0].i = sizeof_int; w[1].i = DW_ATE_signed; break;
    case FT_unsigned_integer: w[0].i = sizeof_int; w[1].i = DW_ATE_unsigned; break;
    case FT_signed_long:      w[0].i = sizeof_long; w[1].i = DW_ATE_signed; break;
    case FT_signed_long_long: w[0].i = sizeof_longlong; w[1].i = DW_ATE_signed; break;
    case FT_unsigned_long:    w[0].i = sizeof_long; w[1].i = DW_ATE_unsigned; break;
    case FT_unsigned_long_long:w[0].i = sizeof_longlong; w[1].i = DW_ATE_unsigned; break;
    case FT_float:            w[0].i = sizeof_float; w[1].i = DW_ATE_float; break;
    case FT_dbl_prec_float:   w[0].i = sizeof_double; w[1].i = DW_ATE_float; break;
    case FT_void:             w[0].i = 0; w[1].i = DW_ATE_signed; break;
    default:                  syserr("Dw_2_ItemEntry basetype %d", basetype_typecode_(p));
    }
    return &abbr_basetype;


  case DW_TAG_pointer_type:
    w[0].i = dbgloc_(qualtype_qualifiedtype_(p));
    return &abbr_pointertype;

  case DW_TAG_const_type:
    w[0].i = dbgloc_(qualtype_qualifiedtype_(p));
    return &abbr_consttype;

  case DW_TAG_volatile_type:
    w[0].i = dbgloc_(qualtype_qualifiedtype_(p));
    return &abbr_volatiletype;

  case DW_TAG_reference_type:
    w[0].i = dbgloc_(qualtype_qualifiedtype_(p));
    return &abbr_referencetype;

  case DW_TAG_array_type:
    w[0].i = dbgloc_(array_basetype_(p));
    return &abbr_arraytype;

  case DW_TAG_array_bound:
    if (arraybound_open_(p))
      return &abbr_arraybound_open;
    else {
      w[0].i = arraybound_upperbound_(p);
      return &abbr_arraybound;
    }

  case DW_TAG_fref:
    w[0].p = struct_name_(p);
    if (struct_undefsort_(p) == DW_TAG_class_type)
      return &abbr_classtype_fref;
    else if (struct_undefsort_(p) == DW_TAG_structure_type)
      return &abbr_structtype_fref;
    else
      return &abbr_uniontype_fref;

  case DW_TAG_class_type:
    if (struct_name_(p) != NULL) {
      if (struct_size_(p) == 0)
        return &abbr_classtype_nodata_anon;
      else {
        w[0].i = struct_size_(p);
        return &abbr_classtype_anon;
      }
    } else {
      w[0].p = struct_name_(p);
      if (struct_size_(p) == 0)
        return &abbr_classtype_nodata;
      else {
        w[1].i = struct_size_(p);
        return &abbr_classtype;
      }
    }

  case DW_TAG_union_type:
    if (struct_name_(p) == NULL) {
      w[0].i = struct_size_(p);
      return &abbr_uniontype_anon;
    } else {
      w[0].p = struct_name_(p);
      if (struct_size_(p) == 0)
        return &abbr_uniontype_nodata;
      else {
        w[1].i = struct_size_(p);
        return &abbr_uniontype;
      }
    }

  case DW_TAG_structure_type:
    if (struct_name_(p) == NULL) {
      if (struct_size_(p) == 0)
        return &abbr_structtype_nodata_anon;
      else {
        w[0].i = struct_size_(p);
        return &abbr_structtype_anon;
      }
    } else {
      w[0].p = struct_name_(p);
      if (struct_size_(p) == 0)
        return &abbr_structtype_nodata;
      else {
        w[1].i = struct_size_(p);
        return &abbr_structtype;
      }
    }

  case DW_TAG_inheritance:
    w[0].i = dbgloc_(inherit_type_(p));
    { Uint n = ATTRIBBUFSIZE - block_mem[0];
      w[n].i = DW_OP_plus_uconst;
      w[n+1].i = inherit_offset_(p);
      w[1].i = Dw_2_BlockArgs(w, n, block_mem); w[1].p = block_mem;
    }
    w[2].i = inherit_virt_(p) ? DW_VIRTUALITY_virtual : DW_VIRTUALITY_none;
    return &abbr_inheritance;

  default:
    syserr("Dw_2_ItemEntry %d", debsort_(p));
    return NULL;
  }
}

static Uint Dw_2_AbbrevLookup(AbbrevEntry const *abbrev) {
  Uint i;
  AbbrevEntry const * const *table = LanguageIsCPlusPlus ? abbrevlist_cpp : abbrevlist_c;
  for (i = 0; table[i] != NULL; i++)
    if (table[i] == abbrev) return i;
  syserr("Dw_2_AbbrevLookup %p (%d)", abbrev, abbrev->tag);
  return 0;
}

static uint32 Dw_2_InfoItemSize(Dw_ItemList const *p, AttribVal *w) {
  AbbrevEntry const *abbrev = Dw_2_ItemEntry(p, w);
  if (abbrev == NULL) return 0;
  { Uint abbrevindex = Dw_2_AbbrevLookup(abbrev);
    Uint i;
    uint32 size = Dw_SizeLEB128_U(abbrevindex);
    for (i = 0; ; i++) {
      Uint form = abbrev->attribs[i].form;
    indirect:
      switch (form) {
      case 0:                return size;
      case DW_FORM_indirect: form = w[i].form;
                             size += Dw_SizeLEB128_U(form);
                             if (form == DW_FORM_indirect) syserr("Dw_2_InfoItemSize indirect indirect");
                             goto indirect;
      case DW_FORM_flag:
      case DW_FORM_data1:    size += 1; break;
      case DW_FORM_data2:    size += 2; break;
      case DW_FORM_tref:
      case DW_FORM_addr:
      case DW_FORM_data4:    size += 4; break;
      case DW_FORM_data8:    size += 8; break;
      case DW_FORM_ref_udata:if (w[i].i == 0               /* forward reference before writing */
                                 || (w[i].i & 0x80000000)) /* forward reference after writing */
                               size += 3;
                             else
                               size += Dw_SizeLEB128_U(w[i].i);
                             break;
      case DW_FORM_udata:    size += Dw_SizeLEB128_U(w[i].i); break;
      case DW_FORM_string:   size += (uint32)strlen((char const *)w[i].p)+1; break;
      case DW_FORM_block:    size += blocksize_(w[i].i) + Dw_SizeLEB128_U(blocksize_(w[i].i)); break;

      case DW_FORM_string_expr:
        { StringSegList const *p = (StringSegList const *)w[i].p;
          for (; p != NULL; p = p->strsegcdr) size += p->strseglen;
          size += 1;
        }
        break;

      default:               syserr("Dw_2_InfoItemSize form %d", abbrev->attribs[i].form);
      }
    }
  }
}

static void Dw_RoundUp(uint32 offset) {
  if ((offset & 3) != 0) {
    uint32 w = 0;
    obj_writedebug(&w, 4 - (offset & 3));
  }
}

void Dw_2_WriteInfo(void) {
  Dw_ItemList *p;
  uint32 infosize, offset;
  AttribVal w[ATTRIBBUFSIZE];
  dw_xrefs = NULL;
  obj_startdebugarea(Dwarf2DebugAreaName);
  for (infosize = 11, p = dw_list; p != NULL; p = cdr_(p)) {
    uint32 n = Dw_2_InfoItemSize(p, w);
    dbgloc_(p) = n == 0 ? 0 : infosize;
    infosize += n;
  }
  for (p = dw_list; p != NULL; p = cdr_(p)) {
    if (dbgloc_(p) != 0) dbgloc_(p) |= 0x80000000;
  }
  offset = Dw_WriteW(infosize-4, 0);
  offset = Dw_WriteH(2, offset);
  offset = Dw_WriteW_Relocated(0, dw_abbrev_sym, offset);
  offset = Dw_WriteB(4, offset);

  for (p = dw_list; p != NULL; p = cdr_(p)) {
    AbbrevEntry const *abbrev = Dw_2_ItemEntry(p, w);
    if (abbrev == NULL) continue;
    { Uint abbrevindex = Dw_2_AbbrevLookup(abbrev);
      Uint i;
      dbgloc_(p) &= ~0x80000000;
      if (offset != dbgloc_(p)) syserr("dw_2_writeinfo %lx != %lx", offset, dbgloc_(p));
      offset = Dw_WriteLEB128_U(abbrevindex, offset);
      for (i = 0; ; i++)
        if (abbrev->attribs[i].form == 0)
          break;
        else {
          Uint form = abbrev->attribs[i].form;
        indirect:
          switch (form) {
          default:               syserr("Dw_2_WriteInfo from %d", abbrev->attribs[i].form);
          case DW_FORM_flag:
          case DW_FORM_data1:    offset = Dw_WriteB(w[i].i, offset); break;
          case DW_FORM_data2:    offset = Dw_WriteH(w[i].i, offset); break;
          case DW_FORM_data4:    offset = Dw_WriteW(w[i].i, offset); break;
          case DW_FORM_data8:    offset = Dw_WriteL((uint32 const *)w[i].p, offset); break;
          case DW_FORM_ref_udata:if (w[i].i & 0x80000000)
                                   offset = Dw_WriteLEB128_U_3(w[i].i, offset);
                                 else
                                   offset = Dw_WriteLEB128_U(w[i].i, offset);
                                 break;
          case DW_FORM_udata:    offset = Dw_WriteLEB128_U(w[i].i, offset); break;
          case DW_FORM_string:   offset = Dw_WriteInlineString((char const *)w[i].p, offset); break;
          case DW_FORM_tref:
          case DW_FORM_addr:     offset = Dw_WriteW_Relocated(w[i].i, (Symstr const *)w[i].p, offset); break;

          case DW_FORM_indirect:
            { uint32 n = form = w[i].form;
              if (n == DW_FORM_string_expr) n = DW_FORM_string;
              offset = Dw_WriteLEB128_U(n, offset);
              goto indirect;
            }

          case DW_FORM_string_expr:
            { StringSegList const *p = (StringSegList const *)w[i].p;
              for (; p != NULL; p = p->strsegcdr)
                offset = Dw_WriteBN((uint8 *)p->strsegbase, p->strseglen, offset);

              offset = Dw_WriteB(0, offset);
            }
            break;

          case DW_FORM_block:
            offset = Dw_WriteLEB128_U(blocksize_(w[i].i), offset);
            { uint8 const *p = (uint8 const *)w[i].p+1;
              uint32 j = blockix_(w[i].i);
              for (; *p != 0; p++, j++)
                switch (*p) {
                case DW_FORM_data1: offset = Dw_WriteB(w[j].i, offset); break;
                case DW_FORM_data2: offset = Dw_WriteH(w[j].i, offset); break;
                case DW_FORM_data4: offset = Dw_WriteW(w[j].i, offset); break;
                case DW_FORM_addr:  offset = Dw_WriteW_Relocated(w[j].i, (Symstr const *)w[j].p, offset); break;
                case DW_FORM_udata: offset = Dw_WriteLEB128_U(w[j].i, offset); break;
                case DW_FORM_sdata: offset = Dw_WriteLEB128_S(w[j].i, offset); break;
                default:            syserr("Dw_2_WriteInfo block op %d", *p);
                }
            }
          }
        }
    }
  }
  Dw_RoundUp(offset);
  obj_enddebugarea(Dwarf2DebugAreaName, dw_xrefs);
}

void Dw_2_WriteAbbrevs(void) {
  AbbrevEntry const * const *p = LanguageIsCPlusPlus ? abbrevlist_cpp : abbrevlist_c;
  Uint i;
  uint32 offset = 0;
  obj_startdebugarea(AbbrevAreaName);
  for (i = 1; p[i] != NULL; i++) {
    AbbrevEntry const *abbr = p[i];
    offset = Dw_WriteLEB128_U(i, offset);
    offset = Dw_WriteLEB128_U(abbr->tag, offset);
    offset = Dw_WriteB(abbr->children, offset);
    { AttribDesc const *attr = abbr->attribs;
      for (; ; attr++) {
        offset = Dw_WriteLEB128_U(attr->attrib, offset);
        offset = Dw_WriteLEB128_U(attr->form == DW_FORM_tref ? DW_FORM_data4 : attr->form, offset);
        if (attr->attrib == 0) break;
      }
    }
  }
  offset = Dw_WriteLEB128_U(0, offset);
  Dw_RoundUp(offset);
  obj_enddebugarea(AbbrevAreaName, NULL);
}

void Dw_2_WriteMacros(void) {
  Dw_MacroList *p;
  uint32 offset;
  dw_xrefs = NULL;
  obj_startdebugarea(MacroAreaName);
  for (offset = 0, p = dw_macrolist; p != NULL; p = cdr_(p)) {
    offset = Dw_WriteB(p->sort, offset);
    switch (p->sort) {
    case DW_MACINFO_define:
    case DW_MACINFO_undef:
      offset = Dw_WriteLEB128_U(p->fl.l, offset);
      offset = Dw_WriteInlineString(p->data.s, offset);
      break;
    case DW_MACINFO_start_file:
      offset = Dw_WriteLEB128_U(p->fl.l, offset);
      offset = Dw_WriteLEB128_U(p->data.i, offset);
      break;
    case DW_MACINFO_end_file:
      break;
    }
  }
  offset = Dw_WriteB(0, offset);
  Dw_RoundUp(offset);
  obj_enddebugarea(MacroAreaName, dw_xrefs);
}

#ifdef TARGET_HAS_HALFWORD_INSTRUCTIONS
#  define AddressQuantum 2
#else
#  define AddressQuantum 4
#endif

static uint8 const dw_lns_operands[DW_LNS_LAST] = DW_LNS_OPERANDS;

static uint32 Dw_Write_LNS_OpU(Uint op, uint32 operand, uint32 offset) {
  offset = Dw_WriteB(op, offset);
  return Dw_WriteLEB128_U(operand, offset);
}

static uint32 Dw_Write_LNS_OpS(Uint op, int32 operand, uint32 offset) {
  offset = Dw_WriteB(op, offset);
  return Dw_WriteLEB128_S(operand, offset);
}

#define LNS_RANGE 6
#define LNS_CODERANGE ((255-DW_LNS_LAST) / LNS_RANGE)
#define DW_LNS_Special_Op(linediff, codediff) \
  ((Uint)((linediff) + LNS_RANGE * (codediff) + DW_LNS_LAST + 1))
#define LNS_SPECIAL_MAX_PC \
  (DW_LNS_Special_Op(0, LNS_CODERANGE) <= 255 ? LNS_CODERANGE : LNS_CODERANGE-1)
#define  CanUseSpecialOp(linediff, codediff) \
  ((codediff) < LNS_CODERANGE \
   || ((codediff) == LNS_CODERANGE \
       && DW_LNS_Special_Op((linediff), (codediff)) <= 255))

#define Write_LNS_OpU(op, arg, offset, write) \
  ((write) ? Dw_Write_LNS_OpU(op, arg, offset) : (offset) + 1 + Dw_SizeLEB128_U(arg))
#define Write_LNS_OpS(op, arg, offset, write) \
  ((write) ? Dw_Write_LNS_OpS(op, arg, offset) : (offset) + 1 + Dw_SizeLEB128_S(arg))
#define Write_LNS_NoOp(op, offset, write) \
  ((write) ? Dw_WriteB(op, offset) : (offset) + 1)

#define WriteB(b, offset, write) \
  ((write) ? Dw_WriteB(b, offset) : (offset) + 1)
#define WriteLEB128_U(n, offset, write) \
  ((write) ? Dw_WriteLEB128_U(n, offset) : (offset) + Dw_SizeLEB128_U(n))
#define WriteW_Relocated(n, sym, offset, write) \
  ((write) ? Dw_WriteW_Relocated(n, sym, offset) : (offset) + 4)

static uint32 Dw_2_ProcessLineList(uint32 offset, bool write) {
  uint32 codeaddr = 0;
  Symstr *codeseg = NULL;
  int32 lineno = 1, column = 0;
  Dw_FileList *fp = NULL;
  Dw_FileCoord *lp;
  bool changed = NO;
  for (lp = dw_coord_p; lp != NULL; lp = cdr_(lp)) {
    int32 linediff = (int32)lp->line - lineno;
    uint32 codediff = (lp->codeaddr - codeaddr) / AddressQuantum;
    if (fp != lp->file) {
      offset = Write_LNS_OpU(DW_LNS_set_file, lp->file->index, offset, write);
      fp = lp->file;
    }
    if (lp->col != column) {
      offset = Write_LNS_OpU(DW_LNS_set_column, lp->col, offset, write);
      changed = YES;
      column = lp->col;
    }
    if (codeseg != lp->codeseg) {
      offset = WriteB(0, offset, write);  /* extended opcode escape */
      offset = WriteLEB128_U(5, offset, write); /* length */
      offset = WriteB(DW_LNE_set_address, offset, write);
      offset = WriteW_Relocated(lp->codeaddr, lp->codeseg, offset, write);
      codeseg = lp->codeseg;
      codeaddr = lp->codeaddr;
      if (linediff != 0) {
        lineno += linediff;
        if (linediff > 0 && linediff < LNS_RANGE) {
          offset = Write_LNS_NoOp(DW_LNS_Special_Op(linediff, 0), offset, write);
          changed = NO;
          continue;
        }
        offset = Write_LNS_OpS(DW_LNS_advance_line, linediff, offset, write);
      }
      offset = Write_LNS_NoOp(DW_LNS_copy, offset, write);
      changed = NO;
      continue;
    }
    if (linediff < 0 || linediff >= LNS_RANGE) {
      offset = Write_LNS_OpS(DW_LNS_advance_line, linediff, offset, write);
      lineno += linediff;
      changed = YES;
      linediff = 0;
    }
    if (codediff > LNS_SPECIAL_MAX_PC
        && CanUseSpecialOp(linediff, codediff - LNS_SPECIAL_MAX_PC)) {
      offset = Write_LNS_NoOp(DW_LNS_const_add_pc, offset, write);
      codeaddr += LNS_SPECIAL_MAX_PC * AddressQuantum;
      codediff -= LNS_SPECIAL_MAX_PC;
      changed = YES;
    }
    if (!CanUseSpecialOp(linediff, codediff)) {
      offset = Write_LNS_OpU(DW_LNS_advance_pc, codediff, offset, write);
      codeaddr += codediff * AddressQuantum;
      codediff = 0;
      changed = YES;
    }
    if (linediff == 0 && codediff == 0) {
      if (changed) offset = Write_LNS_NoOp(DW_LNS_copy, offset, write);
    } else {
      lineno += linediff; codeaddr += codediff * AddressQuantum;
      offset = Write_LNS_NoOp(DW_LNS_Special_Op(linediff, codediff), offset, write);
    }
    changed = NO;
    if (cdr_(lp) == NULL || cdr_(lp)->codeseg != codeseg) {
      lineno = 1; column = 0;
      offset = WriteB(0, offset, write);  /* extended opcode escape */
      offset = WriteLEB128_U(1, offset, write); /* length */
      offset = WriteB(DW_LNE_end_sequence, offset, write);
    }
  }
  return offset;
}

void Dw_2_WriteLineinfo(void) {
  uint32 offset, headerlen = 17+DW_LNS_LAST;
  uint32 infosize;
  Dw_FileList *fp;
  Dw_PathList *pp;

  if (dw_coord_p != NULL) {
    dw_coord_sentinel.codeseg = bindsym_(codesegment);
    *dw_coord_q = &dw_coord_sentinel;
    dw_coord_sentinel =  *(Dw_FileCoord *)dw_coord_q;
    dw_coord_sentinel.cdr = NULL;
    dw_coord_sentinel.codeaddr =
        (LanguageIsCPlusPlus) ? dw_mapped_codebase+dw_mapped_codep : codebase+codep;
  }
  dw_pathlist = (Dw_PathList *)dreverse((List *)dw_pathlist);
  for (pp = dw_pathlist; pp != NULL; pp = cdr_(pp))
    headerlen += (uint32)strlen(pp->name) + 1;

  dw_filelist = (Dw_FileList *)dreverse((List *)dw_filelist);
  if (dw_filelist->index == 0)
    dw_filelist = cdr_(dw_filelist);
  for (fp = dw_filelist; fp != NULL; fp = cdr_(fp)) {
    char const *name = fp->filename;
    Uint ix = 0;
    if (fp->dir != NULL) {
      name += fp->dir->len;
      ix = fp->dir->index;
    }
    headerlen += (uint32)strlen(name) + 1 +
                 Dw_SizeLEB128_U(ix) +
                 Dw_SizeLEB128_U(fp->timestamp) +
                 Dw_SizeLEB128_U(fp->filelength);
  }
  infosize = Dw_2_ProcessLineList(headerlen, NO);
  dw_xrefs = NULL;
  obj_startdebugarea(Dwarf2LineInfoAreaName);
  offset = 0;
  offset = Dw_WriteW(infosize - 4, offset);   /* end of area, relative to here */
  offset = Dw_WriteH(1, offset);
  offset = Dw_WriteW(headerlen - 10, offset); /* end of header, relative to here */
  offset = Dw_WriteB(AddressQuantum, offset);
  offset = Dw_WriteB(1, offset);              /* all entries start a statement */
  offset = Dw_WriteB(0, offset);              /* line-base */
  offset = Dw_WriteB(LNS_RANGE, offset);
  offset = Dw_WriteB(DW_LNS_LAST+1, offset);
  offset = Dw_WriteBN(dw_lns_operands, DW_LNS_LAST, offset);

  for (pp = dw_pathlist; pp != NULL; pp = cdr_(pp))
    offset = Dw_WriteInlineString(pp->name, offset);
  offset = Dw_WriteB(0, offset);              /* include directories */

  for (fp = dw_filelist; fp != NULL; fp = cdr_(fp)) {
    char const *name = fp->filename;
    Uint ix = 0;
    if (fp->dir != NULL) {
      name += fp->dir->len;
      ix = fp->dir->index;
    }
    offset = Dw_WriteInlineString(name, offset);
    offset = Dw_WriteLEB128_U(ix, offset);
    offset = Dw_WriteLEB128_U(fp->timestamp, offset);
    offset = Dw_WriteLEB128_U(fp->filelength, offset);
  }
  offset = Dw_WriteB(0, offset);              /* terminator */
  offset = Dw_2_ProcessLineList(offset, YES);
  Dw_RoundUp(offset);
  obj_enddebugarea(Dwarf2LineInfoAreaName, dw_xrefs);
}

#else

typedef int dummy; /* prevent translation unit from being empty */

#endif /* defined(TARGET_HAS_DWARF) && defined(TARGET_HAS_DEBUGGER) */

/* End of mip/dwarf2.c */
