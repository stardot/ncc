/*
 * C compiler file mip/dw_int.h
 * Copyright:   (C) 1995, Advanced RISC Machines Limited. All rights reserved.
 * Interface between main DWARF debug formatter and table writers.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

/* Private tag values used to mark DbgList items. They don't make it out */
/* into the debug tables.                                                */

#define DW_TAG_endproc (DW_TAG_lo_user)
#define DW_TAG_end_lexical_block (DW_TAG_lo_user+1)
#define DW_TAG_proctype_formal (DW_TAG_lo_user+2)
#define DW_TAG_fref (DW_TAG_lo_user+3)
#define DW_TAG_ignore (DW_TAG_lo_user+4)
#define DW_TAG_array_bound (DW_TAG_lo_user+5)
#define DW_TAG_procdecl (DW_TAG_lo_user+6)

extern Symstr *dw_debug_sym, *dw_lineinfo_sym;
extern Symstr *dw_macro_sym, *dw_abbrev_sym, *dw_location_sym;

#define Dwarf1DebugAreaName ".debug"
#define Dwarf1LineInfoAreaName ".line"
#define Dwarf2DebugAreaName ".debug_info"
#define Dwarf2LineInfoAreaName ".debug_line"

#define DebugAreaName (dw_version == 2 ? Dwarf2DebugAreaName : Dwarf1DebugAreaName)
#define LineInfoAreaName (dw_version == 2 ? Dwarf2LineInfoAreaName : Dwarf1LineInfoAreaName)
#define NameIndexAreaName ".debug_pubnames"
#define RangeIndexAreaName ".debug_arange"
/* The following in DWARF version 2 only */
#define MacroAreaName ".debug_macinfo"
#define AbbrevAreaName ".debug_abbrev"
#define LocationAreaName ".debug_loc"

typedef struct Dw_ItemList Dw_TypeRep;

typedef struct Dbg_Structelt Dbg_Structelt;
typedef struct Dbg_Enumelt Dbg_Enumelt;
typedef struct Dbg_Return Dbg_Return;

/* The following is the *internal* data structure in which debug info. */
/* is buffered.                                                        */
typedef struct Dw_ItemList Dw_ItemList;
typedef enum {
    Stg_Reg,
    Stg_Auto,
    Stg_Static,
    Stg_Extern,
    Stg_ArgReg,
    Stg_ArgAuto
} StgClass;

typedef struct {
    Symstr *sym;
    RealRegister r;
} SymOrReg;

struct Dw_ItemList {
    Dw_ItemList *cdr;
    int debsort;
    Dw_ItemList *sibling;
    uint32 dbgloc;
    union
    {  struct { Dw_ItemList *children;
                char const *name;
                Symstr *codeseg;
                int32 codesize;
              } DEB_SECTION;
       struct { Dw_ItemList *children;
                Dw_TypeRep *type;
                int32 entryaddr, bodyaddr;      /* see dbg_bodyproc */
                Dw_ItemList *endproc;
                int global;
                char const *name;
                Symstr *codeseg;
                bool variadic;
                SET_BITMAP stgbits;             /* StgClass inadequate! */
                Dw_TypeRep *parent;
                Dw_ItemList *decl;
              } DEB_PROC;
       struct { Dw_ItemList *children;
                Dw_TypeRep *type;
                int global;
                char const *name;
                bool variadic;
                int32 voffset;
                SET_BITMAP stgbits;             /* StgClass inadequate! */
                Dw_TypeRep *parent;
              } DEB_PROCDECL;
       struct { Dw_ItemList *qualtypes;
                Dw_ItemList *children;
                Dw_TypeRep *type;
              } DEB_PROCTYPE;
       struct { Dw_ItemList *qualtypes;
                Dw_TypeRep *container;
                Dw_TypeRep *type;
              } DEB_PTRTOMEMTYPE;
       struct { Symstr *name;
                Dw_TypeRep *type;
                Expr *defltexpr;
                Dw_ItemList *defltfn;
                bool invented;
              } DEB_FORMAL;
       struct { int32 dummy;
              } DEB_REST;
       struct { int32 endaddr;
              } DEB_ENDPROC;
       struct { Dw_TypeRep *type;
                StgClass stgclass;
                int32 location;
                Symstr *sym;
                SymOrReg base;
              } DEB_VAR;
       struct { Dw_ItemList *qualtypes;
                char *name;
                Dw_TypeRep *type;
                TypeExpr *typex;
              } DEB_TYPE;
       struct { Dw_ItemList *qualtypes;
                Dw_ItemList *nextbasetype;
                int typecode;
              } DEB_BASETYPE;
       struct { Dw_TypeRep *type;
                Dw_TypeRep *nextqual;   /* chain through qualified types of the same base */
                Dw_TypeRep *basetype;   /* all qualifiers stripped */
                Dw_TypeRep *qualifiedtype;  /* type with this qualifier removed */
                int n;
                uint8 *qualmap;
              } DEB_QUALTYPE;
       struct { Dw_ItemList *qualtypes;
                char const *name;
                Dw_ItemList *children;
                Dw_TypeRep *container;
                char size;
              } DEB_ENUM;
       struct { char *name;
                int32 val;
              } DEB_ENUMERATOR;
       struct { Dw_ItemList *qualtypes;
                Dw_ItemList *children;
                int32 size;
                int32 open;
                Dw_TypeRep *basetype;
                int32 lowerbound;
                int32 upperbound;
              } DEB_ARRAY;
       struct { int32 open;
                int32 upperbound;
              } DEB_ARRAYBOUND;
       struct { Dw_ItemList *qualtypes;
                char const *name;
                Dw_ItemList *children;
                int undefsort;
                  /* A not-yet-defined (but referenced) struct, class or   */
                  /* union is given sort TAG_fref, and its real sort is    */
                  /* stored in undefsort.                                  */
                int32 size;
                Friend *friends;
              } DEB_STRUCT;
       struct { char const *name;                 /* source form */
                int32 offset;
                Dw_TypeRep *type;
                uint8 bsize, boffset;
              } DEB_MEMBER;
       struct { Dw_TypeRep *type;
                int32 offset;
                uint8 virt;
              } DEB_INHERIT;
       struct { union { Dw_ItemList *next;
                        int32 codeaddr;
                      } s;
                Dw_ItemList *children;
                Dw_ItemList *end;
                Symstr *codeseg;
              } DEB_STARTSCOPE;
       struct { union { Dw_ItemList *next;
                        int32 codeaddr;
                      } s;
              } DEB_ENDSCOPE;
       struct { Dw_ItemList **parent;
              } DEB_NULL;
    } car;
};

#define debsort_(p) ((p)->debsort)
#define sibling_(p) ((p)->sibling)
#define dbgloc_(p) ((p)->dbgloc)

#define sect_children_(p) ((p)->car.DEB_SECTION.children)
#define sect_name_(p) ((p)->car.DEB_SECTION.name)
#define sect_codeseg_(p) ((p)->car.DEB_SECTION.codeseg)
#define sect_codesize_(p) ((p)->car.DEB_SECTION.codesize)

#define proc_children_(p) ((p)->car.DEB_PROC.children)
#define proc_type_(p) ((p)->car.DEB_PROC.type)
#define proc_entry_(p) ((p)->car.DEB_PROC.entryaddr)
#define proc_body_(p) ((p)->car.DEB_PROC.bodyaddr)
#define proc_endproc_(p) ((p)->car.DEB_PROC.endproc)
#define proc_global_(p) ((p)->car.DEB_PROC.global)
#define proc_name_(p) ((p)->car.DEB_PROC.name)
#define proc_codeseg_(p) ((p)->car.DEB_PROC.codeseg)
#define proc_variadic_(p) ((p)->car.DEB_PROC.variadic)
#define proc_stg_(p) ((p)->car.DEB_PROC.stgbits)
#define proc_parent_(p) ((p)->car.DEB_PROC.parent)
#define proc_decl_(p) ((p)->car.DEB_PROC.decl)

#define procdecl_children_(p) ((p)->car.DEB_PROCDECL.children)
#define procdecl_type_(p) ((p)->car.DEB_PROCDECL.type)
#define procdecl_global_(p) ((p)->car.DEB_PROCDECL.global)
#define procdecl_name_(p) ((p)->car.DEB_PROCDECL.name)
#define procdecl_variadic_(p) ((p)->car.DEB_PROCDECL.variadic)
#define procdecl_voffset_(p) ((p)->car.DEB_PROCDECL.voffset)
#define procdecl_stg_(p) ((p)->car.DEB_PROCDECL.stgbits)
#define procdecl_parent_(p) ((p)->car.DEB_PROCDECL.parent)

#define proctype_qual_(p) ((p)->car.DEB_PROCTYPE.qualtypes)
#define proctype_children_(p) ((p)->car.DEB_PROCTYPE.children)
#define proctype_type_(p) ((p)->car.DEB_PROCTYPE.type)

#define ptrtomem_container_(p) ((p)->car.DEB_PTRTOMEMTYPE.type)
#define ptrtomem_qual_(p) ((p)->car.DEB_PTRTOMEMTYPE.qualtypes)
#define ptrtomem_type_(p) ((p)->car.DEB_PTRTOMEMTYPE.container)

#define formal_name_(p) ((p)->car.DEB_FORMAL.name)
#define formal_type_(p) ((p)->car.DEB_FORMAL.type)
#define formal_defltexpr_(p) ((p)->car.DEB_FORMAL.defltexpr)
#define formal_defltfn_(p) ((p)->car.DEB_FORMAL.defltfn)
#define formal_invented_(p) ((p)->car.DEB_FORMAL.invented)

#define endproc_endaddr_(p) ((p)->car.DEB_ENDPROC.endaddr)

#define var_type_(p) ((p)->car.DEB_VAR.type)
#define var_stgclass_(p) ((p)->car.DEB_VAR.stgclass)
#define var_loc_(p) ((p)->car.DEB_VAR.location)
#define var_sym_(p) ((p)->car.DEB_VAR.sym)
#define var_base_(p) ((p)->car.DEB_VAR.base)

#define type_qual_(p) ((p)->car.DEB_TYPE.qualtypes)
#define type_name_(p) ((p)->car.DEB_TYPE.name)
#define type_type_(p) ((p)->car.DEB_TYPE.type)
#define type_typex_(p) ((p)->car.DEB_TYPE.typex)

#define basetype_qual_(p) ((p)->car.DEB_BASETYPE.qualtypes)
#define basetype_typecode_(p) ((p)->car.DEB_BASETYPE.typecode)
#define basetype_next_(p) ((p)->car.DEB_BASETYPE.nextbasetype)

#define qualtype_type_(p) ((p)->car.DEB_QUALTYPE.type)
#define qualtype_next_(p) ((p)->car.DEB_QUALTYPE.nextqual)
#define qualtype_basetype_(p) ((p)->car.DEB_QUALTYPE.basetype)
#define qualtype_qualifiedtype_(p) ((p)->car.DEB_QUALTYPE.qualifiedtype)
#define qualtype_n_(p) ((p)->car.DEB_QUALTYPE.n)
#define qualtype_map_(p) ((p)->car.DEB_QUALTYPE.qualmap)

#define enum_qual_(p) ((p)->car.DEB_ENUM.qualtypes)
#define enum_name_(p) ((p)->car.DEB_ENUM.name)
#define enum_children_(p) ((p)->car.DEB_ENUM.children)
#define enum_container_(p) ((p)->car.DEB_ENUM.container)
#define enum_size_(p) ((p)->car.DEB_ENUM.size)

#define enumerator_name_(p) ((p)->car.DEB_ENUMERATOR.name)
#define enumerator_val_(p) ((p)->car.DEB_ENUMERATOR.val)

#define array_qual_(p) ((p)->car.DEB_ARRAY.qualtypes)
#define array_children_(p) ((p)->car.DEB_ARRAY.children)
#define array_size_(p) ((p)->car.DEB_ARRAY.size)
#define array_open_(p) ((p)->car.DEB_ARRAY.open)
#define array_basetype_(p) ((p)->car.DEB_ARRAY.basetype)
#define array_lowerbound_(p) ((p)->car.DEB_ARRAY.lowerbound)
#define array_upperbound_(p) ((p)->car.DEB_ARRAY.upperbound)

#define arraybound_open_(p) ((p)->car.DEB_ARRAYBOUND.open)
#define arraybound_upperbound_(p) ((p)->car.DEB_ARRAYBOUND.upperbound)

#define struct_qual_(p) ((p)->car.DEB_STRUCT.qualtypes)
#define struct_name_(p) ((p)->car.DEB_STRUCT.name)
#define struct_children_(p) ((p)->car.DEB_STRUCT.children)
#define struct_undefsort_(p) ((p)->car.DEB_STRUCT.undefsort)
#define struct_size_(p) ((p)->car.DEB_STRUCT.size)
#define struct_friends_(p) ((p)->car.DEB_STRUCT.friends)

#define member_name_(p) ((p)->car.DEB_MEMBER.name)
#define member_offset_(p) ((p)->car.DEB_MEMBER.offset)
#define member_type_(p) ((p)->car.DEB_MEMBER.type)
#define member_bsize_(p) ((p)->car.DEB_MEMBER.bsize)
#define member_boffset_(p) ((p)->car.DEB_MEMBER.boffset)

#define inherit_type_(p) ((p)->car.DEB_INHERIT.type)
#define inherit_offset_(p) ((p)->car.DEB_INHERIT.offset)
#define inherit_virt_(p) ((p)->car.DEB_INHERIT.virt)

#define startscope_next_(p) ((p)->car.DEB_STARTSCOPE.s.next)
#define startscope_codeaddr_(p) ((p)->car.DEB_STARTSCOPE.s.codeaddr)
#define startscope_children_(p) ((p)->car.DEB_STARTSCOPE.children)
#define startscope_end_(p) ((p)->car.DEB_STARTSCOPE.end)
#define startscope_codeseg_(p) ((p)->car.DEB_STARTSCOPE.codeseg)

#define endscope_next_(p) ((p)->car.DEB_ENDSCOPE.s.next)
#define endscope_codeaddr_(p) ((p)->car.DEB_ENDSCOPE.s.codeaddr)

#define null_parent_(p) ((p)->car.DEB_NULL.parent)

#define is_void_type_(t) \
    (debsort_(t) == DW_TAG_base_type && basetype_typecode_(t) == FT_void)

#define high_pc_(p)     endproc_endaddr_(proc_endproc_(p))

extern Dw_ItemList *dw_list, *dw_listproc;
extern Dw_ItemList *dw_basetypes;

typedef struct {
    Uint index;  /* of the file in filelist */
    uint16 l;
    uint16 column;
} Dw_FileLine;

typedef struct Dw_MacroList Dw_MacroList;
struct Dw_MacroList {
    Dw_MacroList *cdr;
    int sort;
    Dw_FileLine fl;
    union {
        char const *s;
        Uint i;
    } data;
};

extern Dw_MacroList *dw_macrolist;
typedef struct Dw_LocList Dw_LocList;
struct Dw_LocList
{   Dw_LocList *cdr;
    Binder *name;
    int32 pos;
    int32 size;
    Dw_TypeRep *typeref;
};

extern Dw_LocList *dw_loclist;

/* Structs for buffering file/line co-ordinates.                        */
/* We have one of these for every file we see.  BUT because a file may  */
/* validly be included more than once in a C program, we had better do  */
/* pointer, not string, equality on names, and rely on pp.c behaviour.  */

typedef struct Dw_FileCoord Dw_FileCoord;
typedef struct Dw_FileList Dw_FileList;
typedef struct Dw_PathList Dw_PathList;

struct Dw_PathList {
    Dw_PathList *cdr;
    Uint index;
    size_t len;
    char name[1];
};

struct Dw_FileList {
    Dw_FileList *cdr;
    char const *filename;
    Uint index;
    Uint lastline;
    Dw_FileCoord *linelist;
    Dw_PathList *dir;
    uint32 timestamp;
    uint32 filelength;
};

struct Dw_FileCoord {
    Dw_FileCoord *cdr;
    Dw_FileList *file; Dw_FileCoord *nextinfile;
    uint16 line, col;
    int32 codeaddr;
    Symstr *codeseg;
};

extern struct Dw_BaseSeg {
    Symstr *sym;
    int32 len;
} dw_baseseg;

extern Dw_FileList *dw_filelist;
extern Dw_PathList *dw_pathlist;
/* The next two vars are (code order) list & tail pointer */
extern Dw_FileCoord *dw_coord_p, **dw_coord_q;
extern Dw_FileCoord dw_coord_sentinel;

extern int32 dw_mapped_codebase, dw_mapped_codep;

uint32 Dw_WriteInlineString(char const *s, uint32 offset);
uint32 Dw_WriteB(Uint u, uint32 offset);
uint32 Dw_WriteH(uint32 u, uint32 offset);
uint32 Dw_WriteW(uint32 u, uint32 offset);
uint32 Dw_WriteL(uint32 const *d, uint32 offset);
uint32 Dw_WriteBN(uint8 const *p, int32 n, uint32 offset);

extern DataXref *dw_xrefs;
DataXref *Dw_Relocate(DataXref *xrefs, int32 where, Symstr const *sym);
uint32 Dw_WriteW_Relocated(uint32 u, Symstr const *sym, uint32 offset);

char const *Dw_Unmangle(char const *s);

void Dw_1_WriteInfo(void);
void Dw_1_WriteLineinfo(void);

void Dw_2_WriteMacros(void);
void Dw_2_WriteInfo(void);
void Dw_2_WriteAbbrevs(void);
void Dw_2_WriteLineinfo(void);

