/*
 * C compiler file mcdep.h
 * Copyright (C) Acorn Computers Ltd., 1988.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$ Codemist 4
 * Checkin $Date$
 * Revising $Author$
 */

/*
 * (Interface for target-dependent part of local code generation).
 */

#ifndef _mcdep_h
#define _mcdep_h 1

#ifndef _globals_LOADED
  #error include "mcdep.h" out of place
  /* We need globals.h loaded first (host/options/target/defaults).     */
#endif
#ifndef _cgdefs_LOADED
#  include "cgdefs.h"
#endif
#ifndef _jopcode_LOADED
#  include "jopcode.h"
#endif
#include "toolenv.h"

/***************************************************************************/
/*                Interface to the target code generator                   */
/***************************************************************************/

#ifdef TARGET_HAS_FP_LITERALS
#  ifndef fpliteral
  extern bool fpliteral(FloatCon const *val, J_OPCODE op);
  /* Returns true if the argument value can be represented as a literal operand
   * in the expansion of the argument operation (passing this allows for the
   * possibility of changing the operation, eg ADD to SUB, to allow use of a
   * literal).
   */
#  endif
#endif

#ifndef immed_cmp
   extern bool immed_cmp(int32 n);
   /* False if this integer is to be a candidate to be held in a register through
    * loop optimisation.
    */
#endif

#ifndef immed_op
  /* target.h may define this otherwise, to return 1 if n may be an immediate
   * operand in the expansion of J_OPCODE op. If the value is 0, CSE is more
   * enthusiastic about making a load of the value into a CSE or hoisting it
   * out of loops.
   * immed_cmp is a special case, sensibly defined as immed_op(n, J_CMPK) where
   * immed_op has a non-default value.
   */
#  define immed_op(n, op) 1
#endif

#ifndef TARGET_DOESNT_CHECK_SWIS
int32 CheckSWIValue(int32);
#else
#define CheckSWIValue(n) (n)
#endif

#ifdef TARGET_INLINES_MONADS
extern int32 target_inlinable(Binder const *b, int32 nargs);
#endif

#ifndef alterscc
#define alterscc(ic) (sets_psr(ic) || corrupts_psr(ic))
#endif
extern bool sets_psr(Icode const *ic);
extern bool reads_psr(Icode const *ic);
extern bool uses_psr(Icode const *ic);
extern bool corrupts_psr(Icode const *ic);

extern bool corrupts_r1(Icode const* ic);
extern bool corrupts_r2(Icode const* ic);

extern bool has_side_effects(Icode const *ic);
extern void remove_writeback(Icode *ic);

extern RealRegister local_base(Binder const *b);
/* Returns the base register to be used in accessing the object b (must be
 * local).
 */

extern int32 local_address(Binder const *b);
/* Returns the offset from local_base(b) to be used in accessing object b.
 */

extern void setlabel(LabelNumber *l);
/* Sets the argument label at the current code position.
 * Although the idea of setlabel is target independent, it is in the mc-dep
 * interface because it back-patches code.
 * In the long term it should be in codebuf.c and call a target dependent
 * backpatch routine.
 */

extern void branch_round_literals(LabelNumber *m);

/* Encouraged by a comment that only ARM used mustlitby, it has been removed
   from the codebuf interface, and the initialisations of it which used to
   happen in codebuf.c turned into calls of the following function. Targets
   which don't care will just refrain from setting localcg_newliteralpool_exists
 */
#ifdef localcg_newliteralpool_exists
extern void localcg_newliteralpool(void);
#else
#define localcg_newliteralpool() 0
#endif
#ifdef TARGET_HAS_MULTIPLE_CODE_AREAS
void localcg_endcode(void);
#endif

extern void show_instruction(Icode const *const ic);

extern void localcg_reinit(void);       /* under threat (use J_ENTER)   */
extern void localcg_tidy(void);         /* ditto (use J_ENDPROC)        */

typedef struct
{
    RealRegSet use,     /* registers read */
               def,     /* registers written */
               c_in,    /* registers corrupted on entry (clash with input regs) */
               c_out;   /* registers corrupted on exit (clash with output regs) */
}   RealRegUse;

/* returns the physical register usage of ic */
extern void RealRegisterUse(Icode const *ic, RealRegUse *u);

#ifdef TARGET_IS_ARM_OR_THUMB
bool UnalignedLoadMayUse(RealRegister r);
  /* A special purpose version of RealRegisterUse, but called before it's   */
  /* known what Icode the load is going to expand into.                      */
#endif

/* Return the minimum/maximum offset a certain load/store can have (inclusive).
   Larger offsets require either IP to be reserved or a seperate base pointer.
 */
extern int32 MinMemOffset(J_OPCODE op);
extern int32 MaxMemOffset(J_OPCODE op);

/* Return the granularity of memory addresses for op. */
extern int32 MemQuantum(J_OPCODE op);

typedef enum
{
    JCHK_MEM = 1, JCHK_REGS = 2,
    JCHK_SYSERR = 256
}   CheckJopcode_flags;


extern char *CheckJopcode(const Icode *ic, CheckJopcode_flags flags);

extern int multiply_cycles(int val, bool accumulate); /* return #cycles for a multiply */

/***************************************************************************/
/*                Interface to the object code formatter                   */
/***************************************************************************/

extern void obj_codewrite(Symstr *name);
/* (name == 0 && codep == 0) => called from just after the creation of a  */
/*                new code segment. bindsym_(codesegment) is the name of  */
/*                a symbol to be set at the base of the area (not         */
/*                the area name)                                          */
/* (name == 0 && codep != 0) => called generation of a string literal     */
/* (name != 0) => called after generation of code for a function          */

extern void obj_init(void);

extern void obj_header(void);

extern void obj_trailer(void);

extern void obj_common_start(Symstr *name);

extern void obj_common_end(void);

#define obj_setcommoncode() (var_cc_private_flags |= 0x40000000) /* set COMDEF attribute */
#define obj_clearcommoncode() (var_cc_private_flags &= ~0x40000000)
#define obj_iscommoncode() (var_cc_private_flags & 0x40000000)

#define obj_setvtable() (var_cc_private_flags |= 0x20000000) /* set COMDEF attribute */
#define obj_clearvtable() (var_cc_private_flags &= ~0x20000000)
#define obj_isvtable() (var_cc_private_flags & 0x20000000)

#ifdef TARGET_HAS_AOUT
extern void obj_stabentry(struct nlist const *p);
#endif

#ifndef NO_ASSEMBLER_OUTPUT
/***************************************************************************/
/*                Interface to the assembly code generator                 */
/***************************************************************************/

extern void asm_header(void);
extern void asm_trailer(void);
extern void asm_setregname(int regno, char const *name);

extern void display_assembly_code(Symstr const *);
/* Entry conditions as for obj_codewrite.                                 */

#endif

/***************************************************************************/
/*                Interface to the debug table generator                   */
/***************************************************************************/

#define DBG_LINE 1           /* line info -- reduces peepholing     */
#define DBG_PROC 2           /* top level info -- no change to code */
#define DBG_VAR  4           /* local var info -- no leaf procs     */
#define DBG_PP   8

#define DBG_OPT_CSE  0x10
#define DBG_OPT_REG  0x20
#define DBG_OPT_DEAD 0x40

#define DBG_OPT_ALL  (DBG_OPT_CSE|DBG_OPT_REG|DBG_OPT_DEAD)

#ifdef TARGET_DEBUGGER_WANTS_MACROS
#  define DBG_ANY (DBG_LINE|DBG_PROC|DBG_VAR|DBG_PP)
#else
#  define DBG_ANY (DBG_LINE|DBG_PROC|DBG_VAR)
#endif

#ifdef TARGET_HAS_DEBUGGER

/* Language independent type number, to be passed to dbg_tableindex */
#define DT_QINT      1
#define DT_HINT      2
#define DT_INT       3
#define DT_LINT      4
#define DT_UQINT     5
#define DT_UHINT     6
#define DT_UINT      7
#define DT_ULINT     8
#define DT_FLOAT     9
#define DT_DOUBLE   10
#define DT_EXTENDED 11
#define DT_COMPLEX  12
#define DT_LCOMPLEX 13
#define DT_QBOOL    14
#define DT_HBOOL    15
#define DT_BOOL     16
#define DT_LBOOL    17
#define DT_CHAR     18
#define DT_UCHAR    19
#define DT_MAX      19

  extern char dbg_name[];
  extern int usrdbgmask;
#  define usrdbg(DBG_WHAT) (usrdbgmask & (DBG_WHAT))
  extern int32 dbg_tablesize(void);
  extern int32 dbg_tableindex(int32 dt_number);
  extern void *dbg_notefileline(FileLine fl);
  extern void dbg_addcodep(void *debaddr, int32 codeaddr);
  extern bool dbg_scope(BindListList *, BindListList *);
  extern void dbg_final_src_codeaddr(int32, int32);


#  ifdef DEBUGGER_NEEDS_NO_FRAMEPOINTER
  extern bool dbg_needsframepointer(void);
#  else
#    define dbg_needsframepointer() 1
#  endif


#  ifdef TARGET_HAS_BSS
#    define DS_EXT 1  /* bits in stgclass argument of dbg_topvar */
#    define DS_BSS 2
#    define DS_CODE 4
#    define DS_REG 8
#    define DS_UNDEF 16
    extern void dbg_topvar(Symstr *name, int32 addr, TypeExpr *t, int stgclass,
                           FileLine fl);
#  else
    extern void dbg_topvar(Symstr *name, int32 addr, TypeExpr *t, bool ext,
                           FileLine fl);
#  endif
  extern void dbg_type(Symstr *name, TypeExpr *t, FileLine fl);
#ifndef NEW_DBG_PROC_INTERFACE
  extern void dbg_proc(Symstr *name, TypeExpr *t, bool ext, FileLine fl);
#else
  extern void dbg_proc(Binder *b, TagBinder *parent, bool ext, FileLine fl);
#endif
  extern void dbg_locvar(Binder *name, FileLine fl);
  extern void dbg_locvar1(Binder *name);   /* used by F77 front-end */
  extern void dbg_commblock(Binder *name, SynBindList *members, FileLine fl);
  extern void dbg_enterproc(void);
  extern void dbg_bodyproc(void);
  extern void dbg_return(int32 addr);
  extern void dbg_xendproc(FileLine fl);
#  ifdef TARGET_DEBUGGER_WANTS_MACROS
  typedef struct dbg_ArgList dbg_ArgList;
  struct dbg_ArgList {
      char const *name;
      dbg_ArgList *next;
  };
  extern void dbg_define(char const *name, bool objectmacro, char const *body,
                         dbg_ArgList const *args, FileLine fl);
  extern void dbg_undef(char const *name, FileLine fl);
  extern void dbg_include(char const *filename, char const *path, FileLine fl);
  extern void dbg_notepath(char const *pathname);
#  else
#    define dbg_undef(a, b)
#    define dbg_define(a, b, c, d, e)
#    define dbg_include(a, b, c)
#    define dbg_notepath(a)
#  endif
  extern void dbg_init(void);
#else
#  define usrdbg(DBG_WHAT)               ((int)0)
#  define dbg_tablesize()                ((int32)0)
#  define dbg_tableindex(a)              ((int32)0)
#  define dbg_notefileline(a)            ((void *)0)
#  define dbg_init()                     ((void)0)
#  define dbg_scope(a,b)                 ((bool)0)
#  define dbg_addcodep(debaddr,codeaddr) ((void)0)
#  define dbg_enterproc()                ((void)0)
#  define dbg_bodyproc()                 ((void)0)
#  define dbg_return(a)                  ((void)0)
#  define dbg_xendproc(a)                ((void)0)
#  define dbg_type(a,b,c)                ((void)0)
#  define dbg_proc(a,b,c,d)              ((void)0)
#  define dbg_topvar(a,b,c,d,e)          ((void)0)
#  define dbg_locvar(a,b)                ((void)0)
#  define dbg_locvar1(a)                 ((void)0)
#  define dbg_commblock(a, b, c)         ((void)0)
#  define dbg_undef(a, b)                ((void)0)
#  define dbg_define(a, b, c, d, e)      ((void)0)
#  define dbg_include(a, b,c)            ((void)0)
#  define dbg_notepath(a)                ((void)0)
#  define dbg_finalise()                 ((void)0)
#  define dbg_setformat(a)               ((void)0)
#  define dbg_debugareaexists(a)         ((bool)0)
#  define dbg_final_src_codeaddr(a, b)   ((void)0)
#  define dbg_needsframepointer()        0
#endif

/***************************************************************************/
/* Interface between debug table generator, object formatter and target    */
/*                          code generator                                 */
/***************************************************************************/

#ifdef TARGET_HAS_DEBUGGER

#include "xrefs.h"

int32 local_fpaddress(Binder const *b);
/* Returns the offset of the object from the fp (assuming that fp and sp
 * have not been split in the frame containing the object ...).
 */

#define R_NOFPREG ((RealRegister)-1)

RealRegister local_fpbase(Binder const *b);

void dbg_writedebug(void);
/* Call from the object formatter to the debug table generator to
 * cause tables to be output
 */

void obj_writedebug(void const *, int32);

#define DBG_INTFLAG 0x80000000L
/* flag in the size argument to obj_writedebug indicating that the things
 * being written are 4-byte ints (to be byte reversed if appropriate);
 */
#define DBG_SHORTFLAG 0x40000000L
/* flag in the size argument to obj_writedebug indicating that the things
 * being written are 2-byte ints (to be byte reversed if appropriate);
 * If neither flag is present, they are byte strings to be written as
 * presented.
 */

void dbg_finalise(void);

#  ifdef TARGET_HAS_FP_OFFSET_TABLES

typedef struct FPList FPList;
typedef struct {
  FPList *fplist;
  int32 startaddr, endaddr, saveaddr;
  Symstr *codeseg;
  int32 initoffset;
} ProcFPDesc;

struct FPList {
  FPList *cdr;
  int32 addr;
  int32 change;
};

void obj_notefpdesc(ProcFPDesc const *);

#  endif

void dbg_setformat(char const *format);

bool dbg_debugareaexists(char const *name);

Symstr *obj_notedebugarea(char const *name);
void obj_startdebugarea(char const *name);
void obj_enddebugarea(char const *name, DataXref *relocs);

#endif

/***************************************************************************/
/*                Target-dependent argument processing                     */
/***************************************************************************/

typedef enum { KW_NONE, KW_OK, KW_MISSINGARG, KW_BADARG, KW_OKNEXT, KW_BADNEXT } KW_Status;

void mcdep_init(void);

#ifdef TARGET_HAS_DATA_VTABLES
bool mcdep_data_vtables(void);
#endif


#ifdef TARGET_SUPPORTS_TOOLENVS
int mcdep_toolenv_insertdefaults(ToolEnv *t);
extern KW_Status mcdep_keyword(char const *key, char const *nextarg, ToolEnv *t);
void config_init(ToolEnv *t);
void mcdep_set_options(ToolEnv *);
#ifdef CALLABLE_COMPILER
  #define mcdep_config_option(n,t,e)      ((bool)0)
#else
  bool mcdep_config_option(char name, char const tail[], ToolEnv *t);
#endif

#else
extern KW_Status mcdep_keyword(char const *key, int *argp, char **argv);
void config_init(void);
#  ifdef CALLABLE_COMPILER
  #define mcdep_config_option(n,t)        ((bool)0)
#  else
  bool mcdep_config_option(char name, char tail[]);
  void mcdep_debug_option(char name, char tail[]);
#  endif
#endif

#endif

/* end of mcdep.h */
