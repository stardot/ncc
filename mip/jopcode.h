/*
 * file mip/jopcode.h
 * Copyright (C) Codemist Ltd., 1989.
 * Copyright 1991-1997 Advanced Risc Machines Limited. All rights reserved
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$ Codemist 37
 * Checkin $Date$
 * Revising $Author$
 */

/*
 * Some bits of information now live in the joptable[] array, while things
 * like J_DEAD_R1 still live in the main code, since they need to be set
 * on a per-instruction basis.
 */

#ifndef _jopcode_LOADED
#define _jopcode_LOADED 1

#ifndef _defs_LOADED
#  include "defs.h"
#endif

/* The following are in for compatibility with old back ends only.      */
#define J_MOVFR1 J_MOVIFR
#define J_MOVDR1 J_MOVIDR
#define J_double(op) J_fltisdouble(op)

/* Opcodes in the intermediate code that I generate.                    */
/* Idea -- the things beginning with '_' are local to this file, but... */
/* ... strictly speaking this contravenes ANSI's rules on '_' use.      */

/* The following bits give 'global' (not per-use) properties of         */
/* J_opcodes.  The J_opcodes themselves are an enumeration below.       */
/* _J_OPTYPE field must be LS bits since it is used as index.           */
#define _J_OPTYPE     0x000fL   /* Used just for LDR/STR opcodes (MEM_xxx) */
#define _J_READ_R1    0x0010L
#define _J_SET_R1     0x0020L
#define _J_READ_R2    0x0040L
#define _J_SET_R2     0x0080L   /* only for MOVDIR/MOVLIR, arriving...  */
#define _J_READ_R3    0x0100L   /* r3 field is a register rather than literal */
#define _J_REGMOVE    0x0200L
#define _J_ASYMDIAD   0x0400L
#define _J_READ_R4    0x0800L
/* the next few bits are regarded JOP adressing modes.                      */
/* N.B. read the BEWARE on LDRK before using                                */
#define _J_MEM        0x1000L   /* distinguishes mem ref from op            */
#define _J_MEMA       0x3000L   /* address of memory - really _J_MEM+0x2000 */
/* *** N.B. but for LDRV1 we could arrange that _J_MEMA was 0x104 ???       */
#define _J_STACKREF   0x4000L
#define _J_IVD        0x8000L   /* indexed var + disp (only with _J_STACKREF) */
#define _J_GAP1     0x010000L   /* r1 field always unused (printing)    */
#define _J_GAP2     0x020000L   /* r2 field always unused (printing)    */
/* @@@ _J_FLOATING/_J_DOUBLE are dying -- only used in 'J_fltisdouble'  */
/* @@@ 'floatiness()' and the latter is dying -- see cse.c              */
/* It is arguable that these could be J_destregsort(op)->INTREG &c?        */
#define _J_FLOATING 0x040000L   /* overlaps use of MEM_F!                  */
#define _J_DOUBLE   0x080000L   /* overlaps use of MEM_D!                  */
#define _J_PSEUDO_READS_R2 0x00100000L
#define _J_COMMUTATIVE 0x00200000L
#define _J_MEMW     0x80000000L /* writes to memory                        */
#define _J_MEMR     0x40000000L /* reads from memory                       */

/*
 * The above bits are for joptable[], those which follow are part of the main
 * opcode, but must not be used when indexing into joptable[].
 */

#define J_TABLE_BITS 0x1ff  /* Room for 500 distinct jopcodes */
#define _joptable(o) (joptable[(o) & J_TABLE_BITS].bits)

#define J_BASEALIGN4     0x0200L
#define J_VOLATILE       0x0400L /* replacing J_USE/J_VSTORE, only LDRx/STRx */
/* N.B. The compiler currently does not represent 'volatile' on J_MOVC      */
/* because large structs are not subject to dataflow analysis.              */
/* Maybe it should, and hence maybe there should be two J_VOLATILE bits.    */
#define J_DEAD_R1        0x0800L  /* for dataflow analysis                  */
#define J_DEAD_R2        0x1000L  /* for dataflow analysis                  */
#define J_DEAD_R3        0x2000L  /* for dataflow analysis                  */
#define J_DEAD_R4        0x4000L  /* for dataflow analysis                  */
#define J_DEADBITS (J_DEAD_R1+J_DEAD_R2+J_DEAD_R3+J_DEAD_R4)

#if defined TARGET_HAS_SCALED_ADDRESSING || defined TARGET_HAS_SCALED_OPS || \
    defined TARGET_HAS_SCALED_ADD
#  define J_NEGINDEX     0x8000L /* currently or-able with load/store only */
#  define J_SHIFTMASK  0xff0000L
#  define J_SHIFTPOS         16
#    define SHIFT_MASK     0x3fL
#    define SHIFT_RIGHT    0x80L
#    define SHIFT_ARITH    0x40L
#endif
#define J_ALIGNMENT  0x03000000L       /* May 92 experiment.               */
#define J_ALIGN1     0x00000000L       /* May 92 experiment.               */
#define J_ALIGN2     0x01000000L       /* May 92 experiment.               */
#define J_ALIGN4     0x02000000L       /* May 92 experiment.               */
#define J_ALIGN8     0x03000000L       /* May 92 experiment.               */
#define J_ALIGNPOS    24
#define j_aligned(op,align) (((op) & J_ALIGNMENT) >= (align))
#define J_SIGNED     0x04000000L /* orable with LDRBx, LDRWx, also fix/float */
#define J_UNSIGNED   0x08000000L /* ditto */
#define J_ALIGNWR    J_SIGNED    /* => J_STRxx may write to J_ALIGN padding  */

/* type bits (reallocate?) ... n.b. Q_UBIT shares with J_UNSIGNED */

/*
 * The next few macros represent all the places where arithmetic is done
 * on jopcodes to map between them, and hence these lines imply all the
 * constraints that there are on the ordering of jopcodes within the
 * enumeration:
 * 1. When an instruction occurs in both R and K formats, the codes for the
 *    two must be adjacent.  E.g. ADDK then ADDR.
 * 2. The J_LDRVK forms loads and stores must immediately follow the RR form.
 * 3. The J_LDRxx and J_STRxx opcodes are parallel (for J_LDtoST()).
 * 4. The J_LDRV1 (I/F/K), J_MOVK (I/F/K), J_PUSH (I/F/K) are parallel (for
 *    flowgraf.c via J_XtoY()).
 */
#define J_RTOK(a) ((a) - 1)
#define J_KTOR(a) ((a) + 1)
#define J_addvk(a) ((a) + 2)
#define J_subvk(a) ((a) - 2)
#define J_KtoV(a) ((a) + 3)     /* = J_XtoY(a, J_LDRK, J_LDRVK) etc.    */
#define J_XtoY(a,x,y) ((a) + ((y)-(x)))
#define J_LDtoST(a) ((a) + (J_STRK-J_LDRK))
#define J_V1toV(a) ((a) + (J_LDRV-J_LDRV1))
/* It would be nice to arrange J_LDRxK = J_LDRBK + 4*MEM_x too.         */
#define tailify_(a)   ((a) + (J_TAILCALLK-J_CALLK))
/*
 * TAILCALL & CALL separation does not impose any extra constraints here.
 */
/* auxiliary bits in the r2 field of OPSYSK and CALLK/R...              */
#define K_ARGWORDMASK   0x00000fffL
#define K_FLAGS         (~0x00ffffffL)
#define K_VACALL        0x01000000L /* call to variadic function        */
#define K_VAFUNC        K_VACALL    /* function is variadic             */
#define K_PURE          0x02000000L /* replaces J_CALLP                 */
#define K_SPECIAL_ARG   0x04000000L /* extra arg in an odd register     */
#define K_INLINE        0x08000000L /* backend intends special expansion */
/* ECN: Use to mark a J_CALLK which is actually an entry in a Thunk table */
#define K_THUNK         0x10000000L /* C++ virtual function call        */
/* ECN: Do not tailcall this call - hack for use with pSOS - used to    */
/* improve debugability of system calls under pROBE                     */
#define K_NOTAILCALL    0x20000000L /* do not tailcall this call        */
#define K_COMMUTATIVE   0x40000000L
#define K_RESULTINFLAGS 0x80000000L /* returns result in flags only     */

#define k_argwords_(n) ((n)&K_ARGWORDMASK)
#define k_resultregs_(n) (((n)>>12)&15) /* struct result regs, 0 for function or procedure!!!! */
#define k_setresultregs_(n, k) (((n) & (~0x0000f000L)) | ((k) << 12))
#define k_setflags_(n, k) (((n) & ~K_FLAGS) | (k))
#define k_isvariadic_(n) ((n) & K_VACALL)
#define k_ispure_(n)     ((n) & K_PURE)
#define k_iscommutative_(n) ((n) & K_COMMUTATIVE)

#define iscalln_(op, n) (isproccall_(op) && k_resultregs_(n) > 1)

#ifdef TARGET_FP_ARGS_IN_FP_REGS        /* TARGET_FP_ARGS_CALLSTD1      */
#define k_argisfp_(n,i) ((i)<k_fltregs_(n))
#define k_intregs_(n) (((n)>>20)&15)
#define k_fltregs_(n) (((n)>>16)&15)
#define k_argregs_(n) (k_intregs_(n)+k_fltregs_(n))
#define k_argdesc_(aw, fm, ni, nf, nres, f)  (((aw) >= 4095 ? 4095 : (aw)) | (f) | \
        (((unsigned32)ni)<<20) | (((unsigned32)nf)<<16) | ((nres)<<12))
#else  /* TARGET_FP_ARGS in integer registers only                     */
# define k_fltregs_(n) 0
#define k_argisfp_(n,i) 0
#define k_argregs_(n) (k_argwords_(n) < NARGREGS ? k_argwords_(n) : NARGREGS)
#define k_argdesc_(aw, fm, ni, nf, nres, f) \
        ((aw) | (f) | ((nres)<<12))
#endif

/*
 * Now macros which test characteristics of codes.  In some cases this is
 * currently implemented as sets of tests where allocating extra bits in
 * joptable may allow conversion to simple bit testing.
 */
#define register_movep(a) (_joptable(a) & _J_REGMOVE)
#define floatiness_(a) (_joptable(a) & (_J_DOUBLE+_J_FLOATING))
#define J_fltisdouble(a) (_joptable(a) & _J_DOUBLE)
#define vkformat(a) ((_joptable(a) & _J_MEM+_J_IVD) == _J_MEM+_J_IVD)
#define gap_r1(a) (_joptable(a) & _J_GAP1)
#define loads_r1(a) (_joptable(a) & _J_SET_R1)
#define reads_r1(a) (_joptable(a) & _J_READ_R1)
#define uses_r1(a)  (_joptable(a) & (_J_SET_R1 | _J_READ_R1))
#define updates_r1(a) ((_joptable(a) & (_J_READ_R1 | _J_SET_R1)) == (_J_READ_R1 | _J_SET_R1))

/* The following test could easily be indicated by SET_R1+READ_R1 but   */
/* currently various parts of the compiler assumes they are disjoint.   */
/* Merging in certain J_MOVC cases seems indicated here too.            */

/* Peter Armistead - remove for CC */
#ifndef CALLABLE_COMPILER
#ifndef TARGET_IS_ARM_OR_THUMB      /* ARM/Thumb defines corrupts_r1/r2 in mcdep */
#ifdef TARGET_CORRUPTS_SWITCH_REGISTER
#  define corrupts_r1(a) ((a)->op == J_CASEBRANCH)
#else
#  define corrupts_r1(a) 0
#endif
#define corrupts_r2(a) 0
#endif
#endif /* ifndef CALLABLE_COMPILER */

#define stores_r1(a) ((_joptable(a) & (_J_MEM | _J_READ_R1)) == (_J_MEM | _J_READ_R1))
#define gap_r2(a) (_joptable(a) & _J_GAP2)
#define loads_r2(a) (_joptable(a) & _J_SET_R2)
#define reads_r2(a) (_joptable(a) & _J_READ_R2)
#define uses_r2(a)  (_joptable(a) & (_J_SET_R2 | _J_READ_R2))
#define updates_r2(a) ((_joptable(a) & (_J_READ_R2 | _J_SET_R2)) == (_J_READ_R2 | _J_SET_R2))
#define reads_r3(a) (_joptable(a) & _J_READ_R3)
#define uses_r3(a)  reads_r3(a)
#define reads_r4(a) (_joptable(a) & _J_READ_R4)
#define uses_r4(a)  reads_r4(a)
#define uses_stack(a) ((_joptable(a) & _J_STACKREF+_J_MEM) == _J_STACKREF+_J_MEM)

#define reads_mem(a) (_joptable(a) & _J_MEMR)
#define writes_mem(a) (_joptable(a) & _J_MEMW)
#define j_is_ldr_or_str(a) ((_joptable(a) & (_J_MEM | _J_MEMA)) == _J_MEM)
#define j_is_commutative(a) (_joptable(a) & _J_COMMUTATIVE)

/* WD: TODO - make function of isproccall - include SWI!!! */

#ifdef TARGET_COUNT_IS_PROC
#  define isproccall_(op) ((op) == J_CALLK || (op) == J_OPSYSK || \
                           (op) == J_CALLR || (op) == J_COUNT)
#else
#  define isproccall_(op) ((op) == J_CALLK || (op) == J_OPSYSK || \
                           (op) == J_CALLR)
#endif
#ifdef TARGET_HAS_2ADDRESS_CODE
#  define jop_asymdiadr_(op) \
   ((_joptable(op) & _J_ASYMDIAD+_J_READ_R3+_J_MEM) == _J_ASYMDIAD+_J_READ_R3)
/* maybe insert an is_anydiadr() here, but this think w.r.t. ADDK/ADDR and  */
/* possible uses of load address target instructions.                       */
#endif
#define j_is_adcon(a) ((_joptable(a) & _J_MEMA) == _J_MEMA)
#define j_is_diadr(a) ((_joptable(a) & (_J_MEM | _J_READ_R4 | _J_READ_R3 |\
                       _J_READ_R2 | _J_READ_R1)) ==\
                       (_J_READ_R3 | _J_READ_R2))
#define j_is_diadk(a) ((_joptable(a) & (_J_MEM | _J_READ_R4 | _J_READ_R3 |\
                       _J_READ_R2 | _J_READ_R1)) ==\
                       (_J_READ_R2))

/* Note: is-diadicx does not imply sets_r1 */

/* The following two macros are only used by jopprint.c and regalloc.c --   */
/* this is important since they are not overkeen about masking Q_MASK or    */
/* J_DEADBITS.  Note that they must correspond to the loop optimisation     */
/* code in loopopt.c and its friends in flowgraf.c                          */
/* Note that J_FNCON does not need considering as only this is only used    */
/* in the interface to xxxgen.c from flowgraf.c (q.v.)                      */
#ifdef RANGECHECK_SUPPORTED
#  define pseudo_reads_r1(a) (((a) & ~Q_MASK) == J_CMPK ||\
                              (a)==J_CHKLK || (a)==J_CHKUK || (a)==J_CHKNEK)
#  define j_is_check(op) \
    ((J_CHKLK<=(op) && (op)<=J_CHKUR) || (J_CHKNEFK<=(op) && (op)<=J_CHKNER))
#else
#  define pseudo_reads_r1(a) (((a) & ~Q_MASK) == J_CMPK)
#  define j_is_check(op) 0
#endif
#define pseudo_reads_r2(a) (_joptable(a) & _J_PSEUDO_READS_R2)

/* The following give machine types of store reference in LD/STxxx.     */
#define    MEM_B 0L
#define    MEM_W 1L
#define    MEM_I 2L     /* 32 bits -- avoid suffix L for a while.       */
#define    MEM_F 3L
#define    MEM_D 4L
#define    MEM_LL 5L    /* 64 bits -- avoid suffix L for a while.       */

#define j_memsize(a) (_joptable(a) & _J_OPTYPE)  /* (MEM_xxx not bytes) */
/* It would be nice to arrange J_LDRxK = J_LDRBK + 4*MEM_x too.         */

/* Note: if someone sets alignof_double < 8 then they could             */
/* hardly expect J_ALIGN8 on a double to have any meaning?              */
/* Hence we set J_ALIGN8 for all J_LDRDK's, even if alignof_double=4.   */
/* Think a little more about SPARC with mis-aligned double.             */
#define    MEM_to_J_LDRxK_table \
        { J_LDRBK|J_ALIGN1, J_LDRWK|J_ALIGN2, J_LDRK|J_ALIGN4, \
          J_LDRFK|J_ALIGN4, J_LDRDK|J_ALIGN8, J_LDRLK|J_ALIGN8 }

#ifdef TARGET_LACKS_MULDIV_LITERALS
#  define jop_canRTOK(op) \
   (((op) != J_MULR) && \
    ((op) & ~(J_SIGNED+J_UNSIGNED)) != J_DIVR && \
    ((op) & ~(J_SIGNED+J_UNSIGNED)) != J_REMR)
#else
#ifdef TARGET_LACKS_MULTIPLY_LITERALS
#  define jop_canRTOK(op) \
   ((op) != J_MULR)
#else
#ifdef TARGET_LACKS_DIVIDE_LITERALS
#  define jop_canRTOK(op) \
   (((op) & ~(J_SIGNED+J_UNSIGNED)) != J_DIVR && \
    ((op) & ~(J_SIGNED+J_UNSIGNED)) != J_REMR)
#else
#  define jop_canRTOK(op) 1
#endif
#endif
#endif

#ifdef DEFINE_JOPTABLE
/*
 * Although it is generally a nasty thing to do, I have a definition of
 * the initialised array joptable here in a header file, in such a way that
 * it is only activated if the macro DEFINE_JOPTABLE is defined.  This must
 * be so for exactly one place in the source where jopcode.h gets included.
 * Doing things this way allows me to keep the enumeration of codes in step
 * with associated entries in the array.
 * NB: conditional compilation within this table might cause things to
 *     get out of step - so beware.
 */
#  if defined ENABLE_CG || defined ENABLE_REGS || defined ENABLE_CSE
#     define with_bits(n, b) {n, b},
      extern struct JopTable { char *name; unsigned32 bits; } joptable[];
      struct JopTable joptable[] = {
#  else
#     define with_bits(n, b) { b },
      extern struct JopTable { unsigned32 bits; } joptable[];
      struct JopTable joptable[] = {
#  endif
#else
#  define with_bits(n, b)
#  if defined ENABLE_CG || defined ENABLE_REGS || defined ENABLE_CSE
      extern struct JopTable { char *name; unsigned32 bits; } joptable[];
#  else
      extern struct JopTable { unsigned32 bits; } joptable[];
#  endif
#endif

/*
 * Note that whenever an operand has both an R and a K (register and literal
 * operand) form the two must appear adjacent in this table for J_RTOK()
 * Towards the start of this table there are some other numbering nasties
 * which allow rapid conversion between related codes - see the macros defined
 * above for what is done, and do not re-order this table or insert or
 * delete entries without understanding the consequences.
 * In particular, note that there must be no gaps, since the numbering
 * of jopcodes is used as an index into the table created by with_bits().
 */


#define _ldr(MEM_x) _J_MEMR+_J_MEM+MEM_x         /* loads a single byte/short/word from memory */
#define _str(MEM_x) _J_MEMW+_J_MEM+MEM_x         /* stores a single byte/short/word to memory */
#define _memr(MEM_x) _J_MEMW+MEM_x               /* reads data from memory */
#define _memw(MEM_x) _J_MEMW+MEM_x               /* stores data to memory */
#define _memrw(MEM_x) _J_MEMR+_J_MEMW+MEM_x      /* reads and writes data to memory */

#define    J_NOOP       0L
           with_bits("NOOP", _J_GAP1+_J_GAP2)
#define    J_LABEL      1L
           with_bits("LABEL", _J_GAP1+_J_GAP2)
#define    J_B          2L
           with_bits("B", _J_GAP1+_J_GAP2)
#define    J_BXX        3L      /* as J_B but within branch table    */
           with_bits("BXX", _J_GAP1+_J_GAP2)

#define    J_LDRK       4L          /* see J_LDRV1 for ordering      */
           with_bits("LDRK", _ldr(MEM_I)+_J_READ_R2+_J_SET_R1)
#define    J_LDRR       5L
           with_bits("LDRR", _ldr(MEM_I)+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_LDRVK      6L
           with_bits("LDRVK", _ldr(MEM_I)+_J_STACKREF+_J_IVD+_J_SET_R1)
#define    J_LDRV       7L
           with_bits("LDRV", _ldr(MEM_I)+_J_SET_R1+_J_STACKREF+_J_GAP2)

#define    J_LDRBK      8L
           with_bits("LDRBK", _ldr(MEM_B)+_J_READ_R2+_J_SET_R1)
#define    J_LDRBR      9L
           with_bits("LDRBR", _ldr(MEM_B)+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_LDRBVK     10L
           with_bits("LDRBVK", _ldr(MEM_B)+_J_STACKREF+_J_IVD+_J_SET_R1)
#define    J_LDRBV      11L             /* unused Apr 92                */
           with_bits("LDRBV", _ldr(MEM_B)+_J_SET_R1+_J_STACKREF+_J_GAP2)

#define    J_LDRWK      12L
           with_bits("LDRWK", _ldr(MEM_W)+_J_READ_R2+_J_SET_R1)
#define    J_LDRWR      13L
           with_bits("LDRWR", _ldr(MEM_W)+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_LDRWVK     14L
           with_bits("LDRWVK", _ldr(MEM_W)+_J_STACKREF+_J_IVD+_J_SET_R1)
#define    J_LDRWV      15L             /* unused Apr 92                */
           with_bits("LDRWV", _ldr(MEM_W)+_J_SET_R1+_J_STACKREF+_J_GAP2)

#define    J_LDRLK      16L
           with_bits("LDRLK", _ldr(MEM_LL)+_J_READ_R2+_J_SET_R1)
#define    J_LDRLR      17L
           with_bits("LDRLR", _ldr(MEM_LL)+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_LDRLVK     18L
           with_bits("LDRLVK", _ldr(MEM_LL)+_J_STACKREF+_J_IVD+_J_SET_R1)
#define    J_LDRLV      19L             /* unused Apr 92                */
           with_bits("LDRLV", _ldr(MEM_LL)+_J_SET_R1+_J_STACKREF+_J_GAP2)

#define    J_LDRFK      20L
           with_bits("LDRFK", _J_FLOATING+_ldr(MEM_F)+_J_READ_R2+_J_SET_R1)
#define    J_LDRFR      21L
           with_bits("LDRFR", _J_FLOATING+_ldr(MEM_F)+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_LDRFVK     22L
           with_bits("LDRFVK", _J_FLOATING+_ldr(MEM_F)+_J_STACKREF+_J_IVD+_J_SET_R1)
#define    J_LDRFV      23L
           with_bits("LDRFV", _J_FLOATING+_ldr(MEM_F)+_J_SET_R1+_J_STACKREF+_J_GAP2)

#define    J_LDRDK      24L
           with_bits("LDRDK", _J_DOUBLE+_ldr(MEM_D)+_J_READ_R2+_J_SET_R1)
#define    J_LDRDR      25L
           with_bits("LDRDR", _J_DOUBLE+_ldr(MEM_D)+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_LDRDVK     26L
           with_bits("LDRDVK", _J_DOUBLE+_ldr(MEM_D)+_J_STACKREF+_J_IVD+_J_SET_R1)
#define    J_LDRDV      27L
           with_bits("LDRDV", _J_DOUBLE+_ldr(MEM_D)+_J_SET_R1+_J_STACKREF+_J_GAP2)

#define    J_STRK       28L
           with_bits("STRK", _str(MEM_I)+_J_READ_R2+_J_READ_R1)
#define    J_STRR       29L
           with_bits("STRR", _str(MEM_I)+_J_READ_R3+_J_READ_R2+_J_READ_R1+_J_COMMUTATIVE)
#define    J_STRVK      30L
           with_bits("STRVK", _str(MEM_I)+_J_STACKREF+_J_IVD+_J_READ_R1)
#define    J_STRV       31L
           with_bits("STRV", _str(MEM_I)+_J_READ_R1+_J_STACKREF+_J_GAP2)

#define    J_STRBK      32L
           with_bits("STRBK", _str(MEM_B)+_J_READ_R2+_J_READ_R1)
#define    J_STRBR      33L
           with_bits("STRBR", _str(MEM_B)+_J_READ_R3+_J_READ_R2+_J_READ_R1+_J_COMMUTATIVE)
#define    J_STRBVK     34L
           with_bits("STRBVK", _str(MEM_B)+_J_STACKREF+_J_IVD+_J_READ_R1)
#define    J_STRBV      35L             /* unused Apr 92                */
           with_bits("STRBV", _str(MEM_B)+_J_READ_R1+_J_STACKREF+_J_GAP2)

#define    J_STRWK      36L
           with_bits("STRWK", _str(MEM_W)+_J_READ_R2+_J_READ_R1)
#define    J_STRWR      37L
           with_bits("STRWR", _str(MEM_W)+_J_READ_R3+_J_READ_R2+_J_READ_R1+_J_COMMUTATIVE)
#define    J_STRWVK     38L
           with_bits("STRWVK", _str(MEM_W)+_J_STACKREF+_J_IVD+_J_READ_R1)
#define    J_STRWV      39L             /* unused Apr 92                */
           with_bits("STRWV", _str(MEM_W)+_J_READ_R1+_J_STACKREF+_J_GAP2)

#define    J_STRLK      40L
           with_bits("STRLK", _str(MEM_LL)+_J_READ_R2+_J_READ_R1)
#define    J_STRLR      41L
           with_bits("STRLR", _str(MEM_LL)+_J_READ_R3+_J_READ_R2+_J_READ_R1+_J_COMMUTATIVE)
#define    J_STRLVK     42L
           with_bits("STRLVK", _str(MEM_LL)+_J_STACKREF+_J_IVD+_J_READ_R1)
#define    J_STRLV      43L             /* unused Apr 92                */
           with_bits("STRLV", _str(MEM_LL)+_J_READ_R1+_J_STACKREF+_J_GAP2)

#define    J_STRFK      44L
           with_bits("STRFK", _J_FLOATING+_str(MEM_F)+_J_READ_R2+_J_READ_R1)
#define    J_STRFR      45L
           with_bits("STRFR", _J_FLOATING+_str(MEM_F)+_J_READ_R3+_J_READ_R2+_J_READ_R1+_J_COMMUTATIVE)
#define    J_STRFVK     46L
           with_bits("STRFVK", _J_FLOATING+_str(MEM_F)+_J_STACKREF+_J_IVD+_J_READ_R1)
#define    J_STRFV      47L
           with_bits("STRFV", _J_FLOATING+_str(MEM_F)+_J_READ_R1+_J_STACKREF+_J_GAP2)

#define    J_STRDK      48L
           with_bits("STRDK", _J_DOUBLE+_str(MEM_D)+_J_READ_R2+_J_READ_R1)
#define    J_STRDR      49L
           with_bits("STRDR", _J_DOUBLE+_str(MEM_D)+_J_READ_R3+_J_READ_R2+_J_READ_R1+_J_COMMUTATIVE)
#define    J_STRDVK     50L
           with_bits("STRDVK", _J_DOUBLE+_str(MEM_D)+_J_STACKREF+_J_IVD+_J_READ_R1)
#define    J_STRDV      51L
           with_bits("STRDV", _J_DOUBLE+_str(MEM_D)+_J_READ_R1+_J_STACKREF+_J_GAP2)

#define    J_LDRV1      52L     /* see LDRFV1 */
           with_bits("LDRV1", _ldr(MEM_I)+_J_SET_R1+_J_STACKREF+_J_GAP2)
#define    J_PUSHR      53L
           with_bits("PUSHR", _memw(0)+_J_READ_R1+_J_GAP2)
#define    J_MOVK       54L
           with_bits("MOVK", _J_SET_R1+_J_PSEUDO_READS_R2)
#define    J_MOVR       55L
           with_bits("MOVR", _J_READ_R3+_J_SET_R1+_J_REGMOVE+_J_GAP2)

#define    J_CMPK       56L       /* see pseudo_reads_r2() (loop opt) */
           with_bits("CMPK", _J_READ_R2)
#define    J_CMPR       57L
           with_bits("CMPR", _J_READ_R3+_J_READ_R2+_J_GAP1)
#define    J_CHKLK      58L
           with_bits("CHKLK", _J_READ_R2)
#define    J_CHKLR      59L
           with_bits("CHKLR", _J_READ_R3+_J_READ_R2+_J_GAP1)
#define    J_CHKUK      60L
           with_bits("CHKUK", _J_READ_R2)
#define    J_CHKUR      61L
           with_bits("CHKUR", _J_READ_R3+_J_READ_R2+_J_GAP1)
#define    J_STACK      62L
           with_bits("STACK", _J_GAP1+_J_GAP2)
#define    J_SETSPENV   63L
           with_bits("SETSPENV", _J_GAP1+_J_GAP2)

#define    J_LDRLV1     64L     /* must be J_LDRV1 + (J_LDRLK-K_LDRVK)   */
           with_bits("LDRLV1", _ldr(MEM_I)+_J_SET_R1+_J_STACKREF+_J_GAP2)
#define    J_PUSHL      65L
           with_bits("PUSHL", _memw(0)+_J_READ_R1+_J_GAP2)
#define    J_MOVLK      66L
           with_bits("MOVLK", _J_SET_R1)
#define    J_MOVLR      67L
           with_bits("MOVLR", _J_READ_R3+_J_SET_R1+_J_REGMOVE+_J_GAP2)

#define    J_LDRFV1     68L     /* must be J_LDRV1 + (J_LDRFK-K_LDRVK)   */
           with_bits("LDRFV1", _J_FLOATING+_ldr(MEM_F)+_J_SET_R1+_J_STACKREF+_J_GAP2)
#define    J_PUSHF      69L
           with_bits("PUSHF", _J_FLOATING+_J_READ_R1+_J_GAP2)
#define    J_MOVFK      70L
           with_bits("MOVFK", _J_FLOATING+_J_SET_R1+_J_GAP2)
#define    J_MOVFR      71L
           with_bits("MOVFR", _J_FLOATING+_J_READ_R3+_J_SET_R1+_J_REGMOVE+_J_GAP2)

#define    J_LDRDV1     72L     /* must be J_LDRV1 + (J_LDRDK-K_LDRVK)   */
           with_bits("LDRDV1", _J_DOUBLE+_ldr(MEM_D)+_J_SET_R1+_J_STACKREF+_J_GAP2)
#define    J_PUSHD      73L
           with_bits("PUSHD", _memw(0)+_J_DOUBLE+_J_READ_R1+_J_GAP2)
#define    J_MOVDK      74L
           with_bits("MOVDK", _J_DOUBLE+_J_SET_R1+_J_GAP2)
#define    J_MOVDR      75L
           with_bits("MOVDR", _J_DOUBLE+_J_READ_R3+_J_SET_R1+_J_REGMOVE+_J_GAP2)

#define    J_ANDK       76L
           with_bits("ANDK", _J_READ_R2+_J_SET_R1)
#define    J_ANDR       77L
           with_bits("ANDR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_ORRK       78L
           with_bits("ORRK", _J_READ_R2+_J_SET_R1)
#define    J_ORRR       79L
           with_bits("ORRR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_EORK       80L
           with_bits("EORK", _J_READ_R2+_J_SET_R1)
#define    J_EORR       81L
           with_bits("EORR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_ADDK       82L
           with_bits("ADDK", _J_READ_R2+_J_SET_R1)
#define    J_ADDR       83L
           with_bits("ADDR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_CALLK      84L
           with_bits("CALLK", _J_SET_R1)
#define    J_CALLR      85L
           with_bits("CALLR", _J_READ_R3+_J_SET_R1)
#define    J_MULK       86L
           with_bits("MULK", _J_READ_R2+_J_SET_R1)
#define    J_MULR       87L
           with_bits("MULR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_CMPFK      88L
           with_bits("CMPFK", _J_FLOATING+_J_READ_R2+_J_GAP1)
#define    J_CMPFR      89L
           with_bits("CMPFR", _J_FLOATING+_J_READ_R3+_J_READ_R2+_J_GAP1)
#define    J_CHKNEFK    90L
           with_bits("CHKNEFK", _J_FLOATING+_J_READ_R2+_J_GAP1)
#define    J_CHKNEFR    91L
           with_bits("CHKNEFR", _J_FLOATING+_J_READ_R3+_J_READ_R2+_J_GAP1)
#define    J_CHKNEDK    92L             /* was misspelt J_CMPNEDK */
           with_bits("CHKNEDK", _J_DOUBLE+_J_READ_R2+_J_GAP1)
#define    J_CHKNEDR    93L
           with_bits("CHKNEDR", _J_DOUBLE+_J_READ_R3+_J_READ_R2+_J_GAP1)
#define    J_CHKNEK     94L
           with_bits("CHKNEK", _J_READ_R2)
#define    J_CHKNER     95L
           with_bits("CHKNER", _J_READ_R3+_J_READ_R2+_J_GAP1)
#define    J_OPSYSK     96L
           with_bits("OPSYSK", _J_SET_R1)
#define    CSE_LOADR    97L    /* wanted internally by CSE */
           with_bits("CSE_LOADR", _J_SET_R1)

#define    J_INIT       98L
           with_bits("INIT", _J_SET_R1+_J_GAP2)
#define    J_ENTER      99L    /* special effect on registers */
           with_bits("ENTER", _J_GAP1+_J_GAP2)
#define    J_ENDPROC   100L
           with_bits("ENDPROC", _J_GAP1+_J_GAP2)
/*
 * From here onwards the order of jopcodes is less important.
 */

#define    J_ADCONF     101L     /* only if SOFTWARE_FLOATING_POINT */
           with_bits("ADCONF", _J_MEMA+_J_SET_R1+_J_PSEUDO_READS_R2)
#define    J_ADCOND     102L     /* only if SOFTWARE_FLOATING_POINT */
           with_bits("ADCOND", _J_MEMA+_J_SET_R1+_J_PSEUDO_READS_R2)
#define    J_STRING     103L
           with_bits("STRING", _J_SET_R1)
#define    J_SUBK       104L
           with_bits("SUBK", _J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_SUBR       105L
           with_bits("SUBR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_DIVK       106L   /* must or in J_SIGNED/J_UNSIGNED */
           with_bits("DIVK", _J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_DIVR       107L
           with_bits("DIVR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_RSBK       108L   /* should only happen as J_RSBR+J_SHIFTM */
           with_bits("RSBK", _J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_RSBR       109L  /* should only happen as J_RSBR+J_SHIFTM */
           with_bits("RSBR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_REMK       110L   /* must or in J_SIGNED/J_UNSIGNED */
           with_bits("REMK", _J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_REMR       111L
           with_bits("REMR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_SHLK       112L   /* must or in J_SIGNED/J_UNSIGNED */
           with_bits("SHLK", _J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_SHLR       113L
           with_bits("SHLR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_SHRK       114L   /* must or in J_SIGNED/J_UNSIGNED */
           with_bits("SHRK", _J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_SHRR       115L
           with_bits("SHRR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_INLINE1    116L
           with_bits("INLINE1", _J_READ_R2+_J_SET_R1)
#define    J_NEGR       117L
           with_bits("NEGR", _J_READ_R3+_J_SET_R1+_J_GAP2)
#define    J_NOTR       118L
           with_bits("NOTR", _J_READ_R3+_J_SET_R1+_J_GAP2)
#define    J_ADCONV     119L   /* special use of r2 field */
           with_bits("ADCONV", _J_MEMA+_J_SET_R1+_J_STACKREF+_J_PSEUDO_READS_R2)
#define    J_USEF       120L
           with_bits("USEF", _J_FLOATING+_J_READ_R1+_J_GAP2)
#define    J_INITF      121L
           with_bits("INITF", _J_FLOATING+_J_SET_R1+_J_GAP2)
#define    J_ADDFK      122L
           with_bits("ADDFK", _J_FLOATING+_J_READ_R2+_J_SET_R1)
#define    J_ADDFR      123L
           with_bits("ADDFR", _J_FLOATING+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_MULFK      124L
           with_bits("MULFK", _J_FLOATING+_J_READ_R2+_J_SET_R1)
#define    J_MULFR      125L
           with_bits("MULFR", _J_FLOATING+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_INLINE1F   126L
           with_bits("INLINE1F", _J_FLOATING+_J_READ_R2+_J_SET_R1)
#define    J_SUBFK      127L
           with_bits("SUBFK", _J_FLOATING+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_SUBFR      128L
           with_bits("SUBFR", _J_FLOATING+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_DIVFK      129L
           with_bits("DIVFK", _J_FLOATING+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_DIVFR      130L
           with_bits("DIVFR", _J_FLOATING+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_NEGFR      131L
           with_bits("NEGFR", _J_FLOATING+_J_READ_R3+_J_SET_R1+_J_GAP2)
#define    J_FLTFR      132L
           with_bits("FLTFR", _J_FLOATING+_J_READ_R3+_J_SET_R1)
#define    J_FIXFR      133L
           with_bits("FIXFR", _J_FLOATING+_J_READ_R3+_J_SET_R1)
#define    J_MOVDFR     134L
           with_bits("MOVDFR", _J_FLOATING+_J_READ_R3+_J_SET_R1+_J_GAP2)
#define    J_MOVIFR     135L
           with_bits("MOVIFR", _J_FLOATING+_J_READ_R3+_J_SET_R1+_J_GAP2)
#define    J_ADCON      136L
           with_bits("ADCON", _J_MEMA+_J_SET_R1+_J_PSEUDO_READS_R2)
#define    J_USED       137L
           with_bits("USED", _J_DOUBLE+_J_READ_R1+_J_GAP2)
#define    J_INITD      138L
           with_bits("INITD", _J_DOUBLE+_J_SET_R1+_J_GAP2)
#define    J_CMPDK      139L
           with_bits("CMPDK", _J_DOUBLE+_J_READ_R2+_J_GAP1)
#define    J_CMPDR      140L
           with_bits("CMPDR", _J_DOUBLE+_J_READ_R3+_J_READ_R2+_J_GAP1)
#define    J_ADDDK      141L
           with_bits("ADDDK", _J_DOUBLE+_J_READ_R2+_J_SET_R1)
#define    J_ADDDR      142L
           with_bits("ADDDR", _J_DOUBLE+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_MULDK      143L
           with_bits("MULDK", _J_DOUBLE+_J_READ_R2+_J_SET_R1)
#define    J_MULDR      144L
           with_bits("MULDR", _J_DOUBLE+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_COMMUTATIVE)
#define    J_MOVIDR     145L    /* a bit special - doesn't require r1/r3 clash. */
           with_bits("MOVIDR", _J_DOUBLE+_J_READ_R3+_J_READ_R2+_J_SET_R1)
#define    J_INLINE1D   146L
           with_bits("INLINE1D", _J_DOUBLE+_J_READ_R2+_J_SET_R1)
#define    J_SUBDK      147L
           with_bits("SUBDK", _J_DOUBLE+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_SUBDR      148L
           with_bits("SUBDR", _J_DOUBLE+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_DIVDK      149L
           with_bits("DIVDK", _J_DOUBLE+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_DIVDR      150L
           with_bits("DIVDR", _J_DOUBLE+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_NEGDR      151L
           with_bits("NEGDR", _J_DOUBLE+_J_READ_R3+_J_SET_R1+_J_GAP2)
#define    J_FLTDR      152L
           with_bits("FLTDR", _J_DOUBLE+_J_READ_R3+_J_SET_R1+_J_GAP2)
#define    J_FIXDR      153L
           with_bits("FIXDR", _J_DOUBLE+_J_READ_R3+_J_SET_R1+_J_GAP2)
#define    J_MOVFDR     154L
           with_bits("MOVFDR", _J_DOUBLE+_J_READ_R3+_J_SET_R1+_J_GAP2)
#define    CSE_WORD1    155L
           with_bits("CSE_WORD1", _J_DOUBLE+_J_READ_R3+_J_SET_R1)
#define    CSE_WORD2    156L
           with_bits("CSE_WORD2", _J_DOUBLE+_J_READ_R3+_J_SET_R1)
/*
 * Things from here on are not needed for all targets.
 */

#define    J_CLRC       157L
           with_bits("CLRC", _memw(0)+_J_READ_R1)
#define    J_MOVC       158L
           with_bits("MOVC", _memrw(0)+_J_READ_R2+_J_READ_R1)
#define    J_TAILCALLK  159L            /* see tailify_() */
           with_bits("TAILCALLK", _J_SET_R1)
#define    J_TAILCALLR  160L            /* see tailify_() */
           with_bits("TAILCALLR", _J_READ_R3+_J_SET_R1)
/* The following are not used at present, but may be sometime - when they
 * are the numbering of jopcodes will probably need rearranging to fit these
 * in somewhere better than at the end. Indeed some of these MAY be used now...
 * but sort of experimentally!
 */
#define    J_COUNT      161L
           with_bits("COUNT", _J_GAP1+_J_GAP2)
#define    J_INFOLINE   162L
           with_bits("INFOLINE", 0)
#define    J_INFOSCOPE  163L
           with_bits("INFOSCOPE", _J_GAP1+_J_GAP2)
#define    J_INFOBODY   164L
           with_bits("INFOBODY", 0)
#define    J_RESULT2    165L
           with_bits("RESULT2", _J_READ_R3+_J_SET_R1)
#define    J_FNCON      166L   /* only if TARGET_CALL_USES_DESCRIPTOR */
           with_bits("FNCON", _J_MEMA+_J_SET_R1)
/* AM reserves the next two opcodes for flt->int reg moves.             */
/* Note that MOVDIR sets r1 and r2.  Regalloc beware.                   */
#define    J_MOVDIR     167L
           with_bits("MOVDIR", _J_SET_R1+_J_SET_R2+_J_READ_R3)
#define    J_MOVFIR     168L
           with_bits("MOVFIR", _J_SET_R1+_J_READ_R3+_J_GAP2)
#define    J_THUNKTABLE 169L    /* compare J_CASEBRANCH                 */
           with_bits("THUNKTABLE", _J_GAP1)
#define    J_EXTEND     170L    /* sign extend */
           with_bits("EXTEND", _J_SET_R1+_J_READ_R2)

#define    J_RORK       171L
           with_bits("RORK", _J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_RORR       172L
           with_bits("RORR", _J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)

#define    J_ORG        173L
           with_bits("ORG", _J_GAP1+_J_GAP2)
#define    J_SAVE       174L
           with_bits("SAVE", _J_GAP1+_J_GAP2)
#define    J_PUSHM      175L     /* for the back end, reg map   */
           with_bits("PUSHM", _memw(0)+_J_GAP1+_J_GAP2)
#define    J_spare176   176L
           with_bits("x176x", _J_GAP1+_J_GAP2)
#define    J_WORD       177L
           with_bits("WORD", _J_GAP1+_J_GAP2)
#define    J_CONDEXEC   178L
           with_bits("CONDEXEC", _J_GAP1+_J_GAP2)
#define    J_VSTORE     179L
           with_bits("VSTORE", _J_GAP1+_J_GAP2)
#define    J_MOVLIR     180L
           with_bits("MOVLIR", _J_SET_R1+_J_SET_R2+_J_READ_R3)
#define    J_MOVILR     181L
           with_bits("MOVILR", _J_SET_R1+_J_READ_R2+_J_READ_R3)
#define    J_SETSPGOTO  182L
           with_bits("SETSPGOTO", _J_GAP1+_J_GAP2)
#define    J_SETSP      183L
           with_bits("SETSP", _J_GAP1)
#define    J_CASEBRANCH 184L /* dispatch into branch table          */
           with_bits("CASEBRANCH", _J_READ_R1)
#define    J_USE        185L        /* uses R1 (for support of 'volatile') */
           with_bits("USE", _J_READ_R1+_J_GAP2)

#define    J_SCCK       186L    /* compare J_CONDEXEC                   */
           with_bits("SCCK", _J_SET_R1)
#define    J_BZ         187L     /* For 88000 back-end use only */
           with_bits("BZ", _J_READ_R2)

/* ECN: Some jopcodes for creating Thumb vtables
 *      J_WORD_ADCON    ->      DCD     extern
 *      J_WORD_LABEL    ->      DCD     L<n>
 *      J_THIS_ADJUST   ->      ADD     R1, R2, #K (but cannot be done using J_ADDK
 *                                                  because we need to know it
 *                                                  is a pointer adjustment so we
 *                                                  can generate ARM or Thumb
 *                                                  instructions as appropriate)
 * ECN: It is too difficult to try and conditionalise this on THUMB_CPLUSPLUS
 *      as peepgen includes this header to derive peeppat.c. May be generically
 *      useful anyway (at least J_WORD_ADCON & J_WORD_LABEL).
 */
#define    J_WORD_ADCON         188L
           with_bits("WORD.A", _J_GAP1+_J_GAP2)
#define    J_WORD_LABEL         189L
           with_bits("WORD.L", _J_GAP1+_J_GAP2)
#define    J_THIS_ADJUST        190L
           with_bits("ADJUST", _J_READ_R2+_J_SET_R1)

#define    J_RSBDK              191L
           with_bits("RSBDK", _J_DOUBLE+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_RSBDR              192L
           with_bits("RSBDR", _J_DOUBLE+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_RDVDK              193L
           with_bits("RDVDK", _J_DOUBLE+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_RDVDR              194L
           with_bits("RDVDR", _J_DOUBLE+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)

#define    J_RSBFK              195L
           with_bits("RSBDK", _J_FLOATING+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_RSBFR              196L
           with_bits("RSBDR", _J_FLOATING+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_RDVFK              197L
           with_bits("RDVFK", _J_DOUBLE+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)
#define    J_RDVFR              198L
           with_bits("RDVFR", _J_DOUBLE+_J_READ_R3+_J_READ_R2+_J_SET_R1+_J_ASYMDIAD)

#define    J_MLAR               199
           with_bits("MLAR", _J_READ_R4+_J_READ_R3+_J_READ_R2+_J_SET_R1)
#define    J_MLAK               200
           with_bits("MLAK", _J_READ_R4+_J_READ_R2+_J_SET_R1)
#define    J_MULL               201
           with_bits("MULL", _J_READ_R4+_J_READ_R3+_J_SET_R2+_J_SET_R1)
#define    J_MLAL               202
           with_bits("MLAL", _J_READ_R4+_J_READ_R3+_J_READ_R2+_J_READ_R1+_J_SET_R2+_J_SET_R1)

#define    J_ADCONLL            203
           with_bits("ADCONLL", _J_MEMA+_J_SET_R1+_J_PSEUDO_READS_R2)

#define    CSE_COND             204
           with_bits("CSE_COND", 0)

#define    J_TYPECASE           205
           with_bits("TYPECASE", 0)
#define    J_FIXFRM             206L
           with_bits("FIXDRM", _J_DOUBLE+_J_READ_R3+_J_SET_R1+_J_GAP2)
#define    J_FIXDRM             207L
           with_bits("FIXDRM", _J_FLOATING+_J_READ_R3+_J_SET_R1+_J_GAP2)
#define    J_EXTABLE            208L
           with_bits("EXTABLE", 0)
#define    J_EXHANDLER          209L
           with_bits("EXHANDLER", 0)

#if !defined(ARM_INLINE_ASSEMBLER) && !defined(THUMB_INLINE_ASSEMBLER)

           /* WD: ARM & Thumb share a few opcodes (LDM/STM/BL/SWI) Separate these out later */

#define J_LAST_JOPCODE 209

#else

#define    J_FIRST_ASM_JOPCODE  210
#define    J_ABINRK             J_FIRST_ASM_JOPCODE
           with_bits("BINRK", _J_READ_R2+_J_SET_R1)
#define    J_ABINRR             (J_FIRST_ASM_JOPCODE+1)
           with_bits("BINRR", _J_READ_R3+_J_READ_R2+_J_SET_R1)
#define    J_ABINRRK            (J_FIRST_ASM_JOPCODE+2)
           with_bits("BINRRK", _J_READ_R3+_J_READ_R2+_J_SET_R1)
#define    J_ABINRRR            (J_FIRST_ASM_JOPCODE+3)
           with_bits("BINRRR", _J_READ_R4+_J_READ_R3+_J_READ_R2+_J_SET_R1)
#define    J_AMOVK              (J_FIRST_ASM_JOPCODE+4)
           with_bits("MOVK", _J_SET_R1)
#define    J_AMOVR              (J_FIRST_ASM_JOPCODE+5)
           with_bits("MOVR", _J_READ_R3+_J_SET_R1)
#define    J_AMOVRK             (J_FIRST_ASM_JOPCODE+6)
           with_bits("MOVRK", _J_READ_R3+_J_SET_R1)
#define    J_AMOVRR             (J_FIRST_ASM_JOPCODE+7)
           with_bits("MOVRR", _J_READ_R4+_J_READ_R3+_J_SET_R1)
#define    J_ACMPK              (J_FIRST_ASM_JOPCODE+8)
           with_bits("CMPK", _J_READ_R2)
#define    J_ACMPR              (J_FIRST_ASM_JOPCODE+9)
           with_bits("CMPR", _J_READ_R2+_J_READ_R3)
#define    J_ACMPRK             (J_FIRST_ASM_JOPCODE+10)
           with_bits("CMPRK", _J_READ_R2+_J_READ_R3)
#define    J_ACMPRR             (J_FIRST_ASM_JOPCODE+11)
           with_bits("CMPRR", _J_READ_R2+_J_READ_R3+_J_READ_R4)
#define    J_ALDRK              (J_FIRST_ASM_JOPCODE+12)
           with_bits("LDRK", _ldr(MEM_B)+_J_READ_R2+_J_SET_R1)
#define    J_ALDRR              (J_FIRST_ASM_JOPCODE+13)
           with_bits("LDRR", _ldr(MEM_B)+_J_READ_R3+_J_READ_R2+_J_SET_R1)
#define    J_ALDRKW             (J_FIRST_ASM_JOPCODE+14)
           with_bits("LDRKW", _ldr(MEM_B)+_J_READ_R2+_J_SET_R2+_J_SET_R1)
#define    J_ALDRRW             (J_FIRST_ASM_JOPCODE+15)
           with_bits("LDRRW", _ldr(MEM_B)+_J_READ_R3+_J_READ_R2+_J_SET_R2+_J_SET_R1)
#define    J_ASTRK              (J_FIRST_ASM_JOPCODE+16)
           with_bits("STRK", _str(MEM_B)+_J_READ_R2+_J_READ_R1)
#define    J_ASTRR              (J_FIRST_ASM_JOPCODE+17)
           with_bits("STRR", _str(MEM_B)+_J_READ_R3+_J_READ_R2+_J_READ_R1)
#define    J_ASTRKW             (J_FIRST_ASM_JOPCODE+18)
           with_bits("STRKW", _str(MEM_B)+_J_READ_R2+_J_SET_R2+_J_READ_R1)
#define    J_ASTRRW             (J_FIRST_ASM_JOPCODE+19)
           with_bits("STRRW", _str(MEM_B)+_J_READ_R3+_J_READ_R2+_J_SET_R2+_J_READ_R1)
#define    J_LDM                (J_FIRST_ASM_JOPCODE+20)
           with_bits("LDM",   _memr(MEM_I)+_J_READ_R1+_J_GAP2)
#define    J_LDMW               (J_FIRST_ASM_JOPCODE+21)
           with_bits("LDMW",  _memr(MEM_I)+_J_READ_R1+_J_SET_R1+_J_GAP2)
#define    J_STM                (J_FIRST_ASM_JOPCODE+22)
           with_bits("STM",   _memw(MEM_I)+_J_READ_R1+_J_GAP2)
#define    J_STMW               (J_FIRST_ASM_JOPCODE+23)
           with_bits("STMW",  _memw(MEM_I)+_J_READ_R1+_J_SET_R1+_J_GAP2)
#define    J_MRS                (J_FIRST_ASM_JOPCODE+24)
           with_bits("MRS",   _J_SET_R1+_J_GAP2)
#define    J_MSR                (J_FIRST_ASM_JOPCODE+25)
           with_bits("MSR",   _J_READ_R3+_J_GAP1+_J_GAP2)
#define    J_MSK                (J_FIRST_ASM_JOPCODE+26)
           with_bits("MSK",   _J_GAP1+_J_GAP2)
#define    J_SWP                (J_FIRST_ASM_JOPCODE+27)
           with_bits("SWP",   _memrw(MEM_I)+_J_SET_R1+_J_READ_R2+_J_READ_R3)
#define    J_SWPB               (J_FIRST_ASM_JOPCODE+28)
           with_bits("SWPB",  _memrw(MEM_B)+_J_SET_R1+_J_READ_R2+_J_READ_R3)
#define    J_SWI                (J_FIRST_ASM_JOPCODE+29)
           with_bits("SWI",   0)
#define    J_CDP                (J_FIRST_ASM_JOPCODE+30)
           with_bits("CDP",   0)
#define    J_MRC                (J_FIRST_ASM_JOPCODE+31)
           with_bits("MRC",   _J_SET_R1)
#define    J_MCR                (J_FIRST_ASM_JOPCODE+32)
           with_bits("MCR",   _J_READ_R1)
#define    J_LDC                (J_FIRST_ASM_JOPCODE+33)
           with_bits("LDC",   _memr(MEM_I)+_J_READ_R2)
#define    J_LDCW               (J_FIRST_ASM_JOPCODE+34)
           with_bits("LDCW",  _memr(MEM_I)+_J_READ_R2+_J_SET_R2)
#define    J_STC                (J_FIRST_ASM_JOPCODE+35)
           with_bits("STC",   _memw(MEM_I)+_J_READ_R2)
#define    J_STCW               (J_FIRST_ASM_JOPCODE+36)
           with_bits("STCW",  _memw(MEM_I)+_J_READ_R2+_J_SET_R2)
#define    J_NULLOP             (J_FIRST_ASM_JOPCODE+37)
           with_bits("NOP",   _J_GAP1+_J_GAP2)
#define    J_BL                 (J_FIRST_ASM_JOPCODE+38)
           with_bits("BL",    0)

#ifdef THUMB_INLINE_ASSEMBLER

/* YEAH - I know.... */

#define J_TEMP_JOPCODE (J_FIRST_ASM_JOPCODE+38)
#define J_ADCK   (J_TEMP_JOPCODE+1L)
        with_bits("ADCK", _J_READ_R2+_J_SET_R1)
#define J_ADCR   (J_TEMP_JOPCODE+2L)
        with_bits("ADCR", _J_READ_R2+_J_READ_R3+_J_SET_R1)
#define J_SBCK   (J_TEMP_JOPCODE+3L)
        with_bits("SBCK", _J_READ_R2+_J_SET_R1)
#define J_SBCR   (J_TEMP_JOPCODE+4L)
        with_bits("SBCR", _J_READ_R2+_J_READ_R3+_J_SET_R1)
#define J_RSCK   (J_TEMP_JOPCODE+5L)
        with_bits("RSCK", _J_READ_R2+_J_SET_R1)
#define J_RSCR   (J_TEMP_JOPCODE+6L)
        with_bits("RSCR", _J_READ_R2+_J_READ_R3+_J_SET_R1)
#define J_TSTK   (J_TEMP_JOPCODE+7L)
        with_bits("TSTK", _J_READ_R2)
#define J_TSTR   (J_TEMP_JOPCODE+8L)
        with_bits("TSTR", _J_READ_R2+_J_READ_R3)
#define J_BICK   (J_TEMP_JOPCODE+9L)
        with_bits("BICK", _J_READ_R2+_J_SET_R1)
#define J_BICR   (J_TEMP_JOPCODE+10L)
        with_bits("BICR", _J_READ_R2+_J_READ_R3+_J_SET_R1)
#define J_CMNK   (J_TEMP_JOPCODE+11L)
        with_bits("CMNK", _J_READ_R2)
#define J_CMNR   (J_TEMP_JOPCODE+12L)
        with_bits("CMNR", _J_READ_R2+_J_READ_R3)
#define J_MVNK   (J_TEMP_JOPCODE+13L)
        with_bits("MVNK", _J_READ_R2)
#define J_MVNR   (J_TEMP_JOPCODE+14L)
        with_bits("MVNR", _J_READ_R2+_J_READ_R3)

#define J_LAST_ASM_JOPCODE       (J_TEMP_JOPCODE+14)
#define J_LAST_JOPCODE          J_LAST_ASM_JOPCODE


#else

#define J_LAST_ASM_JOPCODE       (J_FIRST_ASM_JOPCODE+38)
#define J_LAST_JOPCODE          J_LAST_ASM_JOPCODE

#endif

#endif /* ARM_INLINE_ASSEMBLER */








#ifdef DEFINE_JOPTABLE
   0 };

#endif

#define J_POPM @@@ Please see "mip/jopcode.h".
/* J_POPM has been superceded on all targets.  Its functionality        */
/* is provided by use of other J_OPCODE's and the new J_MOVDIR.         */
/* J_MOVDIR is unused if TARGET_FP_ARGS_CALLSTD2, but otherwise         */
/* back-ends may naively treat J_MOVDIR r1,r2,r3 as PUSHD r3 followed   */
/* by dropping into the code for POPM {r1,r2} (r2==r1+1 assumable).     */

/*************************************/
/* end of emumeration of all opcodes */
/*************************************/


typedef uint32 J_OPCODE; /* really ENUMERATED_OPCODE but ensure 32 bits */

/* n.b Q_UBIT is the same as J_UNSIGNED */
#define Q_MASK     (~(int32)0x07ffffff)
#define Q_UBIT     ((int32)0x08000000)   /* for test of signed/unsigned CMP   */
#define Q_NEGATE(x) ((x)^Q_NOT)
#define Q_issame(x,y) (((x)|Q_UBIT) == ((y)|Q_UBIT))
/* The next macro may only be applied to Q_GE and Q_LT.                     */
#define Q_tocmpz(x) ((x) ^ (Q_GE^Q_PL))  /* also (Q_LT^Q^MI)                */
#define Q_CondPair(x,y) \
  (((uint32)(x)>(uint32)(y)) ? ((uint32)(x)>>27)|((uint32)(y)>>22) :\
                               ((uint32)(x)>>22)|((uint32)(y)>>27))
/* The following idea of conditions are machine independent - so any new    */
/* code (e.g. Q_CC) should have a different number even if they have the    */
/* same effect on your favorite machine - just as Q_HS and Q_GE differ even */
/* though they have the same effect on the 370 (the comparison differs).    */
#define Q_AL     ((int32)0x00000000)
#define Q_NOT    ((int32)0x10000000)    /* obsolete */
#define Q_GT     ((int32)0x20000000)
#define Q_LE     ((int32)0x30000000)
#define Q_GE     ((int32)0x40000000)
#define Q_LT     ((int32)0x50000000)
#define Q_HI     ((int32)0x60000000|Q_UBIT)
#define Q_LS     ((int32)0x70000000|Q_UBIT)
#define Q_VS     ((int32)0x80000000)
#define Q_VC     ((int32)0x90000000)
#define Q_PL     ((int32)0xa0000000)
#define Q_MI     ((int32)0xb0000000)
#define Q_HS     ((int32)0xc0000000|Q_UBIT)
#define Q_LO     ((int32)0xd0000000|Q_UBIT)
#define Q_EQ     ((int32)0xe0000000)
#define Q_NE     ((int32)0xf0000000)
/* The following are added by AM to check conversion to new style J_CMP */
/* Q_UEQ and Q_UNE are unsigned equality tests.  May be useful one day? */
#define Q_UEQ    (Q_EQ|Q_UBIT)
#define Q_UNE    (Q_NE|Q_UBIT)

/* Special unknown condition, used in CMPs only, for 3 way tests
   where the exact condition is unknown, except for the signedness.
*/
#define Q_UKN    (Q_NOT)

/* Special undefined condition, used in CMPs only, after merging
   signed and unsigned tests where the result isn't signed nor unsigned.
*/
#define Q_UNDEF  (Q_AL)

#define Q_RealMask(m) ((m) != Q_UNDEF && (m) != Q_UKN)

#ifndef JOPCODEDEF_ONLY
#define is_block_label(lab) ((uint32)(lab) < (uint32)NOTALAB)   /* Nasty HACK! */

char *condition_name(uint32 mask);
void print_jopcode(const Icode *const ic);
void print_jopcode_1(const Icode *const ic);
void jopprint_opname(J_OPCODE o);
void jopprint_op3(J_OPCODE op, VRegInt r2, VRegInt r3);
void print_xjopcode(const Icode *const ic, char *fmt, ...);
void flowgraf_printblock(BlockHead *p, bool deadbits);
void flowgraf_print(const char *mess, bool deadbits);
void pr_bindlist(BindList *p);
bool Q_implies(int32 cond1, int32 cond2);

#endif

#endif

/* End of jopcode.h */
