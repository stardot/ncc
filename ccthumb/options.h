/*
 * options.h -- compiler configuration options set at compile time
 * Copyright (C) Acorn Computers Ltd. 1988
 * Copyright (C) Codemist Ltd., 1994
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _options_LOADED
#define _options_LOADED

/*
 * The following conditional settings allow the produced compiler (TARGET)
 * to depend on the HOST (COMPILING_ON) environment.
 * Note that we choose to treat this independently of the target-machine /
 * host-machine issue.
 */

#include "toolver.h"
#define NON_RELEASE_VSN TOOLVER_TCC

#define DISABLE_ERRORS

#define PCS_DEFAULTS (PCS_CALLCHANGESPSR|PCS_NOSTACKCHECK|PCS_NOFP)

#ifdef TARGET_IS_NEWTON
#   define TARGET_MACHINE    "Newton"
#   define TARGET_SYSTEM     "Newton OS"
#   define TARGET_IS_NEWTONOS 1
#   ifndef VERSIONSTRING
#     define VERSIONSTRING   "0.11/C5.00"
#   endif
#   define TARGET_DEFAULT_BIGENDIAN 1
#   define NO_INSTORE_FILES  1          /* no in-store headers for Newton. */
#else
#define TARGET_SYSTEM     ""
#endif

#define TARGET_HAS_DIVREM_FUNCTION 1
#define TARGET_HAS_DIV_10_FUNCTION 1

#ifdef COMPILING_ON_UNIX
#define DRIVER_ENV     { \
      0, (KEY_LINK), 0, \
      "/usr/local/lib/arm", "/usr/local/lib/arm", "", "/", "/usr/local/lib/arm", "", "lst", \
      "tasm", \
      "armlink", NULL, "", \
      "", "", "", \
      "armlib.o", "hostlib.o", "armlib.o", "", "", "" \
}
#else
#ifdef COMPILING_ON_MSDOS
#define DRIVER_ENV { \
       0, (KEY_LINK), 0, \
       "\\arm\\lib", "\\arm\\lib", "", "\\", "\\arm\\lib", "", "lst", \
       "tasm", \
       "armlink", NULL, "", \
       "", "", "", \
       "armlib.o", "hostlib.o", "armlib.o", "", "", "" \
    }
#else
#ifdef COMPILING_ON_MACINTOSH
#define DRIVER_ENV { \
       0, (KEY_LINK), 0, \
       "", "", "", ":", "", "", "lst", \
       "tasm", \
       "armlink", NULL, "", \
       "", "", "", \
       "armlib.o", "hostlib.o", "armlib.o", "", "", "" \
     }
#else
#ifdef COMPILING_ON_RISC_OS
#define DRIVER_ENV { \
       0, (KEY_LINK), 0, \
       "$.clib", "$.clib", "$.plib", ".", "$.clib", "$.plib", "l", \
       "tasm", \
       "CHAIN:link", NULL, "", \
       "", "", "", \
      "o.ansilib", "o.hostlib", "o.ansilib", "o.fortlib", "o.fortlib", "o.plib" \
     }
#else
#error Unknown host
#endif
#endif
#endif
#endif

/* #define DO_NOT_EXPLOIT_REGISTERS_PRESERVED_BY_CALLEE 1 */
/* #define MOVC_KILLS_REGISTER_PRESERVED_BY_CALLEE_EXPLOITATION 1 */

/* to avoid conflict with host compilers */
#define C_INC_VAR  "ARMINC"
#define C_LIB_VAR  "ARMLIB"

#ifndef DRIVER_OPTIONS
#  define DRIVER_OPTIONS     {"-D__thumb", NULL}
#endif

#ifndef RELEASE_VSN
#  define ENABLE_ALL          1 /* -- to enable all debugging options */
#endif

#define HOST_WANTS_NO_BANNER 1

/* mac-specific options - find a better home for these sometime! */
#ifdef macintosh
#  define NO_STATIC_BANNER 1
   pascal void SpinCursor(short increment);        /* copied from CursorCtl.h */
#  define ExecuteOnSourceBufferFill()   SpinCursor(1)
#endif

#define MSG_TOOL_NAME  "armcc"  /* used to load correct NLS message file */

#define TARGET_HAS_ASD
#define TARGET_HAS_DWARF

#define TARGET_HAS_INLINE_ASSEMBLER 1
#define THUMB_INLINE_ASSEMBLER      1

#endif

/* end of ccthumb/options.h */
