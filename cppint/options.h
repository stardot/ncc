/*
 * options.h -- compiler configuration options set at compile time
 * Copyright (C) Acorn Computers Ltd. 1988
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

#define CPLUSPLUS 1

#define USE_PP

#define NO_LISTING_OUTPUT       1
#define NO_CONFIG               1
#define NO_OBJECT_OUTPUT        1
#define NO_DEBUGGER             1
#define NO_ASSEMBLER_OUTPUT     1

#define TARGET_VTAB_ELTSIZE  4
    /* for indirect VTABLEs optimised for single inheritance */

#include "toolver.h"
#define NON_RELEASE_VSN TOOLVER_ARMCPP

/* Expire this version at 00:00:01 on Saturday 01 Oct 94 */
/*#define UNIX_TIME_LIMIT 780969601                      */

#define TARGET_ENDIANNESS_CONFIGURABLE 1
/* #define TARGET_DEFAULT_BIGENDIAN 0 */ /* 1 => bigendian default */
                                         /* 0 => littleendian default */
                                         /* unset => defaults to host */

#define DISABLE_ERRORS       1 /* -- to enable -Exyz... error suppression */
#define EXTENSION_SYSV       1 /* -- to allow #ident ... */

#   define TARGET_SYSTEM "C Interpreter"

#   define HOST_WANTS_NO_BANNER 1

#   ifndef DRIVER_OPTIONS
      /* -D__arm done by TARGET_PREDEFINES */
#     define DRIVER_OPTIONS       {NULL}
#   endif

# define C_INC_VAR  "ARMINC"
# define C_LIB_VAR  "ARMLIB"

#ifndef RELEASE_VSN
#  define ENABLE_ALL          1 /* -- to enable all debugging options */
#endif

/* mac-specific options - find a better home for these sometime! */
#ifdef macintosh
  /* The origin of time is 0th Jan 1904... */
# ifdef UNIX_TIME_LIMIT
#  define TIME_LIMIT (UNIX_TIME_LIMIT+(66*365+16)*24*3600)
# endif
# define NO_STATIC_BANNER 1
  pascal void SpinCursor(short increment);     /* copied from CursorCtl.h */
# define ExecuteOnSourceBufferFill()   SpinCursor(1)

#ifdef TARGET_IS_NEWTON
# define HOST_OBJECT_INCLUDES_SOURCE_EXTN 1  /* .c -> .c.o */
# define EXTENSION_COUNTED_STRINGS     1  /* to enable Pascal-style strings */
# define EXTENSION_UNSIGNED_STRINGS 1 /* and they are unsigned */
# define ALLOW_KEYWORDS_IN_HASHIF      1  /* to allow keywords in #if expns */
# define ALLOW_WHITESPACE_IN_FILENAMES 1  /* to allow as it says...         */
# define ONLY_WARN_ON_NONPRINTING_CHAR 1  /* to do as it says...            */
# define HOST_DOES_NOT_FORCE_TRAILING_NL 1
# define HOST_WANTS_NO_BANNER 1           /* no routine banner output       */
# define DISABLE_ERRORS 1
# define TARGET_WANTS_LINKER_TO_RESOLVE_FUNCTION_REFERENCES 1
# define HOST_CANNOT_INVOKE_ASSEMBLER 1
# define HOST_CANNOT_INVOKE_LINKER 1
# define PUT_FILE_NAME_IN_AREA_NAME 1
# define CHAR_NL '\n'
# define CHAR_CR '\r'
# define CFRONT_MODE_WARN_LACKS_STORAGE_TYPE 0
# define HOST_DOESNT_WANT_FP_OFFSET_TABLES 1
#endif

#else /* NOT macintosh */
# ifdef UNIX_TIME_LIMIT
#  define TIME_LIMIT UNIX_TIME_LIMIT
# endif
#endif

#ifdef TIME_LIMIT
# define VENDOR_NAME "Advanced RISC Machines Limited"
#endif

#ifdef CPLUSPLUS
# ifndef CFRONT_MODE_WARN_LACKS_STORAGE_TYPE
#  define CFRONT_MODE_WARN_LACKS_STORAGE_TYPE 1
# endif
#endif

#define MSG_TOOL_NAME  "armcc"  /* used to load correct NLS message file */

#endif

/* end of cpparm/options.h */
