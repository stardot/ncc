/*
 * C compiler file compiler.h
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1991-92.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef __compiler_h
#define __compiler_h

#include "host.h"
#include "backchat.h"
#define TOOLNAME armcc
#include "toolbox.h"
#include "tooledit.h"

typedef struct {
  backchat_Messenger *send;
  void *handle;
} BackChatHandler;

extern BackChatHandler backchat;
extern ToolEnv *cc_default_env;

int Tool_EditEnv(ToolEnv *t, HWND wh);

int Tool_OrderedEnvEnumerate(
    ToolEnv *t, char const *prefix, ToolEdit_EnumFn *f, void *arg);

bool Tool_Configurable(ToolEnv *t, char const *name);

Uint TE_Integer(ToolEnv *t, char const *name, Uint def);

bool TE_HasValue(ToolEnv *t, char const *name, char const *val);

Uint TE_Count(ToolEnv *t, char const *prefix);

void cc_announce_error(char *s, int severity, char const *file, int32 line);

void UpdateProgress(void);

void TE_DecodeArgumentLine(ToolEnv *t, char const *s, bool ignoreerrors);

void TE_NormaliseEtc(ToolEnv *t);

/* The filename suffixes subject to inversion under RISC OS/MVS etc.  */
#ifdef FORTRAN
#  define FNAME_INCLUDE_SUFFIXES "f F h H"
#  define FNAME_SUFFIXES "a A f F h H o O s S"
#  define LANG_EXTN 'f'
#  define LANG_UC_EXTN 'F'
#  define LANG_EXTN_STRING "f"
#else
#ifdef PASCAL
#  define FNAME_INCLUDE_SUFFIXES "p P h H"
#  define FNAME_SUFFIXES "a A p P h H o O s S"
#  define LANG_EXTN 'p'
#  define LANG_UC_EXTN 'P'
#  define LANG_EXTN_STRING "p"
#else
#ifdef BCPL
#  define FNAME_INCLUDE_SUFFIXES "b B h H"
#  define FNAME_SUFFIXES "a A b B h H o O s S"
#  define LANG_EXTN 'b'
#  define LANG_UC_EXTN 'B'
#  define LANG_EXTN_STRING "b"
#else
#  define FNAME_INCLUDE_SUFFIXES "c C h H c++ C++ cpp CPP cp CP i I"
#  define FNAME_SUFFIXES "a A c C h H o O s S c++ C++ cpp CPP cp CP"
#  define LANG_EXTN 'c'
#  define LANG_UC_EXTN 'C'
#  define LANG_EXTN_STRING "c"
#endif
#endif
#endif

#include <time.h>    /* for time_t */

extern time_t tmuse_front, tmuse_back;

int ccom(ToolEnv *t, char const *in_file, char const *out_file, char const *list_file, char const *md_file);

extern void driver_abort(char *message);

extern void compiler_exit(int status);

extern bool cistreq(const char *s1, const char *s2); /* s2 must be lower case */
extern bool cistrneq(const char *s1, const char *s2, size_t n);

#ifdef FOR_ACORN
extern bool cplusplus_preprocessing(void);
#endif

extern bool inputfromtty;
#endif
