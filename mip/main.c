/*
 * main.c
 * The top level module of the compiler.
 * Copyright (C) 1996 Advanced RISC Machines Limited. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#include "host.h"
#include "globals.h"
#include "compiler.h"
#if defined(FOR_ACORN) && defined(COMPILING_ON_RISCOS)
#include "dde.h"
#endif
#define TOOLNAME armcc
#include "toolbox.h"

/* DEPEND_FORMAT used to output dependency line (-m option) */
#ifdef COMPILING_ON_MACINTOSH
#  define DEPEND_FORMAT     "%s\304\t%s\n"
#else
#  define DEPEND_FORMAT     "%s:\t%s\n"
#endif

static FILE *errorstream;

static void ErrorMessage(backchat_Diagnostic const *diag) {
  char const *msg = diag->msgtext;
  unsigned line = diag->lineno;
  char b[256];
  cc_announce_error(b, diag->severity, diag->filename, line);

#ifdef HOST_IS_RISCOS
  if (dde_throwback_flag != 0)
    switch(diag->severity) {
    case BC_SEVERITY_WARN:    dde_throwback_send(THROWBACK_WARN, line, msg); break;
    case BC_SEVERITY_ERROR:   dde_throwback_send(THROWBACK_ERROR, line, msg); break;
    case BC_SEVERITY_SERIOUS: dde_throwback_send(THROWBACK_SERIOUS, line, msg); break;
    }
#endif

  fputs(b, errorstream);
  fputs(msg, errorstream);
}

static int BC(void *handle, unsigned code, const void *msg) {
  switch (code)
  {
case BC_DIAGMSG:
      if (handle != NULL) errorstream = handle;
      ErrorMessage((backchat_Diagnostic const *)msg);
      break;
case BC_INCLUDEMSG:
      { const backchat_InclusionDependency *dep =
            (const backchat_InclusionDependency *)msg;
        fprintf((FILE *)handle, DEPEND_FORMAT,
                dep->targetName, dep->dependsonName);
      }
      break;
case BC_NULLMSG:
      break;
  }
  return 0;
}

int Tool_EditEnv(ToolEnv *t, HWND wh) {
  IGNORE(t); IGNORE(wh);
  return 0;
}

bool Tool_Configurable(ToolEnv *t, char const *name) {
  IGNORE(t); IGNORE(name);
  return YES;
}

int main(int argc, ArgvType *argv) {
  ToolEntryPoints const *ep;
  ToolEnv *env;
  int status;
  errorstream = stderr;
  ep = armccinit();
  env = ep->toolenv_new();
  ep->toolenv_mark(env);  /* In case of -config argument */
  ep->toolenv_merge(env, "*");
  status = ep->toolbox_main(argc, argv, env, BC, NULL);
  if (errorstream != NULL && errorstream != stderr)
    fclose(errorstream);
  ep->toolenv_dispose(env);
  ep->toolbox_finalise(ep);
  exit(status);
}
