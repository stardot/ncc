/*
 * C compiler file arm/dwasd.c
 * Copyright:   (C) 1995, Advanced RISC Machines Limited. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

/* Runtime switch between ASD and DWARF format debug table generation */

#include <string.h>

#include "globals.h"
#include "mcdep.h"
#include "mcdpriv.h"
#include "defs.h"

#ifdef TARGET_HAS_MULTIPLE_DEBUG_FORMATS

char dbg_name[] = "ASD or DWARF";
int usrdbgmask;

int32 asd_tableindex(int32 dt_number);
void *asd_notefileline(FileLine fl);
void asd_addcodep(void *debaddr, int32 codeaddr);
bool asd_scope(BindListList *, BindListList *);

void asd_topvar(Symstr *name, int32 addr, TypeExpr *t, int stgclass, FileLine fl);
void asd_type(Symstr *name, TypeExpr *t, FileLine fl);
void asd_proc(Binder *b, TagBinder *parent, bool ext, FileLine fl);
void asd_locvar(Binder *name, FileLine fl);
void asd_locvar1(Binder *name);   /* used by F77 front-end */
void asd_commblock(Binder *name, SynBindList *members, FileLine fl);
void asd_enterproc(void);
void asd_bodyproc(void);
void asd_return(int32 addr);
void asd_xendproc(FileLine fl);
void asd_define(char const *name, bool objectmacro, char const *body,
                dbg_ArgList const *args, FileLine fl);
void asd_undef(char const *name, FileLine fl);
void asd_include(char const *filename, char const *path, FileLine fl);
void asd_notepath(char const *pathname);
void asd_init(void);
void asd_setformat(char const *);
bool asd_needsframepointer(void);

void asd_finalise(void);
void asd_final_src_codeaddr(int32, int32);
#ifdef TARGET_HAS_FP_OFFSET_TABLES
void asd_notefpdesc(ProcFPDesc const *fpd);
#endif
bool asd_debugareaexists(char const *name);
void asd_writedebug(void);

int32 dwarf_tableindex(int32 dt_number);
void *dwarf_notefileline(FileLine fl);
void dwarf_addcodep(void *debaddr, int32 codeaddr);
bool dwarf_scope(BindListList *, BindListList *);

void dwarf_topvar(Symstr *name, int32 addr, TypeExpr *t, int stgclass, FileLine fl);
void dwarf_type(Symstr *name, TypeExpr *t, FileLine fl);
void dwarf_proc(Binder *b, TagBinder *parent, bool ext, FileLine fl);
void dwarf_locvar(Binder *name, FileLine fl);
void dwarf_locvar1(Binder *name);   /* used by F77 front-end */
void dwarf_commblock(Binder *name, SynBindList *members, FileLine fl);
void dwarf_enterproc(void);
void dwarf_bodyproc(void);
void dwarf_return(int32 addr);
void dwarf_xendproc(FileLine fl);
void dwarf_define(char const *name, bool objectmacro, char const *body,
                  dbg_ArgList const *args, FileLine fl);
void dwarf_undef(char const *name, FileLine fl);
void dwarf_include(char const *filename, char const *path, FileLine fl);
void dwarf_notepath(char const *pathname);
void dwarf_init(void);
void dwarf_setformat(char const *);
bool dwarf_needsframepointer(void);

void dwarf_finalise(void);
void dwarf_final_src_codeaddr(int32, int32);
#ifdef TARGET_HAS_FP_OFFSET_TABLES
void dwarf_notefpdesc(ProcFPDesc const *fpd);
#endif
bool dwarf_debugareaexists(char const *name);
void dwarf_writedebug(void);

typedef struct {
  int32 (*tableindex)(int32);
  void *(*notefileline)(FileLine);
  void (*addcodep)(void *, int32);
  bool (*scope)(BindListList *, BindListList *);
  void (*topvar)(Symstr *, int32, TypeExpr *, int, FileLine);
  void (*type)(Symstr *, TypeExpr *, FileLine);
  void (*proc)(Binder *, TagBinder *, bool, FileLine);
  void (*locvar)(Binder *, FileLine);
  void (*locvar1)(Binder *);   /* used by F77 front-end */
  void (*commblock)(Binder *, SynBindList *, FileLine);
  void (*enterproc)(void);
  void (*bodyproc)(void);
  void (*returnp)(int32);
  void (*xendproc)(FileLine);
  void (*define)(char const *, bool, char const *, dbg_ArgList const *, FileLine);
  void (*undef)(char const *, FileLine);
  void (*include)(char const *, char const *, FileLine);
  void (*notepath)(char const *);
  void (*init)(void);
  void (*setformat)(char const *);
  void (*finalise)(void);
  void (*final_src_codeaddr)(int32, int32);
#ifdef TARGET_HAS_FP_OFFSET_TABLES
  void (*notefpdesc)(ProcFPDesc const *);
#endif
  bool (*debugareaexists)(char const *);
  void (*writedebug)(void);
  bool (*needsframepointer)(void);

} Dbg_Procs;

static Dbg_Procs const asd_procs = {
  asd_tableindex,
  asd_notefileline,
  asd_addcodep,
  asd_scope,
  asd_topvar,
  asd_type,
  asd_proc,
  asd_locvar,
  asd_locvar1,
  asd_commblock,
  asd_enterproc,
  asd_bodyproc,
  asd_return,
  asd_xendproc,
  asd_define,
  asd_undef,
  asd_include,
  asd_notepath,
  asd_init,
  asd_setformat,
  asd_finalise,
  asd_final_src_codeaddr,
#ifdef TARGET_HAS_FP_OFFSET_TABLES
  asd_notefpdesc,
#endif
  asd_debugareaexists,
  asd_writedebug,
  asd_needsframepointer

};

static Dbg_Procs const dwarf_procs = {
  dwarf_tableindex,
  dwarf_notefileline,
  dwarf_addcodep,
  dwarf_scope,
  dwarf_topvar,
  dwarf_type,
  dwarf_proc,
  dwarf_locvar,
  dwarf_locvar1,
  dwarf_commblock,
  dwarf_enterproc,
  dwarf_bodyproc,
  dwarf_return,
  dwarf_xendproc,
  dwarf_define,
  dwarf_undef,
  dwarf_include,
  dwarf_notepath,
  dwarf_init,
  dwarf_setformat,
  dwarf_finalise,
  dwarf_final_src_codeaddr,
#ifdef TARGET_HAS_FP_OFFSET_TABLES
  dwarf_notefpdesc,
#endif
  dwarf_debugareaexists,
  dwarf_writedebug,
  dwarf_needsframepointer
};

static Dbg_Procs const *procs;

int32 dbg_tableindex(int32 dt_number) {
  return procs->tableindex(dt_number);
}

void *dbg_notefileline(FileLine fl) {
  return procs->notefileline(fl);
}

void dbg_addcodep(void *debaddr, int32 codeaddr) {
  procs->addcodep(debaddr, codeaddr);
}

bool dbg_scope(BindListList *newbl, BindListList *oldbl) {
  return procs->scope(newbl, oldbl);
}

void dbg_topvar(Symstr *name, int32 addr, TypeExpr *t, int stgclass, FileLine fl) {
  procs->topvar(name, addr, t, stgclass, fl);
}

void dbg_type(Symstr *name, TypeExpr *t, FileLine fl) {
  procs->type(name, t, fl);
}

void dbg_proc(Binder *b, TagBinder *parent, bool ext, FileLine fl) {
  procs->proc(b, parent, ext, fl);
}

void dbg_locvar(Binder *name, FileLine fl) {
  procs->locvar(name, fl);
}

void dbg_locvar1(Binder *name) {
  procs->locvar1(name);
}

void dbg_commblock(Binder *name, SynBindList *members, FileLine fl) {
  procs->commblock(name, members, fl);
}

void dbg_enterproc(void) {
  procs->enterproc();
}

void dbg_bodyproc(void) {
  procs->bodyproc();
}

void dbg_return(int32 addr) {
  procs->returnp(addr);
}

void dbg_xendproc(FileLine fl) {
  procs->xendproc(fl);
}

void dbg_define(char const *name, bool objectmacro, char const *body,
                dbg_ArgList const *args, FileLine fl) {
  procs->define(name, objectmacro, body, args, fl);
}

void dbg_undef(char const *name, FileLine fl) {
  procs->undef(name, fl);
}

void dbg_include(char const *filename, char const *path, FileLine fl) {
  procs->include(filename, path, fl);
}

void dbg_notepath(char const *pathname) {
  procs->notepath(pathname);
}

void dbg_init(void) {
  procs->init();
}

void dbg_finalise(void) {
  procs->finalise();
}

void dbg_final_src_codeaddr(int32 a, int32 b) {
  procs->final_src_codeaddr(a, b);
}

#ifdef TARGET_HAS_FP_OFFSET_TABLES
void obj_notefpdesc(ProcFPDesc const *fpd) {
  procs->notefpdesc(fpd);
}
#endif

bool dbg_debugareaexists(char const *name) {
  return procs->debugareaexists(name);
}

void dbg_writedebug(void) {
  procs->writedebug();
}

bool dbg_needsframepointer() {
  return procs->needsframepointer();
}

void dbg_setformat(char const *form) {
  if (StrnEq(form, "=-dwarf", 7)) {
    procs = &dwarf_procs;
    procs->setformat(&form[7]);
  } else {
    procs = &asd_procs;
    procs->setformat(&form[5]);
  }
}

#endif  /* TARGET_HAS_MULTIPLE_DEBUG_FORMATS */
