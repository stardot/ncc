/*
 * C compiler file arm/peeptest.c.
 * Copyright (C) 1992 Advanced Risc Machines Limited. All rights reserved
 * Peepholer testbed.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <stdarg.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "globals.h"
typedef union VRegInt   /* extracted (and pruned) from cgdefs.h */
{   int32 i;
    char *str;
    void *p;
} VRegInt;

int32 pp_pragmavec[26];

int32 procflags, procauxflags; /* NONLEAF used */
RealRegSet regmaskvec; /* bit R_LR */

int32 localcg_debugcount = 2;
int32 config;
int32 pcs_flags;

#include "jopcode.h"

#define DEFINE_A_JOPTABLE 1
typedef int32 RealRegister;

#include "mcdpriv.h"

int32 a_loads_r1(J_OPCODE op)
{
    return (op & J_TABLE_BITS) > J_LAST_JOPCODE ? a_attributes(op) & _a_set_r1:
                                                  loads_r1(op);
}

int32 a_uses_r1(J_OPCODE op)
{
    return (op & J_TABLE_BITS) > J_LAST_JOPCODE ?
             a_attributes(op) & (_a_set_r1 | _a_read_r1) :
             uses_r1(op);
}

int32 a_reads_r1(J_OPCODE op)
{
    return (op & J_TABLE_BITS) > J_LAST_JOPCODE ? a_attributes(op) & _a_read_r1 :
                                                  reads_r1(op);
}

int32 a_uses_r2(J_OPCODE op)
{
    return (op & J_TABLE_BITS) > J_LAST_JOPCODE ? a_attributes(op) & (_a_read_r2 | _a_set_r2) :
                                 uses_r2(op);
}

int32 a_reads_r2(J_OPCODE op)
{
    return (op & J_TABLE_BITS) > J_LAST_JOPCODE ? a_attributes(op) & _a_read_r2 :
                                 reads_r2(op);
}

int32 a_loads_r2(J_OPCODE op)
{
    return (op & J_TABLE_BITS) > J_LAST_JOPCODE ? a_attributes(op) & _a_set_r2 :
                                 loads_r2(op);
}

int32 a_uses_r3(J_OPCODE op)
{
    return (op & J_TABLE_BITS) > J_LAST_JOPCODE ? a_attributes(op) & _a_read_r3 :
                                 uses_r3(op);
}

int32 a_uses_r4(J_OPCODE op, int32 peep)
{
   return (peep & P_RSHIFT) || (op == J_MLA) || (op == J_CALLI);
}

int32 a_modifies_mem(J_OPCODE op)
{
    return (op & J_TABLE_BITS) > J_LAST_JOPCODE ? a_attributes(op) & _a_modify_mem :
                                  stores_r1(op) ? 1 :
                                 op == J_PUSHM || op == J_PUSHF || op == J_PUSHD ||
                                 op == J_MOVC || op == J_CLRC;
}

#define a_pseudo_reads_r1(op) ((op) <= J_LAST_JOPCODE && pseudo_reads_r1(op))


bool setspsr(J_OPCODE op, int32 m)
{
    return ( a_pseudo_reads_r1(op)
#ifdef TARGET_HAS_BLOCKMOVE
             || ((op == J_MOVC || op == J_CLRC || op == J_PUSHC) &&
                 m > MOVC_LOOP_THRESHOLD)
#endif
           );

}

int32 power_of_two(int32 m) {
    int32 r = 0;
    while (!(m & 1)) { m = m>>1; r++; }
    return r;
}

#define ROR(x, n) (((x)<<(32L-(n))) | (((x)>>(n)) & ((1L<<(32L-(n)))-1L)))

int32 eightbits(int32 n)
{
/* If n fits in an ARM immediate field this function returns a 12-bit    */
/* quantity suitable for use there. Otherwise it returns -1              */
    int32 shift;
    for (shift = 0; shift<0x1000; shift += 0x100)
    {   if ((n&0xffffff00)==0) return(shift|n);
        n = ROR(n, 30);
    }
    return(-1);
}

void show_inst_direct(PendingOp *p) {
  IGNORE(p);
}

extern void syserr(char *, ...);
void syserr(char *s, ...) {
  va_list ap;
  va_start(ap, s);
  fprintf(stderr, "fatal error: ");
  vfprintf(stderr, s, ap);
  va_end(ap);
  exit(1);
}

void cc_msg(char *s, ...) {
  va_list ap;
  va_start(ap, s);
  vfprintf(stderr, s, ap);
  va_end(ap);
}

int32 bitcount(int32 n)
{
/* return the number of bits present in the integer n.                   */
    int32 r = 0;
    while (n!=0) n ^= n & (-n), r++;
    return(r);
}

int32 sysdebugmask = -1;

int arthur_module = 0;

extern void *SynAlloc(size_t);

void *SynAlloc(size_t n) { return malloc(n); }

extern void pr_stringsegs(StringSegList *z);

void pr_stringsegs(StringSegList *z)
{   putc('"', stderr);
    for (; z!=NULL; z = z->strsegcdr)
    {   char *s = z->strsegbase;
        int32 len = z->strseglen, i;
        for (i=0; i<len; i++)
        {   int ch = ((unsigned char *)s)[i];   /* for isprint */
            if (isprint(ch)) putc(ch, stderr);
            else if (ch=='\n') fprintf(stderr, "\\n");
            else fprintf(stderr, "\\%lo", (long)ch);
        }
    }
    putc('"', stderr);
}

extern bool is_exit_label(void *);

bool is_exit_label(void *l) { return ((int)l) < 0; }

typedef struct {
  J_OPCODE op;
  int32 r1, r2, r3;
  int32 df;
} Op;

#define skipsp(f, ch) { while (isspace(ch)) ch = fgetc(f); }

static int ReadN(FILE *f, int ch, int32 *p) {
  char b[16];
  int i = 0;
  if (ch == ',') ch = fgetc(f);
  skipsp(f, ch);
  if (ch == '-') { b[i++] = ch; ch = fgetc(f); }
  if (ch == '&') {
    b[i++] = '0'; b[i++] = 'x'; ch = fgetc(f); }
  else if (ch == '0') {
    b[i++] = ch; ch = fgetc(f);
    if (ch == 'x' || ch == 'X') { b[i++] = ch; ch = fgetc(f); }
  }
  while (isxdigit(ch)) { b[i++] = ch; ch = fgetc(f); }
  b[i] = 0;
  sscanf(b, "%li", p);
  return ch;
}

typedef struct { char *name; int32 val; } JopAttr;

static JopAttr attr_table[] = {
  {"EQ", Q_EQ},
  {"NE", Q_NE},
  {"HS", Q_HS},
  {"LO", Q_LO},
  {"HI", Q_HI},
  {"LS", Q_LS},
  {"GE", Q_GE},
  {"LT", Q_LT},
  {"GT", Q_GT},
  {"LE", Q_LE},
  {"AL", Q_AL},
  {"NOT", Q_NOT},
  {"UEQ", Q_UEQ},
  {"UNE", Q_UNE},
  {"UKN", Q_UKN},
  {"VOLATILE", J_VOLATILE},
  {"NEGINDEX", J_NEGINDEX},
  {"SIGNED", J_SIGNED},
  {"UNSIGNED", J_UNSIGNED},
  {0,0}
};

static JopAttr sh_table[] = {
  {"RSHIFT", SHIFT_RIGHT << J_SHIFTPOS},
  {"LSHIFT", 0 << J_SHIFTPOS},
  {"ARSHIFT", (SHIFT_ARITH + SHIFT_RIGHT) << J_SHIFTPOS},
  {0,0}
};

static JopAttr dead_table[] = {
  {"DEAD_R1", J_DEAD_R1},
  {"DEAD_R2", J_DEAD_R2},
  {"DEAD_R3", J_DEAD_R3},
  {"0", 0},
  {0,0}
};

static PendingOp *ReadOp(FILE *f) {
  static PendingOp op;
  int ch = fgetc(f);
  op.op = op.r1 = op.r2 = op.r3 = op.r4 = op.peep = op.dataflow = op.cond = 0;
  skipsp(f, ch);
  if (ch == EOF) return NULL;
  { char b[32];
    int i = 0;
    int32 n = 0;
    while (isalpha(ch)) { b[i++] = toupper(ch); ch = fgetc(f); }
    b[i] = 0;
    while (joptable[n].name != 0 && strcmp(b, joptable[n].name) != 0)
      n++;
    if (joptable[n].name == 0) { fprintf(stderr, "%s??\n", b); n = 0; }
    skipsp(f, ch);
    while (ch == '+') {
      i = 0;
      ch = fgetc(f);
      while (isalpha(ch)) { b[i++] = toupper(ch); ch = fgetc(f); }
      b[i] = 0;
      { JopAttr *p = attr_table;
        int32 a;
        while (p->name != 0 && strcmp(p->name, b) != 0) p++;
        if (p->name != 0)
          a = p->val;
        else {
          p = sh_table;
          while (p->name != 0 && strcmp(p->name, b) != 0) p++;
          if (p->name == 0) {
            fprintf(stderr, "%s??\n", b);
            a = 0;
          } else {
            int32 n;
            a = p->val;
            skipsp(f, ch);
            ch = ReadN(f, ch, &n);
            a |= n << J_SHIFTPOS;
          }
        }
        n |= a;
      }
      skipsp(f, ch);
    }
    op.op = n;
  }
  ch = ReadN(f, ch, &op.r1);
  ch = ReadN(f, ch, &op.r2);
  ch = ReadN(f, ch, &op.m);
  if (ch == ',') ch = fgetc(f);
  if (ch != '\n') {
    skipsp(f, ch);

    op.dataflow = 0;
    for (;;) {
      char b[32];
      int i = 0;
      while (isalnum(ch) || ch == '_') { b[i++] = toupper(ch); ch = fgetc(f); }
      b[i] = 0;
      { JopAttr *p = dead_table;
        while (p->name != 0 && strcmp(p->name, b) != 0) p++;
        if (p->name != 0)
          op.dataflow |= p->val;
        else
          fprintf(stderr, "%s??\n", b);
      }
      if (ch!='+') break;
      ch = fgetc(f);
    }
  }
  ungetc(ch, f);
  return &op;
}

int main(int argc, char **argv) {
  peephole_init();
  peephole_reinit();
  if (argc <= 1) {
    fprintf(stderr, "what file?\n");
  } else {
    FILE *f;
    f = (strcmp(argv[1], "-") == 0) ? stdin :
                                      fopen(argv[1], "r");
    if (f == NULL)
      fprintf(stderr, "can't read \"%s\"\n", argv[1]);
    else {
      PendingOp *p;
      while ((p = ReadOp(f)) != NULL)
        peephole_op(p, 0);
    }
  }
  peephole_tidy();
}
