/*
 * mip/cseeval.c: CSE compile-time evaluation
 * Copyright (C) Acorn Computers Ltd., 1988.
 * Copyright 1991-1997 Advanced Risc Machines Limited. All rights reserved
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$ Codemist 0
 * Checkin $Date$
 * Revising $Author$
 */

#include "globals.h"
#include "aeops.h"
#include "cgdefs.h"
#include "jopcode.h"
#include "builtin.h"   /* for sim */
#include "errors.h"
#include "cseguts.h"
#include "ieeeflt2.h"
#include "aetree.h"
#include "sem.h"

static bool IsLibraryFunction(Expr const *fn, Symstr const *sym) {
    return (bindstg_((Binder const *)fn) & bitofstg_(s_extern))
           && bindsym_((Binder const *)fn) == sym;
}

static bool sdiv(int32 *resp, int32 a0, int32 a1) {
/* nb arguments reversed from natural order */
    if (a0 == 0) { cc_warn(sem_warn_divrem_0, s_div); return NO; }
    *resp = a1 / a0;
    return YES;
}

static bool srem(int32 *resp, int32 a0, int32 a1) {
/* nb arguments reversed from natural order */
    if (a0 == 0) { cc_warn(sem_warn_divrem_0, s_rem); return NO; }
    *resp = a1 % a0;
    return YES;
}

static bool udiv(int32 *resp, int32 a0, int32 a1) {
/* nb arguments reversed from natural order */
    a0 = just32bits_(a0); a1 = just32bits_(a1);
    if (a0 == 0) { cc_warn(sem_warn_divrem_0, s_div); return NO; }
    *resp = (int32)(((unsigned32)a1) / a0);
    return YES;
}

static bool urem(int32 *resp, int32 a0, int32 a1) {
/* nb arguments reversed from natural order */
    a0 = just32bits_(a0); a1 = just32bits_(a1);
    if (a0 == 0) { cc_warn(sem_warn_divrem_0, s_rem); return NO; }
    *resp = (int32)(((unsigned32)a1) % a0);
    return YES;
}


#ifndef TARGET_HAS_MULTIPLY
static bool mul(int32 *resp, int32 a0, int32 a1) {
    *resp = a0 * a1;
    return YES;
}
#endif

FloatCon *CSE_NewDCon(DbleBin const *val) {
  FloatCon *fc = real_of_string("<expr>", 0);
  fc->floatlen = ts_double;
  fc->floatbin.db = *val;
  return CSE_CanonicalFPConst(fc);
}

static FloatCon *D_Evaluated(DbleBin const *dr) {
  FloatCon *a = CSE_NewDCon(dr);
  if (debugging(DEBUG_CSE) && CSEDebugLevel(1))
    cc_msg("Compile-time evaluable\n");
  return a;
}

FloatCon *CSE_NewFCon(FloatBin const *val) {
  FloatCon *fc = real_of_string("<expr>", 0);
  fc->floatlen = ts_float;
  fc->floatbin.fb = *val;
  return CSE_CanonicalFPConst(fc);
}

static FloatCon *F_Evaluated(FloatBin const *fr) {
  FloatCon *a = CSE_NewFCon(fr);
  if (debugging(DEBUG_CSE) && CSEDebugLevel(1))
    cc_msg("Compile-time evaluable\n");
  return a;
}

Int64Con *CSE_NewLLCon(int64 const *ip) {
  Int64Con *lc = mkint64const(0, ip);
  return CSE_CanonicalLLConst(lc);
}

FloatCon *CSE_EvalUnary_F(J_OPCODE op, ExSet *ex) {
  J_OPCODE baseop = op & J_TABLE_BITS;
  ExSet *c1;
  DbleBin dr; FloatBin fr;
  bool done = NO;
  if ((c1 = MOVFKinSet(ex)) != NULL) {
    DbleBin d;
    if (baseop == J_NEGFR) {
      fltrep_widen(&e1f_(c1->exprn)->floatbin.fb, &d);
      if (flt_negate(&dr, &d) <= flt_ok)
        done = YES;
    }
  } else if ((c1 = MOVKinSet(ex)) != NULL) {
    if (baseop == J_FLTFR
        && flt_itod(&dr, e1k_(c1->exprn)) <= flt_ok)
      done = YES;
  }
  return (done && fltrep_narrow(&dr, &fr) <= flt_ok) ? F_Evaluated(&fr) : NULL;
}

FloatCon *CSE_EvalUnary_D(J_OPCODE op, ExSet *ex) {
  J_OPCODE baseop = op & J_TABLE_BITS;
  ExSet *c1;
  DbleBin dr;
  bool done = NO;
  if ((c1 = MOVDKinSet(ex)) != NULL) {
    if (baseop == J_NEGDR
        && flt_negate(&dr, &e1f_(c1->exprn)->floatbin.db) <= flt_ok)
      done = YES;

  } else if ((c1 = MOVKinSet(ex)) != NULL) {
    if (baseop == J_FLTDR
        && flt_itod(&dr, e1k_(c1->exprn)) <= flt_ok)
      done = YES;
  }
  return (done) ? D_Evaluated(&dr) : NULL;
}

#if defined TARGET_HAS_SCALED_ADDRESSING || defined TARGET_HAS_SCALED_OPS || \
    defined TARGET_HAS_SCALED_ADD
int32 ShiftedVal(J_OPCODE op, int32 b)
{
    if (OpIsShifted(op)) {
        int32 msh = (op & J_SHIFTMASK) >> J_SHIFTPOS;
        if ((msh & SHIFT_RIGHT) == 0)
            b = b << (msh & SHIFT_MASK);
        else if (msh & SHIFT_ARITH)
            b = b >> (msh & SHIFT_MASK);
        else
            b = (int32) ((just32bits_((unsigned32)b)) >> (msh & SHIFT_MASK));
    }
    if (op & J_NEGINDEX) b = -b;
    return widen32bitint_(b);
}
#endif

bool CSE_EvalUnary_I(J_OPCODE op, int32 *resp, ExSet *ex) {
  J_OPCODE baseop = op & J_TABLE_BITS;
  ExSet *c1 = MOVKinSet(ex);
  int32 n; bool done = NO;
  if (c1 != NULL) {
    n = ShiftedVal(op, e1k_(c1->exprn));
    switch (baseop) {
    case J_NOTR: n = ~n; break;
    case J_MOVR: n =  n; break;
    case J_NEGR: n = -n; break;
    }
    done = YES;
  } else if (baseop == J_FIXFR && (c1 = MOVFKinSet(ex)) != NULL) {
    DbleBin d;
    fltrep_widen(&e1f_(c1->exprn)->floatbin.fb, &d);
    { int status = (op & J_UNSIGNED) ? flt_dtou((unsigned32 *)&n, &d) :
                                       flt_dtoi(&n, &d);
      done = status == flt_ok;
    }
  } else if (baseop == J_FIXDR && (c1 = MOVDKinSet(ex)) != NULL) {
    DbleBin *d = &e1f_(c1->exprn)->floatbin.db;
    { int status = (op & J_UNSIGNED) ? flt_dtou((unsigned32 *)&n, d) :
                                       flt_dtoi(&n, d);
      done = status == flt_ok;
    }
  }
  /* TODO: add FIXFRM / FIXDRM handling code here */

  if (done) {
    if (debugging(DEBUG_CSE) && CSEDebugLevel(1))
      cc_msg("Compile-time evaluable = %ld\n", n);
    *resp = widen32bitint_(n);
  }
  return done;
}

bool CSE_EvalBinary_I(J_OPCODE op, int32 *resp, Exprn *ax, int32 b)
{
    int32 a, res;
    bool done = YES;
    if (exop_(ax) != J_MOVK) return NO;
    a = e1k_(ax);
    switch (op & J_TABLE_BITS) {
    case J_EXTEND:if (b == 0 || b == 1)
                      res = (a & 0x80) ? a | ~0x7f : a & 0x7f;
                  else
                      res = (a & 0x8000) ? a | ~0x7fff : a & 0x7fff;
                  break;
    case J_ADDK: res = a + b; break;
    case J_MULK: res = a * b; break;
    case J_ANDK: res = a & b; break;
    case J_ORRK: res = a | b; break;
    case J_EORK: res = a ^ b; break;
    case J_SUBK: res = a - b; break;
    case J_RSBK: res = b - a; break;

    case J_DIVK: done = op & J_UNSIGNED ? udiv(&res, b, a) : sdiv(&res, b, a);
                 break;
    case J_REMK: done = op & J_UNSIGNED ? urem(&res, b, a) : srem(&res, b, a);
                 break;

#ifdef TARGET_LACKS_RIGHTSHIFT
    case J_SHLK: if (b >= 0) { res = a << b; break; }
                 b = -b;
                 /* fall through to shrk */
#else
    case J_SHLK: res = a << b; break;
#endif
    case J_SHRK: res = (op & J_UNSIGNED) ?
                        (int32) ((0xffffffff & (unsigned32) a) >> b) :
                        TARGET_RIGHTSHIFT(a, b);
                 break;
#ifdef TARGET_HAS_ROTATE
/* Hmm, ROLK is probably more common than RORK.                         */
    case J_RORK: res = ((unsigned32)a << (32-b)) |
                        ((0xffffffff & (unsigned32)a) >> b);
                 break;
#endif
    default: syserr(syserr_evalconst, (long)op);
             return NO;
    }
    res = widen32bitint_(res);
    if (done) *resp = res;
    if (done && debugging(DEBUG_CSE) && CSEDebugLevel(1))
        cc_msg("Compile-time evaluable = %ld\n", (long int)res);

    return done;
}

FloatCon *CSE_EvalBinary_D(J_OPCODE op, ExSet *as, FloatCon const *b)
{
    FloatCon *a;
    DbleBin db;
    int status;
    as = MOVDKinSet(as);
    if (as == NULL) return NULL;
    a = e1f_(as->exprn);
    switch (op) {
    case J_ADDDK: status = flt_add(&db, &a->floatbin.db, &b->floatbin.db); break;
    case J_MULDK: status = flt_multiply(&db, &a->floatbin.db, &b->floatbin.db); break;
    case J_SUBDK: status = flt_subtract(&db, &a->floatbin.db, &b->floatbin.db); break;
    case J_DIVDK: status = flt_divide(&db, &a->floatbin.db, &b->floatbin.db); break;
    default: syserr(syserr_evalconst, (long)op);
             return NULL;
    }
    return (status > flt_ok) ? NULL : D_Evaluated(&db);
}

static bool EvalBinary_F_1(J_OPCODE op, FloatBin const *a, FloatBin const *b, FloatBin *r) {
    DbleBin dr, da, db;
    int status;
    fltrep_widen(a, &da);
    fltrep_widen(b, &db);
    switch (op) {
    case J_ADDFK: status = flt_add(&dr, &da, &db); break;
    case J_MULFK: status = flt_multiply(&dr, &da, &db); break;
    case J_SUBFK: status = flt_subtract(&dr, &da, &db); break;
    case J_DIVFK: status = flt_divide(&dr, &da, &db); break;
    default: syserr(syserr_evalconst, (long)op);
             return NO;
    }
    if (status > flt_ok) return NO;
    if (fltrep_narrow(&dr, r) > flt_ok) return NO;
    return YES;
}

FloatCon *CSE_EvalBinary_F(J_OPCODE op, ExSet *as, FloatCon const *b)
{
    FloatBin fr;
    as = MOVFKinSet(as);
    if (as == NULL) return NULL;
    { FloatCon *a = e1f_(as->exprn);
      return EvalBinary_F_1(op, &a->floatbin.fb, &b->floatbin.fb, &fr) ? F_Evaluated(&fr) :
                                                                         NULL;
    }
}

bool CSE_Compare_F(int *res, FloatBin const *a, FloatBin const *b) {
/* Currently, flt_compare() can return only -1, 0, or 1 (all argument values
   are considered comparable). Code here anticipates an interface change which
   allows a 'not comparable' return value outside this set.
 */
    DbleBin da, db; int r;
    fltrep_widen(a, &da);
    fltrep_widen(b, &db);
    r = flt_compare(&da, &db);
    if (!(-1 <= r && r <= 1)) return NO;
    *res = r;
    return YES;
}

bool CSE_Compare_D(int *res, FloatCon const *a, FloatCon const *b) {
    int r = flt_compare(&a->floatbin.db, &b->floatbin.db);
    if (!(-1 <= r && r <= 1)) return NO;
    *res = r;
    return YES;
}

#define RemoveInexact(s) ((s) == flt_inexact ? flt_bad : (s))

static int dpow(DbleBin *res, DbleBin const *a0, DbleBin const *a1) {
    int32 n1;
    int status;
    DbleBin t;
    if ((status = flt_dtoi(&n1, a1)) != flt_ok) return RemoveInexact(status);
    if ((status = flt_itod(&t, n1)) != flt_ok) return RemoveInexact(status);
    if (flt_compare(&t, a1) != 0) return flt_bad;
    if (!(-256 <= n1 && n1 <= 256)) return flt_bad;
    if (n1 == 0) {
        flt_itod(&t, 0);
        if (flt_compare(a0, &t) == 0) return flt_invalidop;
        return flt_itod(res, 1);
    }
    {   int32 bit = 256;
        bool invert = NO;
        if (n1 < 0) {
            invert = YES;
            n1 = -n1;
        }
        *res = *a0;
        while (!(bit & n1)) bit = bit >> 1;
        bit = bit >> 1;
        for (; bit != 0; bit = bit >> 1) {
            if ((status = flt_multiply(res, res, res)) != flt_ok)
                return RemoveInexact(status);
            if (bit & n1)
                if ((status = flt_multiply(res, res, a0)) != flt_ok)
                    return RemoveInexact(status);
        }
        if (invert) {
            flt_itod(&t, 1);
            return flt_divide(res, &t, res);
        } else
            return flt_ok;
    }
}

static Exprn *AdconXInSet(ExSet *s, int32 n, J_OPCODE adcon) {
    for (;; s = cdr_(s)) {
      Location *loc;
      s = ExSet_OpMember(s, J_LDRK, J_ALIGNMENT);
      if (s == NULL) return NULL;
      loc = exloc_(s->exprn);
      if (!(loctype_(loc) & LOC_anyVAR) &&
          (exop_(locbase_(loc)) & ~J_BASEALIGN4) == adcon)
        return locoff_(loc) == n ? locbase_(loc) : NULL;
    }
}

static Exprn *ConstantWordxInSet(ExSet *s, J_OPCODE wordx) {
    for (; s != NULL && (s = ExSet_OpMember(s, wordx, 0)) != NULL; s = cdr_(s))
        if (exop_(e1_(s->exprn)) == J_MOVDK) return e1_(s->exprn);
    return NULL;
}

static FloatCon *DoubleLoadedToIntPair(ExSet *a0s, ExSet *a1s) {
    if (software_doubles_enabled) {
        Exprn *adcond = AdconXInSet(a0s, 0, J_ADCOND);
        if (adcond != NULL && AdconXInSet(a1s, 4, J_ADCOND) == adcond)
            return e1f_(adcond);
        adcond = AdconXInSet(a0s, 0, J_ADCON);
        if (adcond != NULL) {
            Binder *b = e1b_(adcond);
            Expr *ex = bindconst_(b);
            if (ex != NULL && h0_(ex) == s_floatcon) {
                SET_BITMAP t = exf_(ex)->floatlen & ~CVBITS;
                if ((t == ts_double || t == ts_longdouble)
                    && AdconXInSet(a1s, 4, J_ADCON) == adcond)
                    return CSE_CanonicalFPConst(exf_(ex));
            }
        }
        return NULL;
    }
    {   Exprn *e0 = ConstantWordxInSet(a0s, CSE_WORD1);
        if (e0 != NULL) {
            Exprn *e1 = ConstantWordxInSet(a1s, CSE_WORD2);
            if (e0 == e1) return e1f_(e0);
        }
    }
    return NULL;
}

static Int64Con *LongLongLoadedToIntPair(ExSet *a0s, ExSet *a1s) {
    Exprn *adconll = AdconXInSet(a0s, 0, J_ADCONLL);
    if (adconll != NULL && AdconXInSet(a1s, 4, J_ADCONLL) == adconll)
        return e1i64_(adconll);
    adconll = AdconXInSet(a0s, 0, J_ADCON);
    if (adconll != NULL) {
        Binder *b = e1b_(adconll);
        Expr *ex = bindconst_(b);
        if (ex != NULL && h0_(ex) == s_int64con
            && AdconXInSet(a1s, 4, J_ADCON) == adconll)
            return CSE_CanonicalLLConst(exi64_(ex));
    }
    return NULL;
}

static I64_Status i64_rsb(int64 *res, int64 const *a1, int64 const *a2) {
    return I64_SSub(res, a2, a1);
}

static I64_Status i64_sdiv(int64 *res, int64 const *a1, int64 const *a2) {
    int64 t;
    return I64_SDiv(res, &t, a1, a2);
}

static I64_Status i64_srdv(int64 *res, int64 const *a1, int64 const *a2) {
    int64 t;
    return I64_SDiv(res, &t, a2, a1);
}

static I64_Status i64_srem(int64 *res, int64 const *a1, int64 const *a2) {
    int64 t;
    return I64_SDiv(&t, res, a1, a2);
}

static I64_Status i64_srrem(int64 *res, int64 const *a1, int64 const *a2) {
    int64 t;
    return I64_SDiv(&t, res, a2, a1);
}

static I64_Status i64_udiv(int64 *res, int64 const *a1, int64 const *a2) {
    uint64 t;
    return I64_UDiv((uint64 *)res, &t, (uint64 const *)a1, (uint64 const *)a2);
}

static I64_Status i64_urdv(int64 *res, int64 const *a1, int64 const *a2) {
    uint64 t;
    return I64_UDiv((uint64 *)res, &t, (uint64 const *)a2, (uint64 const *)a1);
}

static I64_Status i64_urem(int64 *res, int64 const *a1, int64 const *a2) {
    uint64 t;
    return I64_UDiv(&t, (uint64 *)res, (uint64 const *)a1, (uint64 const *)a2);
}

static I64_Status i64_urrem(int64 *res, int64 const *a1, int64 const *a2) {
    uint64 t;
    return I64_UDiv(&t, (uint64 *)res, (uint64 const *)a2, (uint64 const *)a1);
}

static I64_Status i64_not(int64 *res, int64 const *a1) {
    I64_Not(res, a1); return i64_ok;
}

static I64_Status i64_and(int64 *res, int64 const *a1, int64 const *a2) {
    I64_And(res, a2, a1); return i64_ok;
}

static I64_Status i64_or(int64 *res, int64 const *a1, int64 const *a2) {
    I64_Or(res, a2, a1); return i64_ok;
}

static I64_Status i64_eor(int64 *res, int64 const *a1, int64 const *a2) {
    I64_Eor(res, a2, a1); return i64_ok;
}

#define Arg_D  0x10
#define Arg_DD 0x20
#define Arg_L  0x30
#define Arg_LL 0x40
#define Arg_LI 0x50
#define Res_D  0x60
#define Res_L  0x70
#define Arg_F  0x80

#define ArgCount 0xf

typedef union {
    int (*dunaryu)(DbleBin *res, uint32 n);
    int (*dunaryi)(DbleBin *res, int32 n);
    int (*unaryd)(DbleBin *res, DbleBin const *a1);
    int (*binaryd)(DbleBin *res, DbleBin const *a1, DbleBin const *a2);
    void (*fwiden)(FloatBin const *a1, DbleBin *res);
    I64_Status (*lunaryu)(int64 *res, uint32 n);
    I64_Status (*lunaryi)(int64 *res, int32 n);
    I64_Status (*unaryl)(int64 *res, int64 const *a1);
    I64_Status (*binaryll)(int64 *res, int64 const *a1, int64 const *a2);
    I64_Status (*binaryli)(int64 *res, int64 const *a1, Uint a2);
    I64_Status (*binarylui)(uint64 *res, uint64 const *a1, Uint a2);
} FPEvalFn;

static int32 EvaluableDoubleValuedFn(Expr const *fn, VRegnum res, FPEvalFn *ev)
{
    if (IsLibraryFunction(fn, sim.dabs))   { ev->unaryd = flt_abs;   return 2+Arg_D; }
    if (IsLibraryFunction(fn, sim.dfloor)) { ev->unaryd = flt_floor; return 2+Arg_D; }
    if (IsLibraryFunction(fn, sim.dceil))  { ev->unaryd = flt_ceil;  return 2+Arg_D; }

    if (IsLibraryFunction(fn, sim.dpow))   { ev->binaryd = dpow;     return 4+Arg_DD; }
    if (IsLibraryFunction(fn, sim.dmod))   { ev->binaryd = flt_fmod; return 4+Arg_DD; }

    if (fn == sim.dfloat)    { ev->dunaryi = flt_itod;     return 1+Res_D; }
    if (fn == sim.dfloatu)   { ev->dunaryu = flt_utod;     return 1+Res_D; }
    if (fn == sim.dwiden)    { ev->fwiden = fltrep_widen;  return 1+Arg_F; }

    if (fn == sim.dnegate)   { ev->unaryd = flt_negate;    return 2+Arg_D; }

    if (fn == sim.dadd)      { ev->binaryd = flt_add;      return 4+Arg_DD; }
    if (fn == sim.dsubtract) { ev->binaryd = flt_subtract; return 4+Arg_DD; }
    if (fn == sim.dmultiply) { ev->binaryd = flt_multiply; return 4+Arg_DD; }
    if (fn == sim.ddivide)   { ev->binaryd = flt_divide;   return 4+Arg_DD; }

    if (fn == sim.llnot)     { ev->unaryl   = i64_not;  return 2+Arg_L; }
    if (fn == sim.llneg)     { ev->unaryl   = I64_Neg;  return 2+Arg_L; }

    if (fn == sim.lladd)     { ev->binaryll = I64_SAdd; return 4+Arg_LL; }
    if (fn == sim.llsub)     { ev->binaryll = I64_SSub; return 4+Arg_LL; }
    if (fn == sim.llrsb)     { ev->binaryll = i64_rsb;  return 4+Arg_LL; }
    if (fn == sim.llmul)     { ev->binaryll = I64_SMul; return 4+Arg_LL; }
    if (fn == sim.llsdiv)    { ev->binaryll = res == R_A1 ? i64_sdiv : i64_srem;  return 4+Arg_LL; }
    if (fn == sim.llsrdv)    { ev->binaryll = res == R_A1 ? i64_srdv : i64_srrem; return 4+Arg_LL; }
    if (fn == sim.lludiv)    { ev->binaryll = res == R_A1 ? i64_udiv : i64_urem;  return 4+Arg_LL; }
    if (fn == sim.llurdv)    { ev->binaryll = res == R_A1 ? i64_urdv : i64_urrem; return 4+Arg_LL; }

    if (fn == sim.lland)     { ev->binaryll = i64_and;  return 4+Arg_LL; }
    if (fn == sim.llor)      { ev->binaryll = i64_or;   return 4+Arg_LL; }
    if (fn == sim.lleor)     { ev->binaryll = i64_eor;  return 4+Arg_LL; }

    if (fn == sim.llfroml)   { ev->lunaryi  = I64_IToS; return 1+Res_L; }

    if (fn == sim.llshiftl)  { ev->binaryli = I64_Lsh;  return 3+Arg_LI; }
    if (fn == sim.llushiftr) { ev->binarylui = I64_URsh; return 3+Arg_LI; }
    if (fn == sim.llsshiftr) { ev->binaryli = I64_SRsh; return 3+Arg_LI; }

    return 0;
}

bool CSE_EvaluableDoubleValuedCall(
  Expr const *fn, VRegnum res, int32 argres, ExSet *arg[], DRes *dresp) {
    FPEvalFn op;
    int32 args = EvaluableDoubleValuedFn(fn, res, &op);
    bool done = NO;
    if (args == 0) return NO;
    if (argres & K_RESULTINFLAGS)
        return NO;
    if ((args & ArgCount) == k_argregs_(argres))
      switch(args) {
      case 1+Res_D:
        if (!k_argisfp_(argres, 0) && arg[0] != NULL) {
          ExSet *c0 = MOVKinSet(arg[0]);
          done = (c0 != NULL && op.dunaryi(&dresp->val.d, e1k_(c0->exprn)) <= flt_ok);
          dresp->isdouble = YES;
        }
        break;
      case 1+Arg_F:
        if (!k_argisfp_(argres, 0) && arg[0] != NULL) {
          ExSet *c0 = MOVKinSet(arg[0]);
          if (c0 != NULL) {
            FloatBin fb; fb.val = e1k_(c0->exprn);
            op.fwiden(&fb, &dresp->val.d);
            done = dresp->isdouble = YES;
          }
        }
        break;
      case 2+Arg_D:
        if (   !k_argisfp_(argres, 0) && arg[0] != NULL
            && !k_argisfp_(argres, 1) && arg[1] != NULL)
        { FloatCon *a0 = DoubleLoadedToIntPair(arg[0], arg[1]);
          done = (a0 != NULL && op.unaryd(&dresp->val.d, &a0->floatbin.db) <= flt_ok);
          dresp->isdouble = YES;
        }
        break;
      case 4+Arg_DD:
        if (   !k_argisfp_(argres, 0) && arg[0] != NULL
            && !k_argisfp_(argres, 1) && arg[1] != NULL
            && !k_argisfp_(argres, 2) && arg[2] != NULL
            && !k_argisfp_(argres, 3) && arg[3] != NULL)
        { FloatCon *a0 = DoubleLoadedToIntPair(arg[0], arg[1]);
          FloatCon *a1 = DoubleLoadedToIntPair(arg[2], arg[3]);
          done = (a0 != NULL && a1 != NULL
                  && op.binaryd(&dresp->val.d, &a0->floatbin.db, &a1->floatbin.db) <= flt_ok);
          dresp->isdouble = YES;
        }
        break;
      case 1+Res_L:
        if (!k_argisfp_(argres, 0) && arg[0] != NULL) {
          ExSet *c0 = MOVKinSet(arg[0]);
          done = (c0 != NULL && op.lunaryi(&dresp->val.i, e1k_(c0->exprn)));
          dresp->isdouble = NO;
        }
        break;
      case 2+Arg_L:
        if (   !k_argisfp_(argres, 0) && arg[0] != NULL
            && !k_argisfp_(argres, 1) && arg[1] != NULL)
        { Int64Con *a0 = LongLongLoadedToIntPair(arg[0], arg[1]);
          done = (a0 != NULL && op.unaryl(&dresp->val.i, &a0->bin.i) == i64_ok);
          dresp->isdouble = NO;
        }
        break;
      case 3+Arg_LI:
        if (   !k_argisfp_(argres, 0) && arg[0] != NULL
            && !k_argisfp_(argres, 1) && arg[1] != NULL
            && !k_argisfp_(argres, 2) && arg[2] != NULL)
        { Int64Con *a0 = LongLongLoadedToIntPair(arg[0], arg[1]);
          ExSet *c1 = MOVKinSet(arg[2]);
          done = (a0 != NULL && c1 != NULL
                  && op.binaryli(&dresp->val.i, &a0->bin.i, (Uint)e1k_(c1->exprn)) == i64_ok);
          dresp->isdouble = NO;
        }
        break;
      case 4+Arg_LL:
        if (   !k_argisfp_(argres, 0) && arg[0] != NULL
            && !k_argisfp_(argres, 1) && arg[1] != NULL
            && !k_argisfp_(argres, 2) && arg[2] != NULL
            && !k_argisfp_(argres, 3) && arg[3] != NULL)
        { Int64Con *a0 = LongLongLoadedToIntPair(arg[0], arg[1]);
          Int64Con *a1 = LongLongLoadedToIntPair(arg[2], arg[3]);
          done = (a0 != NULL && a1 != NULL
                  && op.binaryll(&dresp->val.i, &a0->bin.i, &a1->bin.i) == i64_ok);
          dresp->isdouble = NO;
        }
        break;
      }
    if (done && debugging(DEBUG_CSE) && CSEDebugLevel(1)) {
      cc_msg("Compile-time evaluable fn = ");
      if (dresp->isdouble) {
        char b[256];
        fltrep_sprintf(b, "%g", &dresp->val.d);
        cc_msg("%s\n", b);
      } else {
        pr_int64(&dresp->val.i);
        cc_msg("\n");
      }
    }
    return done;
}

static bool fadd(int32 *resp, int32 a0, int32 a1) {
    return EvalBinary_F_1(J_ADDFK, (FloatBin *)&a0, (FloatBin *)&a1, (FloatBin *)resp);
}

static bool fsub(int32 *resp, int32 a0, int32 a1) {
    return EvalBinary_F_1(J_SUBFK, (FloatBin *)&a0, (FloatBin *)&a1, (FloatBin *)resp);
}

static bool fmul(int32 *resp, int32 a0, int32 a1) {
    return EvalBinary_F_1(J_MULFK, (FloatBin *)&a0, (FloatBin *)&a1, (FloatBin *)resp);
}

static bool fdiv(int32 *resp, int32 a0, int32 a1) {
    return EvalBinary_F_1(J_DIVFK, (FloatBin *)&a0, (FloatBin *)&a1, (FloatBin *)resp);
}

static bool fgreater(int32 *resp, int32 a0, int32 a1) {
    int r;
    if (CSE_Compare_F(&r, (FloatBin *)&a0, (FloatBin *)&a1)) {
      *resp = (r > 0);
      return YES;
    }
    return NO;
}

static bool fgeq(int32 *resp, int32 a0, int32 a1) {
    int r;
    if (CSE_Compare_F(&r, (FloatBin *)&a0, (FloatBin *)&a1)) {
      *resp = (r >= 0);
      return YES;
    }
    return NO;
}

static bool fless(int32 *resp, int32 a0, int32 a1) {
    int r;
    if (CSE_Compare_F(&r, (FloatBin *)&a0, (FloatBin *)&a1)) {
      *resp = (r < 0);
      return YES;
    }
    return NO;
}

static bool fleq(int32 *resp, int32 a0, int32 a1) {
    int r;
    if (CSE_Compare_F(&r, (FloatBin *)&a0, (FloatBin *)&a1)) {
      *resp = (r <= 0);
      return YES;
    }
    return NO;
}

static bool fequal(int32 *resp, int32 a0, int32 a1) {
    int r;
    if (CSE_Compare_F(&r, (FloatBin *)&a0, (FloatBin *)&a1)) {
      *resp = (r == 0);
      return YES;
    }
    return NO;
}

static bool fneq(int32 *resp, int32 a0, int32 a1) {
    int r;
    if (CSE_Compare_F(&r, (FloatBin *)&a0, (FloatBin *)&a1)) {
      *resp = (r != 0);
      return YES;
    }
    return NO;
}

static bool fneg(int32 *resp, int32 a0) {
    DbleBin dr, da;
    fltrep_widen((FloatBin *)&a0, &da);
    return (flt_negate(&dr, &da) == flt_ok &&
            fltrep_narrow(&dr, (FloatBin *)resp) == flt_ok);
}

static bool fnarrow(int32 *resp, FloatCon const *a0) {
    return fltrep_narrow(&a0->floatbin.db, (FloatBin *)resp) == flt_ok;
}

static bool ffix(int32 *resp, int32 a0) {
    DbleBin da;
    fltrep_widen((FloatBin *)&a0, &da);
    return (flt_dtoi(resp, &da) == flt_ok);
}

static bool ffixu(int32 *resp, int32 a0) {
    DbleBin da;
    fltrep_widen((FloatBin *)&a0, &da);
    return (flt_dtou((unsigned32 *)resp, &da) == flt_ok);
}

static bool ffloat(int32 *resp, int32 a0) {
    DbleBin dr;
    return (flt_itod(&dr, a0) == flt_ok &&
            fltrep_narrow(&dr, (FloatBin *)resp) == flt_ok);
}

static bool ffloatu(int32 *resp, int32 a0) {
    DbleBin dr;
    return (flt_utod(&dr, a0) == flt_ok &&
            fltrep_narrow(&dr, (FloatBin *)resp) == flt_ok);
}

static bool dgreater(int32 *resp, FloatCon const *a0, FloatCon const *a1) {
    int r;
    if (CSE_Compare_D(&r, a0, a1)) {
      *resp = (r > 0);
      return YES;
    }
    return NO;
}

static bool dgeq(int32 *resp, FloatCon const *a0, FloatCon const *a1) {
    int r;
    if (CSE_Compare_D(&r, a0, a1)) {
      *resp = (r >= 0);
      return YES;
    }
    return NO;
}

static bool dless(int32 *resp, FloatCon const *a0, FloatCon const *a1) {
    int r;
    if (CSE_Compare_D(&r, a0, a1)) {
      *resp = (r < 0);
      return YES;
    }
    return NO;
}

static bool dleq(int32 *resp, FloatCon const *a0, FloatCon const *a1) {
    int r;
    if (CSE_Compare_D(&r, a0, a1)) {
      *resp = (r <= 0);
      return YES;
    }
    return NO;
}

static bool dequal(int32 *resp, FloatCon const *a0, FloatCon const *a1) {
    int r;
    if (CSE_Compare_D(&r, a0, a1)) {
      *resp = (r == 0);
      return YES;
    }
    return NO;
}

static bool dneq(int32 *resp, FloatCon const *a0, FloatCon const *a1) {
    int r;
    if (CSE_Compare_D(&r, a0, a1)) {
      *resp = (r != 0);
      return YES;
    }
    return NO;
}

static bool dfix(int32 *resp, FloatCon const *a0) {
    return flt_dtoi(resp, &a0->floatbin.db) == flt_ok;
}

static bool dfixu(int32 *resp, FloatCon const *a0) {
    return flt_dtou((unsigned32 *)resp, &a0->floatbin.db) == flt_ok;
}

typedef union {
    bool (*unaryi)(int32 *, int32);
    bool (*binaryi)(int32 *, int32, int32);
    bool (*unaryd)(int32 *, FloatCon const *);
    bool (*binaryd)(int32 *, FloatCon const *, FloatCon const *);
} EvalFn;

static int32 EvaluableIntValuedFn(Expr const *fn, VRegnum res, EvalFn *ev)
{
#ifndef TARGET_HAS_DIVIDE
    if (fn == arg1_(sim.divfn))  { ev->binaryi = res == R_A1 ? sdiv : srem; return 2; }
    if (fn == arg1_(sim.udivfn)) { ev->binaryi = res == R_A1 ? udiv : urem; return 2; }
#endif
#if defined(TARGET_LACKS_REMAINDER) || !defined(TARGET_HAS_DIVIDE)
    if (fn == arg1_(sim.remfn))  { ev->binaryi = srem; return 2; }
    if (fn == arg1_(sim.uremfn)) { ev->binaryi = urem; return 2; }
#endif
#ifndef TARGET_HAS_MULTIPLY
    if (fn == arg1_(sim.mulfn))  { ev->binaryi = mul; return 2; }
#endif
    if (fn == sim.fadd)       { ev->binaryi = fadd; return 2; }
    if (fn == sim.fsubtract)  { ev->binaryi = fsub; return 2; }
    if (fn == sim.fmultiply)  { ev->binaryi = fmul; return 2; }
    if (fn == sim.fdivide)    { ev->binaryi = fdiv; return 2; }
    if (fn == sim.fnegate)    { ev->unaryi = fneg; return 1; }
    if (fn == sim.ffix)       { ev->unaryi = ffix; return 1; }
    if (fn == sim.ffixu)      { ev->unaryi = ffixu; return 1; }
    if (fn == sim.ffloat)     { ev->unaryi = ffloat; return 1; }
    if (fn == sim.ffloatu)    { ev->unaryi = ffloatu; return 1; }

    if (fn == sim.fgreater)   { ev->binaryi = fgreater; return 2; }
    if (fn == sim.fgeq)       { ev->binaryi = fgeq; return 2; }
    if (fn == sim.fless)      { ev->binaryi = fless; return 2; }
    if (fn == sim.fleq)       { ev->binaryi = fleq; return 2; }
    if (fn == sim.fequal)     { ev->binaryi = fequal; return 2; }
    if (fn == sim.fneq)       { ev->binaryi = fneq; return 2; }

    if (fn == sim.dfix)       { ev->unaryd = dfix; return 2+Arg_D; }
    if (fn == sim.dfixu)      { ev->unaryd = dfixu; return 2+Arg_D; }
    if (fn == sim.fnarrow)    { ev->unaryd = fnarrow; return 2+Arg_D; }

    if (fn == sim.dgreater)   { ev->binaryd = dgreater; return 4+Arg_DD; }
    if (fn == sim.dgeq)       { ev->binaryd = dgeq; return 4+Arg_DD; }
    if (fn == sim.dless)      { ev->binaryd = dless; return 4+Arg_DD; }
    if (fn == sim.dleq)       { ev->binaryd = dleq; return 4+Arg_DD; }
    if (fn == sim.dequal)     { ev->binaryd = dequal; return 4+Arg_DD; }
    if (fn == sim.dneq)       { ev->binaryd = dneq; return 4+Arg_DD; }

    return 0;
}

bool CSE_EvaluableIntValuedCall(
  Expr const *fn, VRegnum res, int32 argres, ExSet *arg[], int32 *resp)
{   /* If fn is an integer-valued function (including float-valued if software
     * floating point) in the set we know how to evaluate, and this call has
     * constant arguments, see whether we can evaluate the call now (we may
     * not be able to, as for example in div(1, 0))
     */
    EvalFn op;
    int32 args = EvaluableIntValuedFn(fn, res, &op);
    bool done = NO;
    if (args == 0) return NO;
    if (argres & K_RESULTINFLAGS)
        return NO;
    if ((args & ArgCount) == k_argregs_(argres) && !k_argisfp_(argres, 0) && arg[0] != NULL)
      switch(args)
      {
case 1: { ExSet *c0 = MOVKinSet(arg[0]);
          done = (c0 != 0 && op.unaryi(resp, e1k_(c0->exprn)));
        }
        break;
case 2: if (!k_argisfp_(argres,1) && arg[1] != NULL)
        { ExSet *c0 = MOVKinSet(arg[0]);
          ExSet *c1 = MOVKinSet(arg[1]);
          done = (c0 != 0 && c1 != 0 &&
                  op.binaryi(resp, e1k_(c0->exprn), e1k_(c1->exprn)));
        }
        break;
case 2+Arg_D:
        if (!k_argisfp_(argres,1) && arg[1] != NULL)
        { FloatCon *a0 = DoubleLoadedToIntPair(arg[0], arg[1]);
          done = (a0 != NULL && op.unaryd(resp, a0));
        }
        break;
case 4+Arg_DD:
        if (!k_argisfp_(argres,1) && arg[1] != NULL &&
            !k_argisfp_(argres,2) && arg[2] != NULL &&
            !k_argisfp_(argres,3) && arg[3] != NULL)
        { FloatCon *a0 = DoubleLoadedToIntPair(arg[0], arg[1]);
          FloatCon *a1 = DoubleLoadedToIntPair(arg[2], arg[3]);
          done = (a0 != NULL && a1 != NULL &&
                  op.binaryd(resp, a0, a1));
        }
        break;
      }

    if (done && debugging(DEBUG_CSE) && CSEDebugLevel(1))
      cc_msg("Compile-time evaluable fn = %ld\n", *resp);
    return done;
}

/* end of mip/cseeval.c */
