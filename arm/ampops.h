/*
 * C compiler file arm/ampops.h
 * Copyright (C) Advanced Risc Machines Ltd., 1997. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef _ampops_LOADED
#define _ampops_LOADED 1

#define AMP_FADD 0x50
#define AMP_FSUB 0x54
#define AMP_FABS 0x55
#define AMP_FMIN 0x47
#define AMP_FMAX 0x46
#define AMP_FCMPLT 0x44
#define AMP_FCMPEQ 0x41
#define AMP_FCMPLE 0x45
/* 6-2 single precision fp conversion ops */
#define AMP_FCVTFI 0x48
#define AMP_FCVTIF 0x58
#define AMP_FCPS 0x5a
#define AMP_FCPSN 0x5b
#define AMP_FCPSE 0x59
#define AMP_FNORM 0x51
/* 6-3 single precision fp multiply & accumulate ops */
#define AMP_FMAC0 0x52
#define AMP_FMAC1 0x53
#define AMP_FMDC0 0x56
#define AMP_FMDC1 0x57
#define AMP_FMUL0 0x42
#define AMP_FMUL1 0x43
#define AMP_FMTACC0 0x4a
#define AMP_FMTACC1 0x4b
#define AMP_FMFACC0 0x4a
#define AMP_FMFACC1 0x4b
/* 6-4 single precision fp divide sqrt move ops */
#define AMP_FDIV 0x5c
#define AMP_FDIVF 0x5d
#define AMP_FSQRT 0x5e
#define AMP_FSQRTF 0x5f
#define AMP_FMTQ 0x49
#define AMP_FMFQ 0x4c
#define AMP_FMFQN 0x4d

/* 7-1 move operations (nb MBU ops) */
#define AMP_MFCSR 0x2e
#define AMP_MTCSR 0x0e

#define AMP_TCM_EXU(op, rd, rs1, rs2) \
  (0x0c000400 | ((op) << 16) | ((rs1) << 11) | ((rs2) << 5) | (rd))
#define AMP_TCM_MBU(op, rd, rs3, disp) \
  (0x0d000400 | ((op) << 16) | ((rs3) << 11) | ((disp) << 5) | (rd))

#define MCR_32 0
#define MCR_4 1
#define AMP_MCR(size, r, cr) \
  (0x0e000810 | ((size) << 22) | ((r) << 12) \
              | 0x40 | (((cr)&0x10)<<1) | ((cr) & 0xf))
#define AMP_MRC(size, r, cr) \
  (0x0e100810 | ((size) << 22) | ((r) << 12) \
              | 0x40 | (((cr)&0x10)<<1) | ((cr) & 0xf))

#define CPDT_LD (1 << 20)
#define CPDT_ST 0
#define CPDT_PRE (1 << 24)
#define CPDT_POST 0
#define CPDT_WB  (1<<21)
#define CPDT_UP   (1 << 23)
#define CPDT_DOWN 0

#define AMP_DT(flags, base, fr, offset) \
  (0x0c008800 | (flags) | ((base) << 16) \
              | (((fr) & 0x3c) << 10) | (((fr) & 3) << 8) \
              | ((offset) >> 2))

#define AMP_ZeroReg 31
#define AMP_WorkReg 30
#define AMP_F0 0

#define AMP_FPCR_RND (3<<4)
#define AMP_FPCR_RND_N 0
#define AMP_FPCR_RND_Z (1<<4)
#define AMP_FPCR_RND_U (2<<4)
#define AMP_FPCR_RND_D (3<<4)

#define AMP_FPSR 8
#define AMP_FPCR 11

#endif /* _ampops_loaded */

/* End of ampops.h */
