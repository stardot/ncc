/*
 * arm/mcerrs.h - prototype for machine-specific error messages file
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#ifndef NLS                     /* if NLS, miperrs will have included tags */

%O  /* Ordinary error messages - mapped onto numeric codes */

/*
 * The following message is always machine specific since it may include
 * information about the source of support for the product.
 */
#define misc_disaster_banner   "\n\
Internal inconsistency: either resource shortage or compiler fault. If you\n\
cannot alter your program to avoid this failure, please contact your supplier\n"

#define mcdep_warn_fpinconsistent \
        "Software floating point inconsistent with FPE2/3 and FPREGARGS"
#define gen_warn_Lisp "Lisp-support stack push needed %ld"
#define gen_err_swi "SWI number 0x%x too large"
#define gen_err_irq "%s cannot handle __irq functions"
#define obj_err_common "repeated common block $r"
#define obj_err_common1 "common block $r too small"
#define obj_err_common2 "common block $r too large"
#define armobj_fatalerr_toomany "Too many symbols for ACORN linker"
#define armdbg_fatalerr_toobig \
        "Debug table size exceeds space in Acorn AOF format"

#define asm_non_const           "illegal constant (ignored): <unknown>"
#define asm_nonconst1           "illegal constant expression (ignored): non constant $b"
#define asm_nonconst2           "illegal constant expression (ignored): $s"
#define asm_err_bad_shift       "illegal shift specifier: %s"
#define asm_err_expected_shift  "shift specifier expected"
#define asm_err_bad_opcode      "illegal instruction opcode: %s"
#define asm_err_bad_physreg     "physical registername expected, but found %s instead"
#define asm_err_bad_psrfield    "illegal PSR field: %s"
#define asm_err_expected_psr    "PSR field expected"
#define asm_err_bad_shiftval    "shift value %d out of range"
#define asm_err_expected_coproc "coprocessor identifier expected"
#define asm_err_unknown_coproc  "illegal coprocessor identifier: %s"
#define asm_err_coproc_op_range "coprocessor operation out of range: %d"
#define asm_err_ldrt_adrmode    "preindexed addressing not available for LDRT/STRT"
#define asm_err_no_mrs          "MRS & MSR are not available on this architecture"
#define asm_err_no_teqp         "TEQP/TSTP/CMPP/CMNP are not available on this architecture"
#define asm_err_no_longmul      "long multiply is not available on this architecture"
#define asm_err_no_halfword     "halfword support is not available on this architecture"
#define asm_err_corrupted_reg   "R%d corrupted but possibly reused later. This code may not work correctly"
#define asm_err_ldm_base_list   "LDM/STM base with writeback in register list"
#define asm_err_ldm_empty       "LDM/STM with empty register list"
#define asm_err_ldm_badlist     "LDM/STM with illegal register list"
#define asm_err_ldr_base_wb     "LDR/STR base, [base] with writeback"
#define asm_err_mul_conflict    "MUL/MLA with Rd = Rm"
#define asm_err_mull_conflict   "MULL/MLAL with Rhi = Rlo, Rhi = Rm or Rlo = Rm"
#define asm_err_swp_conflict    "SWP with Rd = Rm, Rm = Rn or Rd = Rn"
#define asm_err_pc              "illegal use of PC as operand"
#define asm_err_write_pc        "illegal write to PC"
#define asm_err_branch_pc       "a branch by writing to PC is not supported"
#define asm_err_fn_id           "function identifier expected"
#define asm_err_fn_notfound     "function %s undefined"
#define asm_err_thumb_highreg   "illegal access to high register R%d"

#endif                          /* ndef NLS - syserrs are not tags */

%S  /* System failure messages - error text not preserved */

#define syserr_interwork "Cannot compile this code with interworking, recompile without interworking"
#define syserr_commondef "commondef %ld"
#define syserr_obj_checksym "obj_checksym($r)"
#define syserr_obj_codereloc "obj_coderelocation %.8lx"
#define syserr_obj_gendata "obj_gendata(%ld)"
#define syserr_obj_datalen "obj_data len=%ld"
#define syserr_obj_data1 "obj_data %ldEL%ld'%s'"
#define syserr_find_extsym "find_extsym"
#define syserr_duff_lab "asm_trailer(duff lab)"
#define syserr_asm_trailer "asm_trailer(%ld)"
#define syserr_asm_data "asm_data len=%ld"
#define syserr_asm_trailer1 "asm_trailer(%ldF%ld)"
#define syserr_asm_hdata "asm_hdata len=%ld"
#define syserr_module_reloc "Unsupported Arthur module relocation mode %d"
#define syserr_outinstr "outinstr(%lx)"
#define syserr_datasymb "Unable to find another data symbol at %ld"
#define syserr_shifts "Register & constant shift together"
#define syserr_remove_noop "remove_noops(MOVR r,r) failed"
#define syserr_silly_shift "Silly shift value %ld"
#define syserr_inline1 "armgen(J_INLINE1)"
#define syserr_jop_mode "Illegal JOP mode(%lx,%lx)"
#define syserr_gen_freg "freg %ld"
#define syserr_remove_fp_noop "remove_noops(MOVD/FR r,r) failed"
#define syserr_fp_const "%lx not an immediate fp constant"
#define syserr_show_inst_dir "show_inst_dir(%#lX)"
#define syserr_local_base "local_base %lx"
#define syserr_local_addr "local_address %lx"
#define syserr_bad_regpair "bad register pair"
/* #define syserr_neg_addr "negative local address %ld/%lx for $b" */
#define syserr_local_addr1 "local_addr"
#define syserr_debug_addr "debugger local_fpaddr confused"
#define syserr_displacement "displacement out of range %ld"
#define syserr_unknown_label "unknown label reference type %.8lx"
#define syserr_enter "emit(J_ENTER %ld)"
#define syserr_gen_eval "gen_eval %ld"
#define syserr_pendingstack_overflow "pending stack overflow"
#define syserr_setsp_confused "SETSP confused %ld!=%ld %ld"
#define syserr_callingstandard "Callingstandard %c"
#define syserr_peep_bad_field "bad peephole op field"
#define syserr_peep_cant_swap "peephole: can't swap"
#define syserr_peep_bad_optype "bad peephole op type"
#define syserr_bad_maxinst "peephole MAXINST inconsistent with peephole definitions"
#define syserr_bad_useconstraint "bad peephole use constraint"
#ifndef TARGET_HAS_AOUT
#define syserr_debugger_line "debugger #line confused"
#define syserr_addcodep "dbg_addcodep(twice)"
#define syserr_filetooffset "dbg_filetooffset(%s)"
#define syserr_dbg_bitfield "dbg_typerep(BITFIELD)"
#define syserr_dbg_typerep "dbg_typerep(%p,0x%lx)"
#define syserr_dbg_proc "dbg_proc"
#define syserr_dbg_proc1 "dbg_proc confused"
#define syserr_dbg_table "debugger table confusion(local variable $r %lx %lx)"
#define syserr_dbg_scope "dbg_scope"
#define syserr_dbg_write "dbg_write(%lx)"
#define syserr_dbg_struct "dbg_STRUCT confused"
#define syserr_dbg_fileinfobase "dbg_fileinfobase %ld != %ld"
#define syserr_dbgloc "dbgloc %ld != %ld"
#endif
#define syserr_leaf_fn "Leaf function %s too big"
#define syserr_stm_freeregs "STM free register conflict (regmask %d, free regs %d)"

/* end of arm/mcerrs.h */
