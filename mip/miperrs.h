/*
 * C compiler error prototype file (miperrs.h)
 * Copyright (C) Codemist Ltd, 1988.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$ Codemist 151
 * Checkin $Date$
 * Revising $Author$
 */

/*
 * This file is input to the genhdrs utility, which can compress error
 * strings (but leaving escape sequences alone so that format checking can
 * occur) and optionally mapping syserr messages onto numeric codes
 * (in case somebody wants to save the about 4Kbytes of memory involved).
 */

/* AM: (after discussion with LDS) It would seem that error texts below */
/* which (seriously) take 2 or more arguments should be of the form     */
/*    #define ermsg(a,b,c) "ho hum %s had a %s %s", a, b, c             */
/* etc. to allow different sentence order in other (natural) languages. */

/* One nice thing would be to have a variant form of $r (etc) which did */
/* not quote its arg to avoid many uses of symname_() in the code.      */

%Z                              /* Don't compress #include strings! */

#include "msg.h"

#if defined(__CC_NORCROFT) && !defined(NLS)
  /*
   * The next procedure takes a string as a format... check args.
   */
#pragma -v3
#endif

/* cc_msg has been left in globals.h since it takes an uncompressed string */
extern void cc_rerr(msg_t errcode, ...);
extern void cc_ansi_rerr(msg_t errcode, ...);
extern void cc_warn(msg_t errcode, ...);
extern void cc_ansi_warn(msg_t errcode, ...);
extern void cc_pccwarn(msg_t errcode, ...);
extern void cc_err(msg_t errcode, ...);
extern void cc_fatalerr(msg_t errcode, ...);
extern void cc_rerr_cwarn(msg_t errorcode, ...);
extern void cc_rerr_cppwarn(msg_t errorcode, ...);

#if defined(__CC_NORCROFT) && !defined(NLS)
  /*
   * End of procedures that take error strings or codes.
   */
#pragma -v0
#endif

extern void push_fnap_context(Binder* fn); /* NULL fn ok */
extern void set_fnap_arg(int);
extern void pop_fnap_context(void);
extern void push_nested_context(msg_t, IPtr arg1, IPtr arg2);
extern void pop_nested_context(void);

#ifdef NLS
#  include "tags.h"             /* Under NLS, use tag versions */
#else

%O      /* Map strings to offsets in compressed string table */

#define warn_option_letter "unknown option -%c%c: ignored"
#define warn_option_g "unknown debugging option -g%c: -g assumed"
#define warn_option_zq "unknown option -zq%c: ignored"
#define warn_preinclude "can't open pre-include file %s (ignored)"
#define warn_option_E \
        "Obsolete use of '%s' to suppress errors -- use '-zu' for PCC mode"
#define warn_option_p "unknown profile option %s: -p assumed"
#define warn_option   "unknown option %s: ignored"

#define warn_usage_rw \
        "undefined behaviour: $b written and read without intervening sequence point"
#define warn_usage_ww \
        "undefined behaviour: $b written twice without intervening sequence point"

/* Preprocessor messages.  Here presumably because the preprocessor can be
   shared between different language front-ends.  But then why is pp.[ch] in
   cfe?  */

#define pp_warn_triglyph \
        "ANSI '%c%c%c' trigraph for '%c' found - was this intended?"
#define pp_warn_nested_comment "character sequence %s inside comment"
#define pp_warn_many_arglines \
        "(possible error): >= %lu lines of macro arguments"
#define pp_warn_redefinition "repeated definition of #define macro %s"
#define pp_warn_ifvaldef "#ifdef %s may indicate trouble..." /* MACH_EXTNS */
#define pp_warn_nonansi_header "Non-ANSI #include <%s>"
#define pp_warn_bad_pragma "Unrecognised #pragma (no '-' or unknown word)"
#define pp_warn_bad_pragma1 "Unrecognised #pragma -%c"
#define pp_warn_unused_macro "#define macro '%s' defined but not used"
#define pp_warn_unbalanced "Unbalanced #if/#ifdef/#ifndef/#endif in file"
#define pp_warn_macro_arg_exp_in_string "argument %s of macro %s expanded in %c...%c"
#define pp_warn_pragma_suppress "#pragma -b suppresses errors (hence non-ANSI)"
#define pp_warn_not_guarded "Header file not guarded against multiple inclusion"
#define pp_warn_guard_not_defined "File is guarded by '%s' but does not #define it"
#define pp_warn_continued_comment "trailing '\\' continues comment"
#define pp_warn_directive_in_args \
    "preprocessor directive ignored in macro argument list\n"
#define pp_warn_eol_string_skipped "Unmatched quote (%c) in skipped line"
#define pp_rerr_redefinition "differing redefinition of #define macro %s"
#define pp_rerr_nonunique_formal "duplicate macro formal parameter: '%s'"
#define pp_rerr_define_hash_arg "operand of # not macro formal parameter"
#define pp_rerr_define_hashhash "## first or last token in #define body"
#define pp_rerr_newline_eof "missing newline before EOF - inserted"
#define pp_rerr_nonprint_char "unprintable char %#.2x found - ignored"
#define pp_rerr_illegal_option "illegal option -D%s%s"
#define pp_rerr_spurious_else "spurious #else ignored"
#define pp_rerr_spurious_elif "spurious #elif ignored"
#define pp_rerr_spurious_endif "spurious #endif ignored"
#define pp_rerr_hash_line "number missing in #line"
#define pp_rerr_hash_error "#error encountered \"%s\""
#define pp_rerr_hash_ident "#ident is not in ANSI C"
#define pp_rerr_junk_eol "junk at end of #%s line - ignored"
#define pp_err_eof_comment "EOF in comment"
#define pp_err_eof_string "EOF in string"
#define pp_err_eol_string "quote (%c) inserted before newline"
#define pp_err_eof_escape "EOF in string escape"
#define pp_err_missing_quote "Missing '%c' in pre-processor command line"
#define pp_err_if_defined "No identifier after #if defined"
#define pp_err_if_defined1 "No ')' after #if defined(..."
#define pp_err_rpar_eof "Missing ')' after %s(... on line %ld"
#define pp_err_many_args "Too many arguments to macro %s(... on line %ld"
#define pp_err_few_args "Too few arguments to macro %s(... on line %ld"
#define pp_err_missing_identifier "Missing identifier after #define"
#define pp_err_missing_parameter "Missing parameter name in #define %s(..."
#define pp_err_missing_comma "Missing ',' or ')' after #define %s(..."
#define pp_err_undef "Missing identifier after #undef"
#define pp_err_ifdef "Missing identifier after #ifdef"
#define pp_err_include_quote "Missing '<' or '\"' after #include"
#define pp_err_include_junk "Junk after #include %c%s%c"
#define pp_err_include_file "#include file %c%s%c wouldn't open"
#define pp_err_unknown_directive "Unknown directive: #%s"
#define pp_err_endif_eof "Missing #endif at EOF"
#define pp_fatalerr_hash_error "#error encountered \"%s\""
#define pp_fatalerr_readfail "Host file system read error\n"

#define bind_warn_extern_clash \
        "extern clash $r, $r clash (ANSI 6 char monocase)"
#define bind_warn_unused_static_decl "unused earlier static declaration of $r"
#define bind_warn_not_in_hdr "extern $r not declared in header"
#define bind_warn_main_not_int "extern 'main' needs to be 'int' function"
#define bind_warn_label_not_used "label $r was defined but not used"
/*
 * Note that when part of an error string MUST be stored as a regular
 * non-compressed string I have to inform the GenHdrs utility with %Z and %O
 * This arises when a string contains literal strings as extra sub-args.
 */

/* No longer applies to bind_warn_*_not_used, though */

#define bind_warn_typedef_not_used "typedef $b declared but not used"
#define bind_warn_typename_not_used "typename $b declared but not used"
#define bind_warn_function_not_used "function $b declared but not used"
#define bind_warn_variable_not_used "variable $b declared but not used"

#define bind_warn_static_not_used "static $b declared but not used"
#define cg_warn_implicit_return "implicit return in non-void %s()"
#define flowgraf_warn_implicit_return "implicit return in non-void function"
#define regalloc_warn_use_before_set "$b may be used before being set"
#define regalloc_warn_never_used "$b is set but never used"
#define sem_warn_unsigned "ANSI surprise: 'long' $s 'unsigned' yields 'long'"
#define sem_warn_format_type "actual type $t of argument %d mismatches format '%.*s'"
#define sem_warn_bad_format "Illegal format conversion '%%%c'"
#define sem_warn_incomplete_format "Incomplete format string"
#define sem_warn_format_nargs_0 "Format requires 0 parameters, but %ld given"
#define sem_warn_format_nargs_1 "Format requires 1 parameter, but %ld given"
#define sem_warn_format_nargs_n "Format requires %ld parameters, but %ld given"
#define sem_warn_addr_array "'&' unnecessary for function or array $e"
#define sem_warn_bad_shift(_m,_n) "shift of $m by %ld undefined in ANSI C",_m,_n
#define sem_warn_divrem_0 "division by zero: $s"
#define sem_warn_fp_overflow(op) "floating point constant overflow: $s",op
#define sem_warn_implicit_constructor "implicit constructor $t()"
#define sem_warn_cast_sametype "explicit cast to same type"
#define sem_warn_ctor_confused "user defined constructors confusing"
#define sem_warn_structassign "structure assignment"

#define sem_errwarn_udiad_overflow "unsigned constant overflow: $s"
#define sem_errwarn_diad_overflow "signed constant overflow: $s"
#define sem_errwarn_umonad_overflow "unsigned constant overflow: $s"
#define sem_errwarn_monad_overflow "signed constant overflow: $s"
#define sem_errwarn_bad_shift "overlarge shift distance"
#define sem_errwarn_divrem_0 "division by zero: $s"
#define sem_errwarn_ucomp_0 "odd unsigned comparison with 0: $s"
#define sem_errwarn_ucomp_0_false "unsigned comparison with 0 is always false: $s"
#define sem_errwarn_ucomp_0_true "unsigned comparison with 0 is always true: $s"
#define sem_errwarn_fp_overflow "floating point constant overflow: $s"

#define sem_rerr_udiad_overflow(op,_a,_b,_c) "unsigned constant overflow: $s",op
#define sem_rerr_diad_overflow(op,_a,_b,_c) "signed constant overflow: $s",op
#define sem_rerr_umonad_overflow(op,_a,_b) "unsigned constant overflow: $s",op
#define sem_rerr_monad_overflow(op,_a,_b) "signed constant overflow: $s",op
#define sem_rerr_implicit_cast_overflow(_t,_a,_b) \
                                "implicit cast (to $t) overflow",_t
#define sem_rerr_implicit_cast5 \
        "$s: implicit cast of pointer loses $m qualifier"
#define sem_warn_depr_string \
        "$s: deprecated conversion of string literal to pointer to non-const"
#define sem_rerr_too_many_args_ovld "too many arguments for overload resolution"
#define sem_rerr_postdecr_bool "operand of $s cannot be 'bool'"
#define sem_rerr_opequal_bool \
        "type of left operand of $s cannot be 'bool'"

#define sem_warn_deprecated_bool "deprecated use: operand of $s is 'bool'"
#define sem_warn_unusual_bool "$s: cast to 'bool' (other than integer 0 or 1)"
#define sem_warn_fix_fail "floating to integral conversion overflow"
#define sem_warn_index_ovfl "out-of-bound offset %ld in address"
#define sem_warn_low_precision "lower precision in wider context: $s"
#define sem_warn_odd_condition "use of $s in condition context"
#define sem_warn_void_context "no side effect in void context: $s"
#define sem_warn_olde_mismatch "argument and old-style parameter mismatch: $e"
#define sem_warn_uncheckable_format \
        "'format' arg. to printf/scanf etc. is variable, so cannot be checked"
#define sem_warn_narrow_voidstar "implicit cast from (void *), C++ forbids"
#define sem_warn_narrowing "implicit narrowing cast: $s"
#define sem_warn_fn_cast \
        "$s: cast between function pointer and non-function object"
#define sem_warn_pointer_int "explicit cast of pointer to 'int'"
#define bind_warn_unused_this_in_member "'this' unused in non-static member function"
#define bind_warn_cpp_scope_differ "C++ scope may differ: $c"
#define bind_warn_new_tag_in_formals "$c invented in parameter list"

#define bind_err_cant_use_outer_member "attempt to use $p$r"
#define bind_err_extern_clash "extern clash $r, $r (linker %ld char)"
#define bind_err_extern_clash_monocase "extern clash $r, $r (linker %ld char monocase)"
#define bind_err_duplicate_tag "duplicate definition of $s tag $b"
#define bind_err_reuse_tag "re-using $s tag $b as $s tag"
#define bind_err_incomplete_tentative \
        "incomplete tentative declaration of $r"
#define bind_err_type_disagreement "type disagreement for $r"
#define bind_err_duplicate_definition "duplicate definition of $r"
#define bind_err_duplicate_label "duplicate definition of label $r - ignored"
#define bind_err_unset_label "label $r has not been set"
#define bind_err_undefined_static \
        "static function $b not defined - treated as extern"
#define bind_err_conflicting_globalreg \
        "conflicting global register declarations for $b"
#define xbind_err_template_specialize_after "attempt to specialize after use"
#define xbind_err_template_undef "attempt to instantiate undefined template $b"
#define xbind_err_template_none "no template found to specialize or instantiate"
#define xbind_err_template_ambiguous "ambiguous templates to specialize or instantiate"
#define bind_err_template_tpara_redeclared "re-declaration of template type paramenter $r"

#define fp_err_very_big "Overlarge floating point value found"
#define fp_err_big_single \
        "Overlarge (single precision) floating point value found"
#define sem_err_typeclash "Illegal types for operands: $s"
#define sem_err_sizeof_struct "size of $c needed but not yet defined"
#define sem_rerr_sizeof_opaque "size of opaque $c needed"
#define sem_err_lvalue "Illegal in lvalue: function or array $e"
#define sem_err_bitfield_address "bit fields do not have addresses"
#define sem_err_lvalue1 "Illegal in l-value: 'enum' constant $b"
#define sem_err_lvalue2 "Illegal in the context of an l-value: $s"
#define sem_err_nonconst "illegal in %s: <unknown>"
#define sem_err_nonconst1 "illegal in %s: non constant $b"
#define sem_err_nonconst2 "illegal in %s: $s"

#define bind_msg_const_nonconst "illegal in constant expression: <unknown>"
#define bind_msg_const_nonconst1 \
     "illegal in constant expression: non constant $b"
#define bind_msg_const_nonconst2 "illegal in constant expression: $s"

#define moan_floating_type_nonconst \
     "illegal in floating type initialiser: <unknown>"
#define moan_floating_type_nonconst1 \
     "illegal in floating type initialiser: non constant $b"
#define moan_floating_type_nonconst2 \
     "illegal in floating type initialiser: $s"
#define moan_static_int_type_nonconst \
     "illegal in static integral type initialiser: <unknown>"
#define moan_static_int_type_nonconst1 \
     "illegal in static integral type initialiser: non constant $b"
#define moan_static_int_type_nonconst2 \
     "illegal in static integral type initialiser: $s"

#define sem_err_nonfunction "attempt to apply a non-function"
#define sem_err_void_argument "'void' values may not be arguments"
#define sem_err_bad_cast "$s: illegal cast of $t to pointer"
#define sem_err_bad_cast1 "$s: illegal cast to $t"
#define sem_err_bad_cast2 "$s: cast to non-equal $t illegal"
#define sem_err_undef_struct \
        "$c not yet defined - cannot be selected from"
#define sem_err_unknown_field "$c has no $r field"
#define sem_err_no_this_pntr "no 'this' pointer to access member"
#define sem_err_no_this_pntr2 "no 'this' pointer for member function $e"
#define sem_err_dupl_default_value \
        "duplicate declaration of default value for argument #%d: $r"
#define sem_err_missing_default_value "missing default value for argment #%d: $r"
#define sem_err_noncallsite_function "non-call site '.*' or '->*' yielding function"
#define sem_err_illegal_loperand "illegal left operand to $s"
#define sem_err_addr_globalvar "$b is a global register variable - can't take its address"
#define sem_err_ctor_confused "construction of value of type $t is recursively confused"
#define sem_err_no_ctor_at_type "no constructor for $c at this type signature"

#define sem_err_template_nontype_storage "template non-type $b has no storage"
#define syn_err_template_member "template member expected"
#define syn_rerr_temp_para_redefinition "template parameter $b re-defined"

#define simplify_err_outsizearray "array $b too large"
#define simplify_err_illegal_use_of_mem_fn "illegal use of member function - '&$#b' intended?"

/* This has been removed - MJW
 * #define errs_membobj(_m)\
 * %Z  (_m ? "member":"object")
 * %O
 */

#define bind_rerr_undefined_tag "$s tag $b not defined"
#define bind_rerr_mem_opaque "can't access data member $b of opaque type"
#define bind_rerr_linkage_disagreement \
        "linkage disagreement for $r - treated as $g"
#define bind_rerr_linkage_disagreement_2 \
        "$b has no linkage, but $b requires it to have"
#define bind_rerr_linkage_disagreement_3 \
        "linkage disagreement between $b and $b"
#define bind_rerr_linkage_previously_c \
        "$b was previously declared without \"C\" linkage"
#define bind_rerr_duplicate_typedef "duplicate typedef $r"
#define bind_rerr_local_extern "extern $r mismatches top-level declaration"
#define xbind_warn_implicit_virtual "$p$r inherits implicit virtual"
#define xbind_rerr_inherited_type_differs "inherited virtual function type differs: $b"
#define xbind_rerr_ovld_non_fn "attempt to overload non-function $b"
#define xbind_rerr_is_ambiguous_name "$r is an ambiguous name in $c"
#define xbind_rerr_dl_cant_be_overloaded "'operator delete' can't be overloaded"
#define xbind_rerr_bad_dl_type \
    "'operator delete' must be of type 'void (void*)' or 'void (void*, size_t)'"
#define xbind_rerr_bad_global_dl_type \
    "'::operator delete' must be of type 'void (void*)'"
#define xbind_rerr_bad_nw_type \
    "'operator new' must have a return type of 'void*' and have a first argument of type 'size_t'"
#define xbind_rerr_more_than_one_C_linkage "more than one $r has \"C\" linkage"
#define xbind_rerr_both_virtual_and_nonvirtual \
    "$r inherits both virtual and non-virtual attributes from bases of $c"

#define cg_rerr_iffy_arithmetics "iffy arithmetic shift"
#define fp_rerr_very_small "small floating point value converted to 0.0"
#define fp_rerr_small_single \
        "small (single precision) floating value converted to 0.0"
#define sem_rerr_sizeof_bitfield \
        "sizeof <bit field> illegal - sizeof(int) assumed"
#define sem_rerr_sizeof_void "size of 'void' required - treated as 1"
#define sem_rerr_sizeof_array "size of a [] array required, treated as [1]"
#define sem_rerr_sizeof_function \
        "size of function required - treated as size of pointer"
#define sem_rerr_pointer_arith \
        "<int> $s <pointer> treated as <int> $s (int)<pointer>"
#define sem_rerr_pointer_arith1 \
        "<pointer> $s <int> treated as (int)<pointer> $s <int>"
#define sem_rerr_assign_const "assignment to 'const' object $e"
#define sem_rerr_addr_regvar \
        "'register' attribute for $b ignored when address taken"
#define sem_rerr_lcast "objects that have been cast are not l-values"
#define sem_rerr_pointer_compare \
"comparison $s of pointer and int:\n\
  literal 0 (for == and !=) is only legal case"
#define sem_rerr_different_pointers "differing pointer types: $s"
#define sem_rerr_wrong_no_args "wrong number of parameters to $e"
#define sem_rerr_casttoenum "$s: cast of $m to differing enum" /* warn in C */
#define sem_rerr_valcasttoref "$s: non-lvalue cast to non-const reference"
#define sem_rerr_implicit_cast1 \
        "$s: implicit cast of pointer to non-equal pointer"
#define sem_rerr_implicit_cast2 "$s: implicit cast of non-0 int to pointer"
#define sem_rerr_implicit_cast3 "$s: implicit cast of pointer to 'int'"
#define sem_rerr_implicit_cast4 "$s: implicit cast of $t to 'int'"
#define sem_rerr_nonpublic "$p$r is a non-public member"
#define sem_rerr_cant_balance "differing pointer types: $s"
#define sem_rerr_void_indirection "illegal indirection on (void *): '*'"
#define sem_rerr_noncallsite_ovld "non-call site overload (using $b for $e)"
#define sem_rerr_cast_dtype_from_vbase "cast to derived type from virtual base"
#define sem_rerr_abclass_need_vtable "confusion: abstract? $c needs vtable"
#define sem_rerr_too_many_args "too many arguments: $t constructor"

#define obj_fatalerr_io_object "I/O error on object stream"
#define compiler_rerr_no_extern_decl\
    "no external declaration in translation unit"
#define compiler_fatalerr_io_error "I/O error writing '%s'"
#define compiler_fatalerr_load_version \
    "load file created with a different version of the compiler"
#define driver_fatalerr_io_object "I/O error on object stream"
#define driver_fatalerr_io_asm "I/O error on assembler output stream"
#define driver_fatalerr_io_listing "I/O error on listing stream"

#ifdef TARGET_HAS_AOUT
#define aout_fatalerr_toomany "Too many symbols for 'a.out' output"
#define aout_fatalerr_toobig "Module too big for a.out formatter"
#endif
#ifdef TARGET_HAS_COFF
#define coff_fatalerr_toomany "Too many relocations for COFF format in .o file"
#define coff_fatalerr_toobig "Module too big for COFF formatter"
#endif
#define misc_fatalerr_space1 "out of store (for error buffer)"
#define misc_fatalerr_toomanyerrs "Too many errors"
#define misc_fatalerr_space2 "out of store (in cc_alloc)\n\
(Compilation of the debugging tables requested with the -g option\n\
 requires a great deal of memory. Recompiling without -g, with\n\
 the more restricted -gf option, or with the program broken into\n\
 smaller pieces, may help.)"
#define misc_fatalerr_space3 "out of store (in cc_alloc)"

/* Beware: none of the following driver_message #defined are used!      */
#define driver_message_nolisting \
        "Unable to open %s for listing: -l option ignored\n"
#ifdef NO_ASSEMBLER_OUTPUT
#define driver_message_noasm \
        "This version of the compiler does not support -s\n"
#endif
#define driver_message_writefail "Couldn't write file '%s'\n"
#define driver_message_oddoption "Unrecognized option '%c': ignored\n"
#define driver_message_readfail "Couldn't read file '%s'\n"
/* NB the next error can not arise with the current ARM driver */
#define driver_message_toomanyfiles "Too many file args"
#define driver_message_asmstdout "Assembly code will go to stdout\n"
#define driver_message_no_listing \
        "-m option useless without source listing. Ignored\n"
#define driver_message_nomap "-m file not available or corrupt - ignored\n"
#define driver_message_notest \
        "This version of the compiler does not support the -test option\n"
#define driver_message_needfile "At least one file argument wanted\n"
#ifndef COMPILING_ON_ARM_OS
#define driver_message_spool "output to clog1.log & clog2.log\n"
#endif
#define driver_message_testfile "No files allowed with -test\n"

%Z
#define driver_abort_msg "Compilation aborted: %s\n"
#define driver_couldnt_write "couldn't write file '%s'\n"
#define driver_couldnt_read "couldn't read file '%s'\n"
#define driver_too_many_file_args "too many file arguments"
#define driver_couldnt_read_counts "couldn't read \"counts\" file"
#define driver_malformed_counts "malformed \"counts\" file"
#define driver_toolenv_writefail "Couldn't write installation configuration\n"

#define driver_incompat_cfrontcpp_ansi "-ansi incompatible with -cfront or -cpp"
#define driver_incompat_cfrontcpp_pcc "-pcc incompatible with -cfront or -cpp"
#define driver_incompat_pccansi_cfront "-cfront incompatible with -pcc or -ansi"
#define driver_incompat_pccansi_cpp "-cpp incompatible with -pcc or -ansi"
#define driver_ignored_arthur_unix "Warning: -arthur/-super ignored under unix\n"
#ifdef WANT_WHINGY_MSGS_EVEN_WHEN_WRONG
#define driver_ignored_linkerflags \
    "Warning: linker flag(s) ignored with -c -E -M or -S\n"
#endif
#define driver_conflict_EM "Warning: options -E and -M conflict: -E assumed\n"
#ifdef FORTRAN
#define driver_conflict_strict_onetrip \
    "Warning: -onetrip and -strict conflict: -strict assumed\n"
#define driver_conflict_strict_f66 \
    "Warning: -f66 and -strict conflict: -strict assumed\n"
#define driver_conflict_strict_extend \
    "Warning: -extend and -strict conflict: -strict assumed\n"
#define driver_implies_f66_onetrip "Warning: -f66 implies -onetrip\n"
#endif

#define driver_ignored_filename_overlong "Overlong filename ignored: %s\n"
#define driver_unknown_filetype "Error: type of '%s' unknown (file ignored)\n"
#define driver_option_bad "Error: bad option '%s': ignored\n"
#ifdef FORTRAN
#define driver_option_nimp "Error: unimplemented option '%s': ignored\n"
#endif
#define driver_via_not_opened "Can't open -via file %s\n"
/* Printed as "Error: bad option '<opt> <opt> <opt> ...': ignored\n" */
#define driver_option_bad1 "Error: bad option "
#define driver_option_bad2 ": ignored\n"
#define driver_option_missing_filearg "Missing file argument for %s\n"
#define driver_option_missing_lastarg "No argument to last compiler option"
#define driver_option_missing_arg "No argument to compiler option %s\n"
#define driver_cant_open_output "Can't open %s for output\n"

#define driver_cpp_toomanyfileargs "More than 2 file arguments to cpp ignored\n"
#define driver_cpp_cantopenoutput "Can't open output file %s\n"
#define driver_stdin_otherfiles "stdin ('-') combined with other files -- ignored\n"

#define driver_noeffect "Warning: %s command with no effect\n"

/* Needs fixing for NLS */
#define driver_banner "%s\n", CC_BANNER
#define driver_prerelease "%s\n", DRIVER_PRE_RELEASE_MSG


/* messages that form part of the varied help texts produced by cc -help */

#define driver_expire "\
This time-limited software remains the property of %s.\n\
It will expire at (GMT) %s\n"

#define help_blank "\n"
#define help_bsd_f77 "\n\nBSD compatible ANSI F77 compiler.\n"
#define help_usage "\n\
Usage:         %s [options] file1 file2 ... filen\n"
#define help_main_options "Main options:\n"
#define help_list                "\
-list          Generate a compilation listing\n"
#define help_iso                 "\
-iso           Compile strictly according to ISO (BS 6192 : 1982)\n"
#define help_ansi                "\
-ansi          Compile ANSI-style C source code\n"
#define help_strict              "\
-strict        Accept only programs strictly conforming to ANSI standard\n"
#define help_pcc                 "\
-pcc           Compile UNIX PCC style C source code\n"
#define help_pcc_bsd             "\
-pcc           Compile BSD UNIX PCC-style C source code\n"
#define help_f66                 "\
-f66           Follow F66 practices or rules when conflicting with F77\n"
#define help_arthur              "\
-arthur        Add 'arthur' to the list of libraries to be linked with\n\
                 (Arthur only)\n"
#define help_brazil              "\
-super         Add 'supervisor' to the list of libraries to be linked with\n\
                 (Brazil only)\n"
#define help_dont_link           "\
-c             Do not link the files being compiled\n"
#define help_dont_link_invoke    "\
-c             Do not invoke the linker to link the files being compiled\n"
#define help_leave_comments      "\
-C             Prevent the preprocessor from removing comments (Use with -E)\n"
#define help_predefine           "\
-D<symbol>     Define <symbol> on entry to the compiler\n"
#define help_predefine_pp        "\
-D<symbol>     Define preprocessor <symbol> on entry to the compiler\n"
#define help_preprocess_pascal   "\
-E             Preprocess the Pascal source code only\n"
#define help_preprocess_c        "\
-E             Preprocess the C source code only\n"
#define help_preprocess_fortran  "\
-E             Preprocess the F77 source code only\n"
#define help_compiler_features   "\
-F<options>    Enable a selection of compiler defined features\n"
#define help_runtime_checks      "\
-R<options>    Disable selected run time error checks\n"
#define help_debug               "\
-g<options>    Generate code that may be used with the debugger\n"
#define help_debug_noopt         "\
-g             Generate code that may be used with the debugger\n"
#define help_include_I           "\
-I<directory>  Include <directory> on the #include search path\n"
#define help_16bit_ints          "\
-i2            Make the default integer size 16 bits\n"
#define help_include_J           "\
-J<directory>  Replace the default #include path with <directory>\n"
#define help_libraries           "\
-L<libs>       Specify a comma-joined list of libraries to be linked with\n\
               instead of the standard library\n"
#define help_makefile            "\
-M<options>    Generate a 'makefile' style list of dependencies\n"
#define help_output              "\
-o<file>       Instruct the linker to call the object code produced <file>\n"
#define help_output_space        "\
-o <file>      Instruct the linker to name the object code produced <file>\n"
#define help_optimised           "\
-O             Invoke the object code improver\n"
#define help_onetrip             "\
-onetrip       Compile DO loops that are performed at least once if reached\n"
#define help_profile             "\
-P<options>    Generate code to generate 'profile' information\n"
#define help_profile_lc          "\
-p<options>    Generate code to generate 'profile' information\n"
#define help_readonly_strings    "\
-R             Place all compile time strings in a 'Read only' segment\n"
#define help_readonly_strings_lc "\
-r             Place all compile time strings in a 'Read only' segment\n"
#define help_output_assembler    "\
-S             Output assembly code instead of object code\n"
#define help_generate_assembler  "\
-S             Generate assembly code instead of object code\n"
#define help_dont_downcase       "\
-U             Do not convert upper case letters to lower case\n"
#define help_preundefine         "\
-U<symbol>     Undefine <symbol> on entry to the compiler\n"
#define help_preundefine_pp      "\
-U<symbol>     Undefine preprocessor <symbol> on entry to the compiler\n"
#define help_disable_warnings    "\
-W<options>    Disable all or selected warning and error messages\n"
#define help_disable_warnings_lc "\
-w<options>    Disable all or selected warning and error messages\n"
#define help_helios_libraries    "\
-Z<option>     Special Helios options for shared library building etc\n"

%O
/* messages generated by misc.c */
#ifndef TARGET_IS_UNIX
#  ifndef COMPILING_ON_MPW
#define misc_message_lineno(_f,_l,_s) "\"%s\", line %ld: %s",_f,_l,_s
#  else
#define misc_message_lineno_mpw(_f,_l,_s) \
        "File \"%s\"; Line %ld # %s",_f,_l,_s
#  endif
#else
#define misc_message_lineno_unix(_f,_l,_s) "%s: %ld: %s",_f,_l,_s
#endif

#ifndef TARGET_IS_UNIX
#  ifndef COMPILING_ON_MPW
#define misc_message_nolineno(_f,_s) "\"%s\": %s",_f,_s
#  else
#define misc_message_nolineno_mpw(_f,_s) \
        "File \"%s\" # %s",_f,_s
#  endif
#else
#define misc_message_nolineno_unix(_f,_s) "%s: %s",_f,_s
#endif

#define misc_message_nofile(_s) "%s",_s

#ifndef COMPILING_ON_MPW
#define misc_message_sum1_zero "%s: 0 warnings"
#define misc_message_sum1_sing "%s: 1 warning"
#define misc_message_sum1(_f,nx) "%s: %ld warnings", _f, nx
#else
#define misc_message_sum1_zero_mpw "### \"%s\": 0 warnings"
#define misc_message_sum1_sing_mpw "### \"%s\": 1 warning"
#define misc_message_sum1_mpw(_f,nx) "### \"%s\": %ld warnings", _f, nx
#endif

#define misc_message_sum2 " (+ %ld suppressed)"
#define misc_message_sum3_zero ", 0 errors"
#define misc_message_sum3_sing ", 1 error"
#define misc_message_sum3 ", %ld errors"
#define misc_message_sum5_zero ", 0 serious errors\n"
#define misc_message_sum5_sing ", 1 serious error\n"
#define misc_message_sum5 ", %ld serious errors\n"

#define misc_message_internal "Failure of internal consistency check"

#define misc_msg_fnarg_this_fnap "<implicit 'this' argument to function call>"
#define misc_msg_fnarg_this_fnname "<implicit 'this' argument to $b>"
#define misc_msg_fnarg_fnap "<argument %d to function call>"
#define misc_msg_fnarg_fnname "<argument %d to $b>"

/* Cannot be issued if NARGREGS==0 */
#define warn_untrustable "untrustable code generated for $r"

#endif                          /* ndef NLS */
/* Under NLS the following are still not tags */

%S /* The next batch of things just get mapped onto syserr codes */

#define syserr_removepostincs "unexpected op in RemovePostIncs"
#define syserr_mkqualifiedtype "mkqualifiedtype(..., %ld)"
#define syserr_unbitfield "unbitfield_type $t"
#define syserr_bf_promote "bf_promoted_type $t"
#define syserr_typeof "typeof(%ld)"
#define syserr_alignoftype "alignoftype(%ld,%#lx)"
#define syserr_sizeoftype "sizeoftype(%ld,%#lx)"
#define syserr_codeoftype "codeoftype"
#define syserr_equivtype "equivtype(%ld)"
#define syserr_compositetype "compositetype(%ld)"
#define syserr_trydiadicreduce "trydiadreduce(unsigned op %ld)"
#define syserr_trydiadicreduce1 "trydiadreduce(signed op %ld)"
#define syserr_trydiadicreduce2 "trydiadreduce(float op %ld)"
#define syserr_fp_op "FP op %ld unknown"
#define syserr_trymonadicreduce "trymonadreduce(int op %ld)"
#define syserr_trymonadicreduce1 "trymonadreduce(float op %ld)"
#define syserr_bf_container "bf_container"
#define syserr_coerceunary1 "coerceunary(%ld,%#lx)"
#define syserr_bitfieldassign "bitfieldassign"
#define syserr_mkindex "sem(mkindex)"
#define syserr_ptrdiff "sem(mkbinary/ptrdiff)"
#define syserr_va_arg_fn "sem(odd va_arg fn)"
#define syserr_mkcast "mkcast(%ld,%#lx)"
#define syserr_mkcast1 "mkcast(%ld)"
#define syserr_te_plain "te_plain(%ld)"
#define syserr_clone_node "clone_node(%ld)"
#define syserr_optimise "optimise &(%ld)"
#define syserr_optimise1 "optimise(%ld)"
#define syserr_mcrepofexpr "mcrepofexpr(%ld,%#lx)"
#define syserr_mcreparray "mcrep(array %ld)"
#define syserr_newdbuf "pp_newdbuf(%ld,%ld)"
#define syserr_pp_recursion "pp recursive sleep: '%s'"
#define syserr_pp_special "pp_special(%ld)"
#define syserr_overlarge_store1 "Overlarge storage request (binder %ld)"
#define syserr_overlarge_store2 "Overlarge storage request (local %ld)"
#define syserr_discard2 "discard2 %p"
#define syserr_discard3 "discard3 %p"
#define syserr_alloc_unmark "alloc_unmark - called too often"
#define syserr_alloc_unmark1 "alloc_unmark(no drop_local_store())"
#define syserr_alloc_reinit "alloc_reinit(no drop_local_store())"
#define syserr_addclash "add_clash (0x%lx, 0x%lx)"
#define syserr_forget_slave "forget_slave(%ld, %ld) %ld"
#define syserr_GAP "GAP in reference_register"
#define syserr_corrupt_register "corrupt_register %ld %p"
#define syserr_regalloc "regalloc(corrupt/alloc)"
#define syserr_regalloc_typefnaux "regalloc(typefnaux)"
#define syserr_regalloc_POP "regalloc(POP)"
#define syserr_call2 "CALL2 %ld"
#define syserr_regno "reg %lx"
#define syserr_vregsort "vregsort(%lx)"
#define syserr_liveness "liveness odd"
#define syserr_dataflow "dataflow &-var"
#define syserr_choose_real_reg "choose_real_reg %lx"
#define syserr_fail_to_spill "Failed to spill register for %ld"
#define syserr_regalloc_reinit2 "regalloc_reinit2"
#define syserr_regheap "Register heap overflow"
#define syserr_bad_fmt_dir "bad fmt directive"
#define syserr_syserr "syserr simulated"
#define syserr_r1r "r1r %ld"
#define syserr_r2r "r2r %ld"
#define syserr_r3r "r3r %ld"
#define syserr_r4r "r4r %ld"
#define syserr_expand_jop "expand_jop(2address)"
#define syserr_nonauto_active "Non auto 'active_binders' element"
#define syserr_size_of_binder "size_of_binder"
#define syserr_insertblockbetween "insertblockbetween(%ld, %ld)"
#define syserr_reopen_block "reopen_block called"
#define syserr_scaled_address "emit5(scaled address)"
#define syserr_expand_pushr "expand_jop_macro(PUSHR)"
#define syserr_remove_noop_failed "remove_noop failed"
#define syserr_remove_noop_failed2 "remove_noop failed2"
#define syserr_bad_bindaddr "Bad bindaddr_() with LDRVx1"
#define syserr_ldrfk "duff LD/STRF/DK 0x%lx"
#define syserr_ldrk "duff LD/STRK 0x%lx"
#define syserr_ldrbk "duff LD/STRBK 0x%lx"
#define syserr_ldrwk "duff LD/STRWK 0x%lx"
#define syserr_branch_backptr "Bad back-pointer code in branch_chain"
#define syserr_no_main_exit "use_cond_field(no main exit)"
#define syserr_two_returns "Two return exits from a block"
#define syserr_unrefblock "unrefblock"
#define syserr_zip_blocks "zip_blocks(SETSP confused %ld!=%ld)"
#define syserr_live_empty_block "ALIVE empty block L%ld"
#define syserr_loctype "loctype"
#define syserr_adconbase "cse_adconbase"
#define syserr_find_exprn "CSE: find_exprn %ld"
#define syserr_removecomparison "CSE: removecomparison %lx"
#define syserr_evalconst "CSE: evalconst %lx"
#define syserr_scanblock "cse_scanblock %08lx"
#define syserr_prune "csescan(prune)"
#define syserr_globalize "globalize_declaree1(%p,%ld)"
#define syserr_globalize1 "globalize_typeexpr(%p,%ld)"
#define syserr_copy_typeexpr "copy_typeexpr(%p,%ld)"
#define syserr_tentative "is_tentative(tmpdataq == NULL)"
#define syserr_tentative1 "is_tentative(ADCON)"
#define syserr_tentative2 "tentative definition confusion"
#define syserr_instate_decl "instate_decl %ld"
#define syserr_totarget "totargetsex(%d)"
#define syserr_vg_wpos "vg_wpos(%ld)"
#define syserr_vg_wflush "vg_wflush(type=0x%x)"
#define syserr_gendcI "gendcI(%ld,%ld)"
#define syserr_vg_wtype "vg_wtype=0x%x"
#define syserr_codevec "code vector overflow"
#define syserr_nonstring_lit "non-string literal: %.8lx"
#define syserr_addr_lit "Address-literals should not arise in HELIOS mode"
#define syserr_dumplits "dumplits(codep&3)"
#define syserr_dumplits1 "codebuf(dumplits1)"
#define syserr_dumplits2 "codebuf(dumplits2)"
#define syserr_outlitword "outlitword confused"
#define syserr_dumplits3 "codebuf(dumplits3)"
#define syserr_addlocalcse "addlocalcse %ld"
#define syserr_cse_lost_def "cse: def missing"
#define syserr_cse_lost_use "cse: use missing"
#define syserr_cse_safetolift "cse: safetolift"
#define syserr_storecse "storecse %ld %ld\n"
#define syserr_baseop "cse: baseop %lx"
#define syserr_cse_wordn "CSE_WORDn"
#define syserr_addcsedefs "addcsedefs"
#define syserr_cse_preheader "cse: loop preheader %d != %ld"
#define syserr_cse_modifycode "cse: modifycode %ld %ld!=%ld"
#define syserr_cse_modifycode_2 "cse: compare ref L%ld not reachable from def L%ld"
#define syserr_cse_makesubdef "cse: MakeSubDef"
#define syserr_referencecsedefs "ReferenceCSEDefs"
#define cse_rewritenext "cse: RewriteNext %ld: %ld"
#define syserr_regtype "ensure_regtype(%lx)"
#define syserr_struct_val "Value of structure requested improperly"
#define syserr_missing_expr "missing expr"
#define syserr_checknot "s_checknot"
#define syserr_structassign_val "value of structure assignment needed"
#define syserr_floating "Float %%"
#define syserr_cg_expr  "cg_expr(%ld = $s)"
#define syserr_bad_reg "bad reg %lx in use"
#define syserr_bad_fp_reg "fp reg in use"
#define syserr_cg_fnarg "cg_fnarg(odd rep %lx)"
#define syserr_fnarg_struct "cg(struct arg confused)"
#define syserr_fnret_struct "struct returning s_fnap"
#define syserr_cg_fnarg1 "cg_fnargs confused"
#define syserr_cg_argcount "arg count confused"
#define syserr_cg_fnarg2 "cg_fnargs tidy"
#define syserr_padbinder "odd padbinder $b in cg_fnargs()"
#define syserr_null_nb "nb==NULL in cg.c (A)"
#define syserr_cg_fnap "cg_fnap"
#define syserr_cg_fnap_1 "cg_fnap_1(typefnaux-mismatch)"
#define syserr_cg_return_struct "cg_return(struct)"
#define syserr_cg_cmd "cg_cmd(%ld = $s)"
#define syserr_cg_endcase "cg_cmd(endcase)"
#define syserr_cg_break "cg_cmd(break)"
#define syserr_cg_cont "cg_cmd(cont)"
#define syserr_cg_switch "switch expression must have integer type"
#define syserr_cg_caselist "cg_caselist"
#define syserr_cg_case "cg_cmd(case)"
#define syserr_unset_case "Unset case_lab"
#define syserr_cg_default "cg_cmd(default)"
#define syserr_cg_badrep "rep bad in comparison %.8lx"
#define syserr_cg_plain "(plain) qualifies non-<narrow-int-binder>"
#define syserr_cg_cast "Illegal cast involving a structure or union"
#define syserr_cg_fpsize "fp sizes are wrong %ld %ld"
#define syserr_cg_cast1 "bad mode %ld in cast expression"
#define syserr_cg_cast2 "cast %ld %ld %ld %ld"
#define syserr_cg_indexword "Indexed address mode with word-store"
#define syserr_cg_bad_width "bad width %ld in cg_stind"
#define syserr_cg_bad_mode "bad mcmode %ld in cg_stind"
#define syserr_chroma "chroma_check(target.h setup wrong or multi-temp op confused)"
#define syserr_Q_swap "Q_swap(%lx)"
#define syserr_cg_stgclass "Funny storage class %#lx"
#define syserr_cg_storein "cg_storein(%ld)"
#define syserr_cg_addr "p nasty in '&(p=q)'"
#define syserr_cg_addr1 "cg_addr(%ld)"
#define syserr_cg_shift0 "n=0 in shift_op1"
#define syserr_not_shift "not a shift in shift_operand()"
#define syserr_not_shift1 "not a shift in shift_amount()"
#define syserr_integer_expected "integer expression expected"
#define syserr_nonauto_arg "Non-auto arg!"
#define syserr_struct_result "Unexpected struct result"
#define syserr_cg_topdecl "cg_topdecl(not fn type)"
#define syserr_cg_unknown "unknown top level %ld"
#define syserr_cg_narrowformal "unwidened formal"
#define syserr_emitfloat1 "Bad FP op with emitfloat1"
#define syserr_regalloc_clash "Register allocation clash (R%d)"

#ifdef TARGET_HAS_AOUT
#define syserr_aout_reloc "relocate_code_to_data(PCreloc)"
#define syserr_aout_checksym "obj_checksym(%s)"
#define syserr_aout_reloc1 "obj_coderelocation %.8lx"
#define syserr_aout_gendata "obj_gendata(%ld)"
#define syserr_aout_datalen "obj_data len=%ld"
#define syserr_aout_data "obj_data %ldEL%ld'%s'"
#define syserr_aout_debug "writedebug: aoutobj linked with xxxdbg not dbx"
#  ifdef TARGET_HAS_DEBUGGER  /* dbx support */
#define syserr_too_many_types "too many types in dbx"
#define syserr_addcodep "bad pointer in dbg_addcodep"
#define syserr_tagbindsort "bad tagbindsort 0x%08lx"
#define syserr_sprinttype "sprinttype(%p,0x%lx)"
#define syserr_dbx_locvar "debugger table confusion(local variable $r %lx %lx)"
#define syserr_dbx_scope "dbg_scope"
#define syserr_dbx_proc "dbg_proc"
#define syserr_dbx_proc1 "dbg_proc confused"
#define syserr_dbx_write "dbg_write(%lx)"
#  endif
#endif

#ifdef TARGET_HAS_COFF
#define syserr_coff_reloc "relocate_code_to_data(PCreloc)"
#define syserr_coff_pcrel "coff(unexpected X_PCreloc)"
#define syserr_coff_m88000 "coffobj(X_DataAddr needs extending)"
#define syserr_coff_toobig "coffobj(Module over 64K -- fix)"
#define syserr_coff_checksym "obj_checksym($r)"
#define syserr_coff_reloc1 "obj_coderelocation(%.8lx)"
#define syserr_coff_gendata "obj_gendata(%ld)"
#define syserr_coff_datalen "obj_data len=%ld"
#define syserr_coff_data "obj_data %ldEL%ld'%s'"
#  ifdef TARGET_HAS_DEBUGGER  /* dbx support */
#define syserr_too_many_types "too many types in dbx"
#define syserr_addcodep "bad pointer in dbg_addcodep"
#define syserr_tagbindsort "bad tagbindsort 0x%08lx"
#define syserr_sprinttype "sprinttype(%p,0x%lx)"
#define syserr_dbx_locvar "debugger table confusion(local variable $r %lx %lx)"
#define syserr_dbx_scope "dbg_scope"
#define syserr_dbx_proc "dbg_proc"
#define syserr_dbx_proc1 "dbg_proc confused"
#define syserr_dbx_write "dbg_write(%lx)"
#  endif
#endif

#ifndef NLS                     /* These become tags */

%Z      /* The following remain as ordinary (uncompressed) strings */

/*
 * @@@ Wording here is subject to change...
 */
#define misc_message_warning   "Warning: "
#define misc_message_error     "Error: "
#define misc_message_serious   "Serious error: "
#define misc_message_fatal     "Fatal error: "
#define misc_message_abandoned "\nCompilation abandoned.\n"

/*
 * The following are used in init_sym_name_table() and/or ctxtofdeclflag()
 * and eventually find their ways into various error messages.
 */

/* _aftercommand has been added where needed in feerrs.h
 * #define errname_aftercommand    " after command"
 */
#define errname_unset              "<?>"
#define errname_pointertypes       "<after * in declarator>"
#define errname_toplevel           "<top level>"
#define errname_structelement      "<structure component>"
#define errname_formalarg          "<formal parameter>"
#define errname_formaltype         "<formal parameter type declaration>"
#define errname_blockhead          "<head of block>"
#define errname_typename           "<type-name>"
#define errname_unknown            "<unknown context>"
#define errname_error              "<previous error>"
#define errname_invisible          "<invisible>"
#define errname_let                "<let>"
#define errname_character          "<character constant>"
#define errname_wcharacter         "<wide character constant>"
#define errname_integer            "<integer constant>"
#define errname_int64con           "<int64 constant>"
#define errname_boolean            "<boolean constant>"
#define errname_floatcon           "<floating constant>"
#define errname_string             "<string constant>"
#define errname_wstring            "<wide string constant>"
#define errname_identifier         "<identifier>"
#define errname_binder             "<variable>"
#define errname_tagbind            "<struct/union tag>"
#define errname_simpletype         "<simple type>"
#define errname_conversiontype     "<conversion type>"
#define errname_new_type_name      "<new-type-name>"
#define errname_catch_name         "<catch name>"
#define errname_cond               "_?_:_"
#define errname_displace           "++ or --"
#define errname_postinc            "++"
#define errname_postdec            "--"
#define errname_arrow              "->"
#define errname_dotstar            ".*"
#define errname_arrowstar          "->*"
#define errname_constructor        "<constructor>"
#define errname_destructor         "<destructor>"
#define errname_addrof             "unary &"
#define errname_content            "unary *"
#define errname_monplus            "unary +"
#define errname_neg                "unary -"
#define errname_fnap               "<function argument>"
#define errname_fnarg              "<function argument %d>"
#define errname_subscript          "<subscript>"
#define errname_cast               "<cast>"
#define errname_sizeoftype         "sizeof"
#define errname_sizeofexpr         "sizeof"
#define errname_ptrdiff            "-"   /* for (a-b)=c msg */
#define errname_endcase            "break"
#define errname_block              "<block>"
#define errname_decl               "decl"
#define errname_fndef              "fndef"
#define errname_typespec           "typespec"
#define errname_typedefname        "typedefname"
#define errname_valof              "valof"
#define errname_ellipsis           "..."
#define errname_init               "="
#define errname_eol                "\\n"
#define errname_eof                "<eof>"
#define errname_longdouble         "long double" /* there is no s_longdouble */
#define errname_membertemplate     "<member template>"
#define errname_classtemplate      "<class template>"

#ifdef RANGECHECK_SUPPORTED
#  define errname_rangecheck       "<rangecheck>"
#  define errname_checknot         "<checknot>"
#endif

#endif                          /* ndef NLS */

/* end of miperrs.h */
