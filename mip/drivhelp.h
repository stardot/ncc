/*
 * drivhelp.c -- help text for Norcroft C/FORTRAN compiler, version 1b.
 * Copyright (C) Codemist Ltd., 1989.
 * Copyright (C) Acorn Computers Ltd., 1989.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

/* The help text here is not what is wanted on non-Acorn machines, and  */
/* is also not what is wanted for a Lint system.                        */
/* parameterisation here seems in the process of getting out of hand!   */

static msg_t driver_help_text[] = {

#ifdef msg_driver_help         /* to allow options.h to define */
    msg_driver_help,

#else
#ifndef TARGET_IS_UNIX
/* Arthur and Brazil -help text */

#ifdef PASCAL /*ECN*/

    help_usage,
    help_main_options,
    help_blank,
    help_list,                  /* -list */
    help_iso,                   /* -iso */
    help_blank,
    help_dont_link,             /* -c */
    help_leave_comments,        /* -C */
    help_predefine,             /* -D<symbol> */
    help_preprocess_pascal,     /* -E */
    help_compiler_features,     /* -F<options> */
    help_runtime_checks,        /* -R<options> */
    help_debug,                 /* -g<options> */
    help_include_I,             /* -I<directory> */
    help_include_J,             /* -J<directory> */
    help_libraries,             /* -L<libs> */
    help_output,                /* -o<file> */
    help_profile,               /* -P<options> */
    help_output_assembler,      /* -S */
    help_preundefine,           /* -U<symbol> */
    help_disable_warnings,      /* -W<options> */

#else

#ifndef FORTRAN

    help_usage,
    help_main_options,
    help_blank,
    help_list,                  /* -list */
    help_pcc,                   /* -pcc */
    help_blank,
    help_dont_link,             /* -c */
    help_leave_comments,        /* -C */
    help_predefine,             /* -D<symbol> */
    help_preprocess_c,          /* -E */
    help_compiler_features,     /* -F<options> */
    help_debug,                 /* -g<options> */
    help_include_I,             /* -I<directory> */
    help_include_J,             /* -J<directory> */
    help_libraries,             /* -L<libs> */
    help_output,                /* -o<file> */
    help_profile,               /* -P<options> */
    help_output_assembler,      /* -S */
    help_preundefine,           /* -U<symbol> */
    help_disable_warnings,      /* -W<options> */

#else /* FORTRAN */

    help_usage,
    help_main_options,
    help_blank,
    help_f66,                   /* -f66 */
    help_list,                  /* -list */
    help_blank,
    help_dont_link,             /* -c */
    help_leave_comments,        /* -C */
    help_predefine,             /* -D<symbol> */
    help_preprocess_fortran,    /* -E */
    help_compiler_features,     /* -F<options> */
    help_debug,                 /* -g<options> */
    help_include_I,             /* -I<directory> */
    help_include_J,             /* -I<directory> */
    help_libraries,             /* -L<libs> */
    help_output,                /* -o<file> */
    help_profile,               /* -P<options> */
    help_output_assembler,      /* -S */
    help_preundefine,           /* -U<symbol> */
    help_disable_warnings,      /* -W<options> */

#endif /* FORTRAN */
#endif /* PASCAL */

#else /* TARGET_IS_UNIX */

#ifdef PASCAL /*ECN*/

#define msg_driver_help ,
    help_blank,
    help_usage,
    help_main_options,
    help_blank,
    help_iso,                   /* -iso */
    help_dont_link_invoke,      /* -c */
    help_leave_comments,        /* -C */
    help_predefine_pp,          /* -D<symbol> */
    help_preprocess_pascal,     /* -E */
    help_compiler_features,     /* -F */
    help_debug_noopt,           /* -g */
    help_include_I,             /* -I<directory> */
    help_list,                  /* -list */
    help_makefile,              /* -M<options> */
    help_output_space,          /* -o <file> */
    help_optimised,             /* -O */
    help_profile_lc,            /* -p<options> */
    help_readonly_strings,      /* -R */
    help_generate_assembler,    /* -S */
    help_preundefine_pp,        /* -U<symbol> */
    help_disable_warnings_lc,   /* -w<options> */

#else

#ifndef FORTRAN

#define msg_driver_help ,
    help_blank,
    help_usage,
    help_main_options,
    help_blank,
    help_ansi,                  /* -ansi */
    help_pcc_bsd,               /* -pcc */
    help_dont_link_invoke,      /* -c */
    help_leave_comments,        /* -C */
    help_predefine_pp,          /* -D<symbol> */
    help_preprocess_c,          /* -E */
    help_compiler_features,     /* -F<options> */
    help_debug_noopt,           /* -g */
    help_include_I,             /* -I<directory> */
    help_list,                  /* -list */
    help_makefile,              /* -M<options> */
    help_output_space,          /* -o <file> */
    help_optimised,             /* -O */
    help_profile_lc,            /* -p<options> */
    help_readonly_strings,      /* -R */
    help_generate_assembler,    /* -S */
    help_preundefine_pp,        /* -U<symbol> */
    help_disable_warnings_lc,   /* -w<options> */

#else /* FORTRAN */

#define msg_driver_help ,
    help_bsd_f77,
    help_usage,
    help_main_options,
    help_blank,
    help_dont_link_invoke,      /* -c */
    help_f66,                   /* -f66 */
    help_debug_noopt,           /* -g */
    help_16bit_ints,            /* -i2 */
    help_list,                  /* -list */
    help_onetrip,               /* -onetrip */
    help_output_space,          /* -o <file> */
    help_optimised,             /* -O */
    help_profile_lc,            /* -p<options> */
    help_readonly_strings_lc,   /* -r */
    help_strict,                /* -strict */
    help_generate_assembler,    /* -S */
    help_dont_downcase,         /* -U */
    help_disable_warnings_lc,   /* -w<options> */

#endif /* FORTRAN */
#endif /* PASCAL */

#endif /* TARGET_IS_UNIX */

#endif /* msg_driver_help */
    NULL
};

/* end of mip/drivhelp.h */
