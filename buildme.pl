# '--*-Perl-*--';
#
# Simple script to build this tool.
#
# To be called when the sources are all known to be present and
# up to date. Assumes that the current working directory is the
# top-level directory for this tool, and that the environment
# variables necessary to specify the correct compiler have been
# set up.
#
# Copyright (C) Advanced RISC Machines 1997. All rights reserved.
# SPDX-Licence-Identifier: Apache-2.0
#
# $Revision$
#   $Author$
#     $Date$
#
#

require Buildinit;

if ($running_on eq 'unix') {

    &do_megamake_build ("armcc$esuffix" , "armcc.b");
    &do_megamake_build ("tcc$esuffix"   , "tcc.b");
    &do_megamake_build ("armcpp$esuffix", "armcpp.b");
    &do_megamake_build ("tcpp$esuffix"  , "tcpp.b");

    # This requires armcpp to be on your path:
    &do_megamake_build ("cpplib_all"    , "cpplib/cpplib.b");

# Should also build armlib libraries

}
else
{

# not on unix

    &do_megamake_build ("armcc$lsuffix" , "armcc.b");
    &do_megamake_build ("tcc$lsuffix"   , "tcc.b");
    &do_megamake_build ("armcpp$lsuffix", "armcpp.b");
    &do_megamake_build ("tcpp$lsuffix"  , "tcpp.b");
# Note, the .err files get made as a side-effect of making tags.h so we do not
# need to make them explicitly.

    &do_megamake_build ("clbcomp$lsuffix", "clbcomp.b") if ($ENV{'CPP_DEBUGGER'});

    @extdirs =
        (
         "../../clx",
         "../../winsupp/dllsupp",
         );
    @extfiles =
        (
         "..\\..\\clx clx.b\\$target\\clx$lsuffix",
         "..\\..\\winsupp dllsupp\\$target\\toolgui"
         );
    &do_vcm_build ("armcc",  "dll");
    &do_vcm_build ("tcc",    "dll");
    &do_vcm_build ("armcpp", "dll");
    &do_vcm_build ("tcpp",   "dll");

# This one is special - it does not need a makefile at all, as the default
# rules build it. However, we need to mess about a bit to distinguish versions
# for different platforms. NB This is all horrible and PC-specific. It would be
# much cleaner to write a Megamake file for it.

    chdir ("cl\\util") || die "Failed to change directory to cl\\util";
    if (! -d $target) {
        mkdir ($target, 0) || die "Failed to create directory $target in cl\\util";
    }
    `copy makemake.c $target`;
    chdir ("..\\..");
    @extfiles = (); @extdirs = ();
    &do_build ("set CFLAGS=-I..\\..\\..\\..\\clx && $make makemake.exe", "cl/util/$target", 0);
    `del cl\\util\\$target\\makemake.c`;

    # This requires armcpp to be on your path:
    &do_megamake_build ("cpplib_all", "cpplib\\cpplib.b");

# Should also build armlib libraries

} # endif (not on unix)
