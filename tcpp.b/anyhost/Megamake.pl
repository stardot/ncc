# '--*-Perl-*--';
#
# Tool-specific definitions for Megamake 3
# Copyright (C) Advanced RISC Machines 1997. All rights reserved.
# SPDX-Licence-Identifier: Apache-2.0
#
# $Revision$
#   $Author$
#     $Date$
#

$dep = 'shared';
$toolname = 'tcpp';
$tooldestname = $toolname;

# Default directories to look for options.h and target.h respectively
$options = 'cppthumb'; ### what's the difference bewtween cc{arm,thumb}/options.h?
$backend = 'thumb';

@srcdirs = (
            "../../$options",
            "../../$backend",
            "../../cfe",
            "../../cppfe",
            "../../mip",
            "../../util",
            "../../armthumb"
            );

&run_include_file ("..".$SEP."armcc.b".$SEP."anyhost".$SEP."CommonMega.pl");


@sources = (@common_sources, @backend_sources, @cpp_sources);

# List of all object code files to be found in the build area, which must be
# built in order to make the tool concerned.
@objall  = (@common_objall, @backend_objall, @cpp_objall);

sub tool_only {
    &tool_only_common;
}


sub tool_host_defs {
    &tool_host_defs_common;
}

