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
$toolname = 'clbcomp';
$tooldestname = $toolname;

# Default directories to look for options.h and target.h respectively
$options = 'clbcomp';

@srcdirs = (
            "../../$options",
            "../../cfe",
            "../../cppfe",
            "../../mip",
            "../../util",
            );

@extdirs = ("../../../clx",
            "../../../armdbg",
            "../../arm"
            );
@dependincdirs = @extdirs;

@sources =
    (
     "clbcomp/clbcomp.c", "cppfe/xsyn.c", "cppfe/xsem.c",
     "cppfe/xbuiltin.c", "mip/aetree.c", "mip/misc.c",
     "cfe/simplify.c", "clbcomp/clb_store.c", "cppfe/xbind.c",
     "cppfe/overload.c", "cppfe/xlex.c",
     "mip/compiler.c", "cfe/pp.c",
     "mip/aeops.h", "mip/aetree.h", "mip/bind.h", "mip/builtin.h",
     "mip/defaults.h", "mip/defs.h", "mip/globals.h", "mip/store.h",
     "mip/util.h",
     "cfe/lex.h", "cfe/pp.h", "cfe/sem.h", "cfe/simplify.h",
     "clbcomp/target.h", "$(OPTIONS)/options.h",
     "cfe/syn.h", "cfe/vargen.h"
     );

# List of all object code files to be found in the build area, which must be
# built in order to make the tool concerned.
@objall = qw
    (
     clbcomp xsyn xsem xbuiltin aetree misc simplify clb_store xbind
     overload xlex compiler pp
     );

sub tool_host_defs {
    $targetcflextra = $dfl."CALLABLE_COMPILER";
}


sub tool_local {
    print "Making local $toolname-specific boilerplate\n";

    &lib;

    &set_var("HDRFILES", "");
    &set_var("HDRPATHS", "");

    &out ("\n");

    &out(&pname("{DERDIR}%j%R%: {DERDIR}%j%R %F %F%;",
                "errors.h",
                "genhdrs$esuffix",
                "../../mip/miperrs.h",
                "../../cfe/feerrs.h"));
    &out(&pname("% {DERDIR}%j%R -e{DERDIR}%j%R -q%F -q%F -s%;%;%;",
                "genhdrs$esuffix",
                "errors.h",
                "../../mip/miperrs.h",
                "../../cfe/feerrs.h"));
    &out(&pname("{DERDIR}%j%R%: %F %F%;",
                "tags.h",
                "../../mip/miperrs.h",
                "../../cfe/feerrs.h"));
    &out(&pname("% {MKTAG} -pz %F %F {DERDIR}%j%R%;",
                "../../mip/miperrs.h",
                "../../cfe/feerrs.h",
                "tags.h"));
    &out(&pname("% {MKMSG} -pqz %F %F %R%;%;",
                "../../mip/miperrs.h",
                "../../cfe/feerrs.h",
                "armcpp.err"));
    &out(&pname("{DERDIR}%j%R%: %F%;",
                "genhdrs$esuffix",
                "../../util/genhdrs.c"));
    &out(&pname("% {CL} {CFLAGS2} ".$cfeout."{DERDIR}%j%R %F%;%;",
                "genhdrs$esuffix",
                "../../util/genhdrs.c"));
}


# This should be more subtle really - it should create a makefile and pipe
# it into make so we only do the ones that need doing:
sub tool_only {
    my $helpdir = "helpers-$hostarch";

    # Build the genhdrs executable

    mkdir ($helpdir, $dir_mode);
    if ($running_on eq 'pc') {
        chdir ($helpdir);
        `vcvars32 && nmake /nologo /f ..\\..\\util\\Makefile.pc OPTIONS=..\\..\\$options BACKEND=..\\..\\arm genhdrs.exe`;
        die "nmake for helper tool failed: $!" if $?;
        chdir '..';
    } else {                    # must be Unix
        my $command = "gcc -O2 -o $helpdir/genhdrs ../util/genhdrs.c";
        print "Building genhdrs: $command\n";
        `$command`;
        die "Compilation of genhdrs failed: $!", if $?;
    }

    # Use genhdrs to create derived source

    my $command = &pname_for_host('%F -e%F -q%F -q%F -s',
                               "$helpdir/genhdrs",
                               "$host/derived/errors.h",
                               "../mip/miperrs.h",
                               "../cfe/feerrs.h");
    `$command`;
    die "genhdrs failed: $!" if $?;

    # Make the tags

    my $workdir = $host.$SEP.'derived';
    chdir ($workdir) || die "***Failed to change directory to $workdir";

    $command = &pname_for_host('mktag -pz %F %F %F',
                               "../../../mip/miperrs.h",
                               "../../../cfe/feerrs.h",
                               "tags.h");
    my $result = `$command`;
    die "mktag failed: $!" if $?;
    print $result;

    chdir ('..'.$SEP.'..') || die "***Failed to change directory back to top level";

    # make the error messages

    $command = &pname_for_host('mkmsg -pqz %F %F %F',
                               "../mip/miperrs.h",
                               "../cfe/feerrs.h",
                               "$host/armcpp.err");
    $result = `$command`;
    die "mkmsg failed: $!" if $?;
    print $result;
}
