# '--*-Perl-*--';
#
# Tool-specific definitions for Megamake 3
# Common Megamake file for the C/C++ compilers, included by each Megamake.pl
# Copyright (C) Advanced RISC Machines 1997. All rights reserved.
# SPDX-Licence-Identifier: Apache-2.0
#
# $Revision$
#   $Author$
#     $Date$
#


######################################################################
# This section contains TOOL specifc stuff which is NOT HOST dependent
#  (ie. it will need customising for a particular tool, but only once)

@backend_sources=
    (
     "armthumb/aaof.c", "armthumb/asd.c", "armthumb/dwasd.c",
     "$backend/asm.c", "$backend/gen.c",
     "$backend/mcdep.c", "$backend/peephole.c",
     "armthumb/asmsyn.c", "armthumb/asmcg.c",
     "mip/dwarf.c", "mip/dwarf1.c", "mip/dwarf2.c", "armthumb/tooledit.c",
     "armthumb/arminst.c",
    );

# .h files used to be in sources and are only for Makesrc -- which is obsolete
#@backend_headers =
#    (
#     "$backend/mcdpriv.h", "$backend/mcvsn.h", "$backend/ops.h",
#     "$backend/target.h",
#     "armthumb/inlnasm.h",
#     "armthumb/arminst.h", "armthumb/armops.h"
#    );

@cpp_sources =
    (
     "cppfe/doe.c", "cppfe/overload.c", "cppfe/xbind.c", "cppfe/xbuiltin.c",
     "cppfe/xlex.c", "cppfe/xsem.c", "cppfe/xsyn.c", "cppfe/xvargen.c"
    );

@c_sources =
    (
     "mip/bind.c", "mip/builtin.c", "cfe/lex.c", "cfe/sem.c",
     "cfe/syn.c", "cfe/vargen.c"
    );

@common_sources =
    (
     "mip/aetree.c", "mip/cg.c",
     "mip/codebuf.c", "mip/compiler.c", "mip/cse.c", "mip/csescan.c", "mip/cseeval.c",
     "mip/driver.c",
     "mip/flowgraf.c", "mip/inline.c", "mip/jopprint.c",
     "mip/misc.c", "mip/regalloc.c", "mip/regsets.c", "mip/sr.c",
     "mip/store.c", "mip/main.c", "mip/dump.c", "mip/version.c",
     "cfe/pp.c", "cfe/simplify.c",
    );

# .h files used to be in sources and are only for Makesrc -- which is obsolete
#@common_headers =
#    (
#     "mip/aeops.h", "mip/aetree.h", "mip/bind.h", "mip/builtin.h",
#     "mip/cg.h", "mip/cgdefs.h", "mip/codebuf.h", "mip/compiler.h",
#     "mip/cse.h", "mip/cseguts.h", "mip/defaults.h", "mip/defs.h",
#     "mip/drivhelp.h", "mip/flowgraf.h", "mip/globals.h",
#     "mip/version.h", "mip/inline.h", "mip/jopcode.h", "mip/mcdep.h",
#     "mip/mipvsn.h",
#     "mip/regalloc.h", "mip/regsets.h", "mip/sr.h", "mip/store.h",
#     "mip/util.h", "mip/xrefs.h", "mip/dump.h", "mip/dw_int.h",
#     "cfe/fevsn.h", "cfe/lex.h", "cfe/pp.h", "cfe/sem.h", "cfe/simplify.h",
#     "cfe/syn.h", "cfe/vargen.h",
#     "$options/options.h"
#    );


# copy_only was for Makesrc -- it's now obsolete
#@c_copy_only =
#    (
#     "cl/stdh/assert.h", "cl/stdh/signal.h", "cl/stdh/ctype.h",
#     "cl/stdh/stdarg.h", "cl/stdh/errno.h", "cl/stdh/stddef.h",
#     "cl/stdh/float.h", "cl/stdh/stdio.h", "cl/stdh/limits.h",
#     "cl/stdh/stdlib.h", "cl/stdh/locale.h", "cl/stdh/string.h",
#     "cl/stdh/math.h", "cl/stdh/time.h", "cl/stdh/setjmp.h", "cl/stdh/iso646.h",
#     "$backend/peeppat", "armthumb/mcerrs.h", "mip/miperrs.h", "cfe/feerrs.h",
#     "util/peepgen.c", "util/genhdrs.c"
#    );
#
#@cpp_copy_only = @c_sources;

# @extdirs holds include files for use on the host system. @extlibs is a list
# of external object library files which can be found on the host in the directory
# $extdirs[n]/$host. Should not need to be tool specific.
sub tool_host_defs_common {
    if ($host eq 'intelrel' || $host eq 'inteldbg' ||
        $host eq 'alpharel' || $host eq 'alphadbg') {
        $targetcfl .= " $dfl"."LINKER_IS_SUBPROGRAM";
        @extdirs = ( "../../../clx",    "../../../armlink" );
        @extlibs = ( "clx.b/$host/clx", "cclink.b/$host/armlink" );
    } else {
        @extdirs = ( "../../../clx" );
        @extlibs = ( "clx.b/$host/clx" );
    }
}

# List of all object code files to be found in the build area, which must be
# built in order to make the tool concerned.
@backend_objall = qw
    ( aaof asd dwasd asm gen mcdep peephole dwarf dwarf1 dwarf2 tooledit
      asmsyn asmcg arminst
    );
@c_objall = qw
    ( bind builtin lex sem syn vargen );
@cpp_objall = qw
    ( overload xbind xbuiltin xlex xsem xsyn xvargen );
@common_objall = qw
    ( aetree cg codebuf compiler cse csescan cseeval sr flowgraf inline
      jopprint misc version driver main dump
      regalloc regsets store pp simplify
    );


@hdrfiles = qw
    ( assert ctype errno float iso646 limits locale math setjmp
      signal stdarg stddef stdio stdlib string time
    );


# Is there a better way of doing this? return true if files are identical,
# ignoring matches with 3rd arg

sub compare_files {
    my $file1 = shift;
    my $file2 = shift;
    my $ignore = shift;
    if (!open (F1, "<$file1")) { return 0; }
    if (!open (F2, "<$file2")) { close F1; return 0; }
    while ($line1 = <F1>) {
        $line1 =~ s/$ignore//;
        if (!($line2 = <F2>)) {
            close F1;
            close F2;
            return 0;
        }
        $line2 =~ s/$ignore//;
        if ($line1 ne $line2) {
            close F1;
            close F2;
            return 0;
        }
    }
    if ($line2 = <F2>) {
        close F1;
        close F2;
        return 0;
    }

    close F1;
    close F2;
    return 1;
}


# This bit is host dependent because of datenow.h, but we leave it here..
sub tool_only_common {
    my $helpdir = "helpers-$hostarch";

    # Build the genhdrs and peepgen executables

    mkdir ($helpdir, $dir_mode);
    if ($running_on eq 'pc') {
        chdir ($helpdir);
        `vcvars32 && nmake /nologo /f ..\\..\\util\\Makefile.pc OPTIONS=..\\..\\$options BACKEND=..\\..\\$backend genhdrs.exe peepgen.exe`;
        die "nmake for helper tools failed: $!" if $?;
        chdir '..';
    } else {                    # must be Unix
        my $command = "gcc -O2 -o $helpdir/genhdrs ../util/genhdrs.c";
        print "Building genhdrs: $command\n";
        `$command`;
        die "Compilation of genhdrs failed: $!", if $?;

        my $incs = join (" -I$host/", (@srcdirs, @extdirs));
        $incs = "-I$host/".$incs.$host_user_sys_incl_boundary if ($incs);
        $command = "gcc -O2 -o $helpdir/peepgen $incs ../util/peepgen.c";
        print "Building peepgen: $command\n";
        `$command`;
        die "Compilation of peepgen failed: $!", if $?;
    }

    # Use genhdrs to create derived source

    rename "$host/derived/headers.c", "$host/derived/headers.old";
    my $command = &pname_for_host('%F -o%F -I%F',
                                  "$helpdir/genhdrs",
                                  "$host/derived/headers.c",
                                  "../cl/stdh") . $SEP . " ";
    my $hdrfiles = join (".h ", @hdrfiles) . ".h";
    $command   .= &pname_for_host ('%F ', $hdrfiles);
    `$command`;
    die "genhdrs failed: $!" if $?;
    if (compare_files("$host/derived/headers.old", "$host/derived/headers.c", ", created by genhdrs on.*")) {
        rename "$host/derived/headers.old", "$host/derived/headers.c";
    } else {
        unlink "$host/derived/headers.old";
    }

    rename "$host/derived/errors.h", "$host/derived/errors.old";
    $command = &pname_for_host('%F -e%F -q%F -q%F -q%F',
                               "$helpdir/genhdrs",
                               "$host/derived/errors.h",
                               "../mip/miperrs.h",
                               "../cfe/feerrs.h",
                               "../armthumb/mcerrs.h");
    `$command`;
    die "genhdrs failed: $!" if $?;
    if (compare_files("$host/derived/errors.old", "$host/derived/errors.h", ", created by genhdrs on.*")) {
        rename "$host/derived/errors.old", "$host/derived/errors.h";
    } else {
        unlink "$host/derived/errors.old";
    }

    # Use peepgen to create more derived source

    rename "$host/derived/peeppat.c", "$host/derived/peeppat.old";
    $command = &pname_for_host('%F %F %F',
                               "$helpdir/peepgen",
                               "../$backend/peeppat",
                               "$host/derived/peeppat.c");
    `$command`;
    die "peepgen failed: $!" if $?;
    if (compare_files("$host/derived/peeppat.old", "$host/derived/peeppat.c", "")) {
        rename "$host/derived/peeppat.old", "$host/derived/peeppat.c";
    } else {
        unlink "$host/derived/peeppat.old";
    }

    # Make the tags

    my $workdir = $host.$SEP.'derived';
    chdir ($workdir) || die "***Failed to change directory to $workdir";

    rename "tags.h", "tags.old";
    $command = &pname_for_host('mktag -pz %F %F %F %F',
                               "../../../mip/miperrs.h",
                               "../../../cfe/feerrs.h",
                               "../../../armthumb/mcerrs.h",
                               "tags.h");
    my $result = `$command`;
    die "mktag failed: $!" if $?;
    if (compare_files("tags.old", "tags.h", "")) {
        rename "tags.old", "tags.h";
    } else {
        unlink "tags.old";
    }
    print $result;

    chdir ('..'.$SEP.'..') || die "***Failed to change directory back to top level";

    # make the error messages

    rename "$host/$toolname.err", "$host/$toolname.old";
    $command = &pname_for_host('mkmsg -pqz %F %F %F %F',
                               "../mip/miperrs.h",
                               "../cfe/feerrs.h",
                               "../armthumb/mcerrs.h",
                               "$host/$toolname.err");
    $result = `$command`;
    die "mkmsg failed: $!" if $?;
    if (compare_files("$host/$toolname.old", "$host/$toolname.err", "")) {
        rename "$host/$toolname.old", "$host/$toolname.err";
    } else {
        unlink "$host/$toolname.old";
    }
    print $result;
}

sub tool_local {
    print "Making local $toolname-specific boilerplate\n";

    &exe;
    &out ("\n");
    &lib;
    &out ("\n");

    my $hdrlist = join (".h ", @hdrfiles) . ".h";
    &set_var("HDRFILES", &pname_via_stdin("%F ", $hdrlist));

    my $hdrpaths = "../../cl/stdh/". join (".h ../../cl/stdh/", @hdrfiles) . ".h";
    &set_var("HDRPATHS", &pname_via_stdin("%F ", $hdrpaths));

    &out ("\n");

    &out(&pname("{DERDIR}%j%R%: {DERDIR}%j%R %F %F %F%;",
                "errors.h",
                "genhdrs$esuffix",
                "../../mip/miperrs.h",
                "../../cfe/feerrs.h",
                "../../armthumb/mcerrs.h"));
    &out(&pname("% {DERDIR}%j%R -e{DERDIR}%j%R -q%F -q%F -q%F -s%;%;%;",
                "genhdrs$esuffix",
                "errors.h",
                "../../mip/miperrs.h",
                "../../cfe/feerrs.h",
                "../../armthumb/mcerrs.h"));
    &out(&pname("{DERDIR}%j%R%: %F %F %F%;",
                "tags.h",
                "../../mip/miperrs.h",
                "../../cfe/feerrs.h",
                "../../armthumb/mcerrs.h"));
    &out(&pname("% {MKTAG} -pz %F %F %F {DERDIR}%j%R%;",
                "../../mip/miperrs.h",
                "../../cfe/feerrs.h",
                "../../armthumb/mcerrs.h",
                "tags.h"));
    &out(&pname("% {MKMSG} -pqz %F %F %F %R%;%;",
                "../../mip/miperrs.h",
                "../../cfe/feerrs.h",
                "../../armthumb/mcerrs.h",
                "$toolname.err"));
    &out(&pname("{DERDIR}%j%R%: {DERDIR}%j%R {HDRPATHS}%;",
                "headers.c",
                "genhdrs$esuffix"));
    &out(&pname("% {DERDIR}%j%R -o{DERDIR}%j%R $ifl%F%j {HDRFILES}%;%;",
                "genhdrs$esuffix",
                "headers.c",
                "../../cl/stdh"));
    &out(&pname("{DERDIR}%j%R%: %F%;",
                "genhdrs$esuffix",
                "../../util/genhdrs.c"));
    &out(&pname("% {CL} {CFLAGS2} ".$cfeout."{DERDIR}%j%R %F%;%;",
                "genhdrs$esuffix",
                "../../util/genhdrs.c"));
    &out(&pname("{DERDIR}%j%R%: %F {DERDIR}%j%R%;",
                "peeppat.c",
                "../../$backend/peeppat",
                "peepgen$esuffix"));
    &out(&pname("% {DERDIR}%j%R %F {DERDIR}%j%R%;%;",
                "peepgen$esuffix",
                "../../$backend/peeppat",
                "peeppat.c"));
    &out(&pname("{DERDIR}%j%R%: %F %F %F%;",
                "peepgen$esuffix",
                "../../util/peepgen.c",
                "../../mip/jopcode.h",
                "../../$backend/mcdpriv.h"));
    &out(&pname("% {CL} {CFLAGS2} ".$cfeout."{DERDIR}%j%R %F $ifl%F $ifl%F $ifl%F $ifl%F$user_sys_incl_boundary%;%;",
                "peepgen$esuffix",
                "../../util/peepgen.c",
                $extdirs[0],
                "../../mip",
                $srcdirs[0],
                $srcdirs[1]));
}
