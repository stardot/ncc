# Makefile to run, or to create script to run, compiler tests.
# Copyright (C) Advanced RISC Machines 1994-7. All rights reserved.
# SPDX-Licence-Identifier: Apache-2.0
#
# RCS $Revision$
# Checkin $Date$
# Revising $Author$

noarg: pc

ECHO=echo # parameterize by host?

PN=pname
COPYRIGHT=../../../Megamake/copyright

CPPLVIA=cpplvia
SUPPORTED=unix pc mac riscos

DOECHO=1
ARMSD=armsd -proc ARM7TDMI
CC=armcc
LD=armlink
CLIBDIR=/chips/sdt/rel210/lib/demon

PCSZ=32
ENDIAN=l

APCSSEP=/
mac := APCSSEP=†/
APCS=3$(APCSSEP)$(PCSZ)$(APCSSEP)swst$(APCSSEP)softfp
COMPFLAGS=-$(ENDIAN)i -apcs $(APCS) $(CFL)
$(SUPPORTED) := COMPFLAGS += -W
LFLAGS=-d -remove
EXEC=$(ARMSD) -$(ENDIAN)
#TESTSDIR=$(CPPDIR)/tests
TESTSDIR=..
UTILDIR=../../../testsupt
UTILSRC=$(UTILDIR)/testutil.c
UTILSRC2=$(UTILDIR)/testutild.c
CLIB=$(CLIBDIR)/armlib.$(PCSZ)$(ENDIAN)
INCLUDE=$(UTILDIR)
HOST_OBJ_IN_SRC_DIR=0
mac := HOST_OBJ_IN_SRC_DIR=1
LOG1=
LOG2=
CC2=$(CC)

unix := PFL=-u
unix := GETSRC=chmod -f +w \$$@; cp -p
unix := RM=rm -f *.o
unix := HOST=unix

pc  := PFL=-d
pc  := GETSRC=unix2dos
pc  := RM=DEL *.o
pc  := HOST=pc
pc  := UNIX2DOS=unix2dos
pc  := SRCHOST=//sun17
pc  := COMPFLAGS=
pc  := LOG1=%STDOUT% $$r.log
pc  := LOG2=-errors err.log
pc  := EXEC=$(ARMSD) %2
pc  := CLIBDIR=
pc  := APCS=
pc  := CLIB=
pc  := CC=%%*
pc  := CC2=%*

mac := PFL=-m
mac := GETSRC=Duplicate -y
mac := RM=rm *.o
mac := HOST=mac

riscos := PFL=-r
riscos := DEFSRC=nfs:/plg/$(DEFSRCDIR)
riscos := GETSRC=copy
riscos := RM=wipe o.* ~c
riscos := HOST=riscos
riscos := DOECHO=0
riscos := EXEC=

$(SUPPORTED): create_runtests

tests := UTILLIB=testutil.o
tests := INCLUDE=$(UTILDIR)

TEST=*.c
KEEP=0
SHOW=0

.SILENT:
.IGNORE:

tests: armsd.ini
	if [ $(SHOW) -ne 0 ] ; then set -x ; fi ; \
	rm -f summary ; \
	echo '***' compiling $(UTILLIB)... ; \
	$(CC2) $(COMPFLAGS) $(EXTRAUTILCOMPFLAGS) -c -I$(INCLUDE) $(UTILSRC) $(UTILSRC2); \
	for j in $(TESTSDIR)/$(TEST); do \
	    r=`basename $$j .c` ; \
	    case $$r in \
	    *-1) b=`basename $$r -1`; r="$$b-*" ; j=$(TESTSDIR)/$$r.c ; o=$$r.o ;; \
	    *-*) continue	 ;; \
	    *)	 b=$$r ; o=$$r.o ;; \
	    esac ; \
	    rm -f $$b $$b.e ; \
	    echo -n '***' test "$$b " ; \
	    compile_ok=1 ; \
	    $(CC) $(COMPFLAGS) -c -I$(INCLUDE) $$j > $$b.e 2>&1 || compile_ok=0 ; \
	    if [ $$compile_ok -eq 1 ] ; then \
		$(LD) $(LFLAGS) -o $$b $$o $(UTILLIB) >> $$b.e 2>&1 || compile_ok=0 ; \
	    fi ; \
	    sed -n -e '/^ARM Linker: finished, /d' -e '/[Ee]rror[^s]/p' -e '/Fatal/p' < $$b.e > $$b.ee ; \
	    if [ \( $$compile_ok -ne 1 \) -o -s $$b.ee ] ; then \
		echo "- FAILED to compile/link" ; \
		cat $$b.e ; \
	    else \
		if [ $(KEEP) -eq 0 ] ; then rm -f $$o ; fi ; \
		$(EXEC) $$b >.asdlog 2>&1 ; \
		sed -n -e '/FAILED/s/armsd: //' -e '/FAILED/s/ -/-/p' .asdlog > $$b.ee ; \
		if [ -s $$b.ee ] ; then \
		    remove_error_file=0 ; \
		else \
		    remove_error_file=1 ; \
		    sed -n -e '/OK/s/armsd: //' -e '/OK/s/ -/-/p' .asdlog > $$b.ee ; \
		fi ; \
		if [ -s $$b.ee ] ; then \
		    cat $$b.ee ; \
		    if [ $$remove_error_file -ne 0 ] ; then \
			 if [ $(KEEP) -eq 0 ] ; then rm $$b.e ; fi ; \
		    else \
			 cp .asdlog $$b.e ; \
		    fi ; \
		    if [ $(KEEP) -eq 0 ] ; then rm $$b ; fi ; \
		else \
		    echo " - execution FAILED" ; \
		    cat .asdlog ; \
		fi ; \
	    fi ; \
	    if [ $(KEEP) -eq 0 ] ; then rm $$b.ee ; fi ; \
	done ; \
	if [ $(KEEP) -eq 0 ] ; then rm $(UTILLIB) ; fi

armsd.ini:
	echo go > $@
	echo quit >> $@

create_runtests:
# here we use '$(PN} $(PFL) "%F" `basename $foo .c`.o' instead of
# '$(PN} $(PFL) "%O" $foo' because we want to use the foo.c => foo.o
# convention instead of using host object file naming convention
	if [ $(DOECHO) -eq 1 ] ; then \
	    echo $(ECHO) go '>' armsd.ini > runtests.$(HOST) ; \
	    echo $(ECHO) quit '>>' armsd.ini >> runtests.$(HOST) ; \
	else \
	    echo -n > runtests.$(HOST) ; \
	fi ; \
	xUTILSRC=`$(PN) $(PFL) "%F" $(UTILSRC)` ; \
	xUTILSRC2=`$(PN) $(PFL) "%F" $(UTILSRC2)` ; \
	utilobj=`$(PN) $(PFL) "%F" \`basename $(UTILSRC) .c\`.o` ; \
	utilobj2=`$(PN) $(PFL) "%F" \`basename $(UTILSRC2) .c\`.o` ; \
	xCPPLIB=`$(PN) $(PFL) "%F" $(CPPLIB)` ; \
	xCLIB=`$(PN) $(PFL) "%F" $(CLIB)` ; \
	xINCLUDE=`$(PN) $(PFL) "%Y" $(INCLUDE)` ; \
    xLOG='$(LOG)' ; \
	if [ $(DOECHO) -eq 1 ] ; then \
	    echo $(ECHO) "'***'" compiling util.o... >> runtests.$(HOST) ; \
	fi ; \
	echo $(CC2) $(COMPFLAGS) $(EXTRAUTILCOMPFLAGS) -c -I$$xINCLUDE $$xUTILSRC -o $$utilobj $$xLOG >> runtests.$(HOST) ; \
	echo $(CC2) $(COMPFLAGS) $(EXTRAUTILCOMPFLAGS) -c -I$$xINCLUDE $$xUTILSRC2 -o $$utilobj2 $$xLOG >> runtests.$(HOST) ; \
	for j in $(TEST) ; do \
	    r=`basename $$j .c` ; \
	    case $$r in \
	    *-1) b=`basename $$r -1` ; \
		if [ $(DOECHO) -eq 1 ] ; then \
		    echo $(ECHO) "'***'" test $$b ; \
		fi ; \
		if [ $(HOST_OBJ_IN_SRC_DIR) -eq 1 ] ; then \
		    if [ -f $$b.h ] ; then \
			$(PN) $(PFL) "$(GETSRC) %F %Y%;" $(TESTSDIR)/$$b.h . ; \
		    fi ; \
		    srcs=`$(PN) $(PFL) "%F" $$j` ; \
		    $(PN) $(PFL) "$(GETSRC) %F %Y%;" $(TESTSDIR)/$$j . ; \
		else \
		    srcs=`$(PN) $(PFL) "%F" $(TESTSDIR)/$$j` ; \
		fi ; \
		objs=`$(PN) $(PFL) "%O" $$j` ; \
		;; \
	    *-[2-8]) \
		if [ $(HOST_OBJ_IN_SRC_DIR) -eq 1 ] ; then \
		    srcs="$$srcs `$(PN) $(PFL) "%F" $$j`" ; \
		    $(PN) $(PFL) "$(GETSRC) %F %Y%;" $(TESTSDIR)/$$j . ; \
		else \
		    srcs="$$srcs `$(PN) $(PFL) "%F" $(TESTSDIR)/$$j`" ; \
		fi ; \
		objs="$$objs `$(PN) $(PFL) "%OF" $$j`" ; \
		;; \
	    *-9) \
		if [ $(HOST_OBJ_IN_SRC_DIR) -eq 1 ] ; then \
		    srcs="$$srcs `$(PN) $(PFL) "%F" $$j`" ; \
		    $(PN) $(PFL) "$(GETSRC) %F %Y%;" $(TESTSDIR)/$$j . ; \
		else \
		    srcs="$$srcs `$(PN) $(PFL) "%F" $(TESTSDIR)/$$j`"  ; \
		fi ; \
		objs="$$objs `$(PN) $(PFL) "%O" $$j`" ; \
		$(PN) $(PFL) "$(CC) $(COMPFLAGS) -c -I$$xINCLUDE $$srcs $(LOG2)%;" ; \
		echo $(LD) -d -o $$b $$objs $$utilobj $$utilobj2 $$xCPPLIB $$xCLIB $(LOG1); \
		echo $(EXEC) $$b $(LOG1); \
		;; \
	    *)	 \
		if [ $(DOECHO) -eq 1 ] ; then \
		    echo $(ECHO) "'***'" test $$r ; \
		fi ; \
		obj=`$(PN) $(PFL) "%F" $$r.o` ; \
		$(PN) $(PFL) "$(CC) $(COMPFLAGS) -c -I$$xINCLUDE %F -o $$obj $(LOG2)%;" \
		     $(TESTSDIR)/$$j ; \
		if [ $(HOST) -eq pc ] ; then \
		    echo "cat err.log $(LOG1)" ; \
		fi ; \
		echo $(LD) -d -o $$r $$obj $$utilobj $$utilobj2 $$xCPPLIB $$xCLIB $(LOG1); \
		echo $(EXEC) $$r $(LOG1); \
		;; \
	    esac ; \
	done >> runtests.$(HOST) ; \
	if [ "$(UNIX2DOS)" != "" ] ; then \
	    $(UNIX2DOS) runtests.$(HOST) runtests.bat ; \
	fi
