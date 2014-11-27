# Makefile for ANMAP and related programs
#
# Copyright Paul Alexander, MRAO Cambridge
#
# Makefile for Version X0.52 of ANMAP
#
# last updated 15/11/94

# parameters for F77 
FFLAGC=-u
FCOMPL=f77

# parameters for cc
CCOMPL=cc
CFLAGC=-O -DTK_NO_SECURITY

# parameters for TCl/TK and X11
TCL_ROOT = /soft/tcl/v7.4
TCL_DIR = $(TCL_ROOT)/tcl
TK_DIR = $(TCL_ROOT)/tk
TCL_INCDIR = -I$(TCL_ROOT)/include -I../include

# parameters for linking anmap
C_LINK = -leqn -lm

# defaults compile for F77 files
.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

# default compile for CC files
.c.o:
	$(CCOMPL) $(CFLAGC) $(TCL_INCDIR) -c $<

# files defined in this directory
INCLUDE_FILE=\
	/mrao/anmap/include/anmap_sys_pars.inc \
        /mrao/anmap/include/error_file_full.inc

ANMAP_OBJECT=\
	AnmapInit.o

SYNCH_OBJECT=\
	Anmap.o SynchInit.o

XANMAP_OBJECT=\
	XanmapInit.o

# object for linking package
all : Anmap Xanmap
Sall : Static-Anmap Static-Xanmap

# object to force a full recompilation
touch :
	touch *.c
	touch *.f

# dependencies
.o : $(INCLUDE_FILES)

# object for linking anmap
Anmap : $(ANMAP_OBJECT) $(SOURCE_OBJECT) 
	f77 -o Anmap  $(ANMAP_OBJECT) \
	`../bin/anmlink` `../bin/anmlink` `../bin/tklink` $(C_LINK)
	mv Anmap ../bin/Anmap_exec

Static-Anmap : $(ANMAP_OBJECT) $(SOURCE_OBJECT) 
	f77 -o SAnmap  $(ANMAP_OBJECT) \
	-Bstatic `../bin/anmlink` `../bin/anmlink` `../bin/tklink` $(C_LINK)
	mv SAnmap ../bin/Anmap_exec

Xanmap : $(XANMAP_OBJECT) $(SOURCE_OBJECT)
	f77 -o Xanmap  $(XANMAP_OBJECT) \
	`../bin/anmlink` `../bin/anmlink` `../bin/tklink` $(C_LINK)
	mv Xanmap ../bin/Xanmap_exec

Static-Xanmap : $(XANMAP_OBJECT) $(SOURCE_OBJECT)
	f77 -o SXanmap  $(XANMAP_OBJECT) \
	-Bstatic `../bin/anmlink` `../bin/anmlink` `../bin/tklink` $(C_LINK)
	mv SXanmap ../bin/Xanmap_exec

Synch : $(SYNCH_OBJECT)
	f77 -o Synch  $(SYNCH_OBJECT) \
	`../bin/anmlink` `../bin/anmlink` `../bin/tklink` $(C_LINK)

test : $(XANMAP_OBJECT) $(SOURCE_OBJECT)
	f77 -o Tanmap  $(XANMAP_OBJECT) \
	`../bin/anmlink` `../bin/anmlink` `../bin/tklink` $(C_LINK)

# target to cleanup after a build
clean :
	rm -f *.o
	rm -f Xanmap SXanmap Anmap SAnmap
