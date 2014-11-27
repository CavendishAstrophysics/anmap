# Makefile for the MRAO/ANMAP iocmd library
#
#   last updated 2/3/92 P. Alexander
#
# parameters for cc
CCOMPL=cc
CFLAGC=-O

# parameters for TCl/TK and X11
TCL_INCDIR = -I/soft/tcl/v7.4/include -I../include

# parameters for f77
FCOMPL=f77

.c.o:
	$(CCOMPL) $(CFLAGC) $(TCL_INCDIR) -c $<
.f.o:
	$(FCOMPL) -c -u $(FFLAGC) $<

OBJECT_FILES=\
	tcltk_Init.o		anm_exec.o \
	anm_start.o		anm_end.o \
	anm_exit.o 		anm_dispatch.o \
	anm_disstart.o		anm_disend.o \
	pgplot_cmd.o \
	iocmd_getoper.o		iocmd_pars2tcl.o \
	iocmd_tclvars.o		iocmd_tclsetl.o \
	iocmd_tclsets.o		iocmd_tclseti.o \
	iocmd_tclsetr.o		iocmd_tclsetd.o \
	iocmd_tclenq.o		iocmd_err.o \
	iocmd_enqvar.o		iocmd_setvar.o \
	iocmd_wrerr.o		atool_Init.o \
	eval_eqn.o		eval_list.o \
	imcat_Init.o \
	datadisplay_sys.o	edit_redt_sys.o \
	plot_util_subs.o	drawing_sys.o \
	image_edit.o		scratch_sys.o


# system dependencies
sunos : libAnmap.a
	ranlib ../lib/libAnmap.a

solaris : libAnmap.a

# Dependencies for building the object library
libAnmap.a : $(OBJECT_FILES)
	ar ru ../lib/libAnmap.a $(OBJECT_FILES)

# Target for cleaning up the source directory
clean :
	rm -f $(OBJECT_FILES)







