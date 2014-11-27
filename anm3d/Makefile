#
# Makefile for the Image/Data Catalogue System
#
#   last updated 1/3/95 P. Alexander
#
# parameters for cc
CCOMPL=cc
CFLAGC=-O -DTK_NO_SECURITY

# parameters for TCl/TK and X11
TCLROOT = /soft/tcl/v7.4
TCL_INCLUDE = ${TCLROOT}/include
TCL_LIB = ${TCLROOT}/lib

C_INCLUDE = -I. -I${TCL_INCLUDE} -I../include -I/usr/local/include

IDA_LINK = -L. -lic -lvogle \
	-L/mrao/lib -lfits -lspec -lmap \
	-lcmd -lutil -lio -lch -lpg\
	-lcpgplot -lpgplot -lnag \
	-L/mrao/anmap/lib

TK_LINK = -L${TCLROOT}/lib ${TCLROOT}/lib/libtk.a  ${TCLROOT}/lib/libtcl.a \
	-L/usr/lib/X11 -lX11 -lsocket -lnsl

STK_LINK = -L${TCLROOT}/lib ${TCLROOT}/lib/libtk.a  ${TCLROOT}/lib/libtcl.a \
	/usr/openwin/lib/libXext.a /usr/openwin/lib/libX11.a \
	-lsocket -lnsl -lw -lintl

C_LINK = -leqn -lm

# parameters for f77
FFLAGC=-c -u
FCOMPL=f77


# default compile for CC files
.c.o:
	$(CCOMPL) $(CFLAGC) $(C_INCLUDE) -c $<

# defaults compile for F77 files
.f.o:
	$(FCOMPL) $(FFLAGC) $<

FINCLUDE = ic_pars.inc	ic_cat.inc	ic_recdefn.inc	ic.inc

CINCLUDE = ic.h

INCLUDE_FILES= ${FINCLUDE} ${CINCLUDE}

FSOURCE=\
	ic_cat.o \
	ic_hashutil.o \
	img_util.o \
	img_apply.o \
	img_fft.o \
	img_geom.o \
	img_fit.o \
	img_anal.o \
	imgRegion.o \
	pgplot_util.o \
	pgplot_graph.o \
	pgplot_key.o \
	pgplot_cttools.o

CSOURCE=\
	ic_Init.o \
	ic_iclPriv.o \
	ic_hash.o \
	idaUtil.o \
	imgPriv.o \
	imgUtil.o \
	imgApply.o \
	imgGeom.o \
	imgFit.o \
	imgAnal.o \
	imgPixel.o \
	graphics_Init.o	\
	pgplotCmd.o \
	vogleCmd.o \
	idaInit.o \
	XidaInit.o

OBJECT_FILES = ${FSOURCE} ${CSOURCE}

SOURCE=\
	Anmap.o

XSOURCE=\
	Xanmap.o

# default target --- this is appropriate to a Solaris system
all : solaris ida Xida

# system dependencies
sunos : libic.a
	ranlib libic.a

solaris : libic.a

# Dependencies for building the object library
libic.a : $(OBJECT_FILES) $(INCLUDE_FILES)
	ar ru libic.a $(OBJECT_FILES)

# dependencies for source files
$(FSOURCE) : $(FINCLUDE)

$(CSOURCE) : $(CINCLUDE)

# Target for cleaning up the source directory
clean :
	rm -f $(OBJECT_FILES)
	cd -f docs ; rm *.log *.dvi *.aux

# Target for building application
ida : libic.a
	f77 -o ida idaInit.o \
	$(IDA_LINK) $(TK_LINK) $(C_LINK)

# Target for building X application
Xida : libic.a
	f77 -o Xida XidaInit.o \
	$(IDA_LINK) $(TK_LINK) $(C_LINK)

# Target for building Static application
Sida : libic.a
	f77 -o Sida idaInit.o -Bstatic \
	$(IDA_LINK) $(STK_LINK) $(C_LINK) \
	$(IDA_LINK) $(STK_LINK) $(C_LINK) \
	-Bdynamic -ldl -Bstatic

# Target for building Static X application
SXida : libic.a
	f77 -o SXida XidaInit.o -Bstatic \
	$(IDA_LINK) $(STK_LINK) $(C_LINK) \
	$(IDA_LINK) $(STK_LINK) $(C_LINK) \
	-Bdynamic -ldl -Bstatic

# target for making distribution
distribution:
	cd .. ; tar cvf anm3d.tar anm3d/Makefile \
		anm3d/*.c anm3d/*f \
		anm3d/*.h anm3d/*.inc \
		anm3d/*.xbm \
		anm3d/tcllib anm3d/util anm3d/docs
	cd .. ; rm -f anm3d.tar.gz ; /usr/local/bin/gzip anm3d.tar

