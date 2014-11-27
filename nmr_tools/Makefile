# Makefile for the MRAO NMR tools
#

FFLAGC=-xl -u
FCOMPL=f77

INCLUDE_FILES=\
	../spec_global.inc ../spec_errors.inc

SPEC_SOURCE = \
	nmr_PSD		nmr_fit2T1	nmr_fitlinear \
	nmr_fitrelax	nmr_leftshift   nmr_a2sp \
	nmr_sp_fft	nmr_d2sp

IMAGE_SOURCE = \
	nmr_readImage	nmr_readSer	nmr_readAmpPhase \
	nmr_kermitread	nmr_read


# target for building all the tools
all : $(SPEC_SOURCE) $(IMAGE_SOURCE)

# target for building spec routines
$(SPEC_SOURCE) : 
	f77 -xl -u $@.f -o $@ `../bin/speclink` -lfft -lnag
	strip $@
	rm -f ../bin/$@
	mv $@ ../bin/$@

# target for building image routines
$(IMAGE_SOURCE) :
	f77 -u $@.f -o $@ `../bin/anmlink` -lfft -lnag 
	strip $@
	rm -f ../bin/$@
	mv $@ ../bin/$@

# target for cleaning up after a make
clean :
	rm -f *.o
	rm -f $(SPEC_SOURCE) $(IMAGE_SOURCE)
	rm -f *.trace

# target for cleaning up after a make
cleanAll : clean
	cd ../bin ; rm -f $(SPEC_SOURCE) $(IMAGE_SOURCE)
