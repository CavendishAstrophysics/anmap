# Makefile for ANMAP and related programs
#
# Copyright Paul Alexander, MRAO Cambridge
#
# Makefile for tools of Version 7.5e of ANMAP
#
# last updated 16/10/93

FFLAGC=-u
FCOMPL=f77


.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILE=\
	/mrao/anmap/include/anmap_sys_pars.inc \
        /mrao/anmap/include/error_file_full.inc

SOURCE=\
	multi_add	multi_fit	Aphi_RealImag	ricean_correction\
	multi_slicefit	image_fft	fractal_calc\
	cluster_make	cluster_data	cluster_print	clean_prog\
	cluster_image	segment_flux    extract_row	extract_column\
	in_data		out_data

# object for building all tools
all : $(SOURCE) map2fits

# default object for basic tools
$(SOURCE) :
	f77 -u $@.f -o $@ `../bin/anmlink` -lfft -lnag
	strip $@
	rm -f ../bin/$@
	mv $@ ../bin/$@

map2fits :
	f77 -u map2fits.f -o map2fits -L/mrao/lib `/mrao/bin/fitslink`
	strip $@
	rm -f ../bin/$@
	mv $@ ../bin/$@

# force full recompilation
touch :
	touch *.f

# target for cleaning up after a make
clean :
	rm -f *.o
	rm -f $(SOURCE)
	rm -f *.trace

# target for cleaning up after a make
cleanAll : clean
	cd ../bin ; rm -f $(SOURCE)

# dependencies
$(SOURCE) : $(INCLUDE_FILES)
