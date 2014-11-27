# Makefile for the MRAO Spectrum package
#

FFLAGC=-xl -u
FCOMPL=f77

INCLUDE_FILES=\
	../spec_global.inc ../spec_errors.inc

SOURCE = \
	sp_combine sp_reinterpolate sp_header sp_gethd \
	sp_integrate sp_log sp_make_histogram sp_rebin sp_scale \
	sp_smooth sp_print sp_baseline sp_register sp_flip \
	sp_fitlinear sp_fitgauss sp_noise sp_pass sp_statistics \
	sp_clean sp_convolve sp_zap sp_rl sp_wrap sp_anneal \
	sp_filter sp_shrink

# target for building all the tools
all : sp_display sp_fft $(SOURCE)

# target for building sp_display
sp_display : sp_display.f
	f77 -xl -u sp_display.f -o sp_display `../bin/speclink` `pgplotlink`
	strip $@
	rm -f ../bin/$@
	mv $@ ../bin/$@

# target for building sp_fft
sp_fft : sp_fft.f
	f77 -u sp_fft.f -o sp_fft -L/mrao/lib -lfft `../bin/speclink`
	strip $@
	rm -f ../bin/$@
	mv $@ ../bin/$@

# target for building other routines
$(SOURCE) : 
	f77 -xl -u $@.f -o $@ `../bin/speclink` -lnag
	strip $@
	rm -f ../bin/$@
	mv $@ ../bin/$@

# target for cleaning up after a make
clean :
	rm -f *.o
	rm -f $(SOURCE) sp_display sp_fft
	rm -f *.trace

# target for cleaning up after a make
cleanAll : clean
	cd ../bin ; rm -f $(SOURCE) sp_display sp_fft
