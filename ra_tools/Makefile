# Makefile for the MRAO RA tools
#

FFLAGC=-u 
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

SPEC_SOURCE = \
	ra_dpm 

IMAGE_SOURCE = \
	galaxy_read	ra_dispatch


# target to install any utilities
install : all

# target for building all the tools
all : $(SPEC_SOURCE) $(IMAGE_SOURCE) ra_dispatch

# target for building other routines
$(SPEC_SOURCE) :
	f77 -xl -u $@.f -o $@ `../bin/speclink` -lnag
	strip $@
	rm -f ../bin/$@
	mv $@ ../bin/$@

$(IMAGE_SOURCE) : 
	f77 -xl -u $@.f -o $@ `../bin/anmlink` -lnag
	strip $@
	rm -f ../bin/$@
	mv $@ ../bin/$@

# target for cleaning up after a make
clean :
	rm -f *.o
	rm -f $(SPEC_SOURCE) $(IMAGE_SOURCE) ra_dispatch
	rm -f *.trace

# target for cleaning up after a make
cleanAll : clean
	cd ../bin ; rm -f $(SPEC_SOURCE) $(IMAGE_SOURCE) ra_dispatch
