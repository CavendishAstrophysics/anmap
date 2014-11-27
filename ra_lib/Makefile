#! Makefile for the Anmap Radio Astrronomy Specific routines

FFLAGC=-u 
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\
	../include/synch_sys_pars.inc \
	../include/synch_errors.inc \
	../include/synch_defn.inc \
	../include/synch_bfield.inc 


OBJECT_FILES=\
	synch_2p_aatofb.o 	synch_bfun8.o 		synch_dispatch.o \
	synch_2p_make.o 	synch_enqlim.o          synch_makesp.o \
	synch_2p_val.o          synch_enqsp.o           synch_setsp.o \
	synch_age.o             synch_enqwn.o \
	synch_bfun.o            synch_fitmode.o		synch_fit.o \
	do_2p_lookup.o		do_age_b_const.o	do_fit_data.o \
	do_alpha_to_age.o 	do_fit_maps.o \
	bfield_set.o 		do_synch_sp.o		do_define_sp.o \
	ffun.o 			b1fun.o \
	chebr.o		 	fang.o			fang2.o

# system dependencies
sunos : libra.a
	ranlib ../lib/libra.a

solaris : libra.a

# Dependencies for building the object library
libra.a : $(OBJECT_FILES) $(INCLUDE_FILES)
	ar ru ../lib/libra.a $(OBJECT_FILES)

# Target for cleaning up the source directory
clean :
	rm -f $(OBJECT_FILES)







