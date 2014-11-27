#! Makefile for the Anmap clean sub-system

FFLAGC=-u 
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\
	/mrao/anmap/include/clean_record.inc \
	/mrao/anmap/include/clean_sys_pars.inc 


OBJECT_FILES=\
	boxmin_map.o            cln_result.o            maxmin_map.o \
	clean_batch_report.o    cln_setdef.o            nearest_pt.o \
	clean_image.o           cln_setmap.o            noise_map.o \
	cln_start.o             qindxr1.o \
	clean_map.o             find_cc.o               restor.o \
	make_ccsl.o             restore_map.o \
	clean_sys.o             make_clbm.o             sort_index.o \
	make_phbm.o             subtrc.o \
	cln_clear.o             make_trbm.o             uv_conv.o \
	cln_display.o           maxmin.o

# system dependencies
sunos : libclean.a
	ranlib ../lib/libclean.a

solaris : libclean.a

# Dependencies for building the object library
libclean.a : $(OBJECT_FILES) $(INCLUDE_FILES)
	ar ru ../lib/libclean.a $(OBJECT_FILES)

# Target for cleaning up the source directory
clean :
	rm -f $(OBJECT_FILES)

