# Makefile for the MRAO SPEC library
#

FFLAGC=
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\
	spec_errors.inc spec_data.inc spec_control.inc spec_global.inc \
	spec_header.inc spec_pars.inc

OBJECT_FILES=\
	spec_allocate.o   spec_deallocate.o  spec_get_data.o \
	spec_getcols.o    spec_hd_copy.o     spec_hd_enq.o \
	spec_hd_enqi.o    spec_hd_enqr.o     spec_hd_print.o \
	spec_hd_set.o     spec_put_data.o    spec_get_row.o \
	spi_alloc.o       spi_check_id.o     spi_dealloc.o \
	spi_enq_access.o  spi_err.o          spi_get_header.o \
	spi_put_header.o  spi_read_data.o    spi_write_data.o \
	spec_init.o      spec_end.o

# system dependencies
sunos : libspec.a
	ranlib ../lib/libspec.a

solaris : libspec.a

# Target for building the library
libspec.a : $(OBJECT_FILES) $(INCLUDE_FILES)
	ar ru ../lib/libspec.a $(OBJECT_FILES)

# Target for installing the library
install : libspec.a

# Target for cleaning up the source directory
clean :
	rm -f $(OBJECT_FILES)
