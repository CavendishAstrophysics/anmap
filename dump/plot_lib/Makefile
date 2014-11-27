#! Makefile for the Anmap Plot_sys sub-system

FFLAGC=-u 
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) -Nq500 $<

INCLUDE_FILES=\
	/mrao/anmap/include/plt_basic_defn.inc \
	/mrao/anmap/include/plt_error_defn.inc \
	/mrao/anmap/include/plt_graphic_array.inc \
	/mrao/anmap/include/plt_image_defn.inc 

OBJECT_FILES=\
	char_io_onoff.o         char_setlev.o \
	plot_doplot.o           plot_shcnt.o \
	plot_ctclear.o          plot_shopt.o		plot_crosses.o \
	plot_init_opt.o		plot_init_plot.o	plot_init_setup.o \
	plot_getuv.o		plot_getuv2.o \
	plot_ctdefine.o         plot_frinit.o           plot_shsymb.o \
	plot_ctlin.o            plot_frset.o            plot_shvec.o \
	plot_ctlog.o            plot_get.o              plot_style.o \
	plot_ctreset.o          plot_getpip.o           plot_sys.o \
	plot_cursor_read.o      plot_tvmod.o 		plot_cursor_get.o \
	plot_display.o          plot_setmap.o           plot_vectors.o \
	plot_getpos2.o		plot_getcol2.o		plot_getrow2.o \
	plot_setpos_parms.o	plot_docont.o		plot_dotext.o \
	plot_dogrey.o		plot_dovecs.o		plot_dosymb.o \
	plot_doframe.o		plot_dopips.o		plot_dogrid.o \
	annotate.o		TV_load.o

# system dependencies
sunos : libplot.a
	ranlib ../lib/libplot.a

solaris : libplot.a

# Dependencies for building the object library
libplot.a : $(OBJECT_FILES) $(INCLUDE_FILES)
	ar ru ../lib/libplot.a $(OBJECT_FILES)

# Target for cleaning up the source directory
clean :
	rm -f $(OBJECT_FILES)

