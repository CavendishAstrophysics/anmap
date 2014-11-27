#! Makefile for the Anmap/Graphic library

FFLAGC=-u
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\
	/mrao/anmap/include/plt_basic_defn.inc \
	/mrao/anmap/include/plt_error_defn.inc \
	/mrao/anmap/include/plt_image_defn.inc \
	/mrao/anmap/include/plt_draw_defn.inc \
	/mrao/anmap/include/plt_annot_defn.inc \
	/mrao/anmap/include/plt_grline_defn.inc \
	/mrao/anmap/include/plt_scratch_defn.inc \
	/mrao/anmap/include/plt_graphic_array.inc 

OBJECT_FILES=\
	graphic_sys.o			graphic_open.o \
	graphic_init.o			graphic_end.o \
	graphic_get_line_opt.o		graphic_get_text_opt.o \
	graphic_get_frame_opt.o		graphic_get_grline.o \
	graphic_get_arrow_opt.o		graphic_getpos.o \
	graphic_set_line_opt.o		graphic_set_text_opt.o \
	graphic_set_view_port.o		graphic_set_arrow_opt.o \
	graphic_compound.o \
	graphic_default_line_opt.o	graphic_default_text_opt.o \
	graphic_default_frame_opt.o	graphic_default_grline.o \
	graphic_default_arrow_opt.o \
	graphic_copy_graphic.o		graphic_copy_object.o \
	graphic_copy_line_opt.o		graphic_copy_text_opt.o \
	graphic_copy_frame_opt.o	graphic_copy_axis_opt.o \
	graphic_copy_grline.o 		graphic_copy_arrow_opt.o \
	graphic_show_object.o		graphic_show_arrow_opt.o \
	graphic_show_line_opt.o		graphic_show_text_opt.o \
	graphic_vport.o 		graphic_cursor.o \
	graphic_frame.o			graphic_grline.o \
	graphic_grrange.o		graphic_scale_coord.o \
	graphic_push_coord.o		graphic_pop_coord.o \
	graphic_put_text.o		graphic_enq_text.o \
	graphic_delete_text.o		graphic_open_compound.o \
	graphic_pars_text_opt.o		graphic_pars_line_opt.o \
	graphic_pars_object.o		graphic_pars_graphic.o \
	graphic_pars_axis.o		graphic_pars_frame.o \
	graphic_pars_grline.o		graphic_pars_graph.o \
	graphic_device_select.o \
	ct_enh_col.o	ct_enh_grey.o	ct_enh_inv_grey.o \
	ct_lin_grey.o	ct_standard.o	ct_lin_inv_grey.o \
	pgcolour.o	pgct01.o	pgdrawby.o \
	pgdrawto.o	pgmfplot.o	pgmovby.o \
	pgmovto.o  	pgqcur.o	pgset.o \
	pgsct.o		pgvectors.o	pgisom.o \
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

BLOCK_DATA = \
	plt_annot.o             plt_graph.o             plt_scratch.o \
	plt_coord_stack.o       plt_graphic_array.o \
	plt_draw.o              plt_image.o

# system dependencies
sunos : libgr.a
	ranlib ../lib/libgr.a

solaris : libgr.a


# Dependencies for building the object library
libgr.a : $(OBJECT_FILES) $(BLOCK_DATA) $(INCLUDE_FILES)
	ar ru ../lib/libgr.a $(OBJECT_FILES)

# Target for cleaning up the source directory
clean :
	rm -f $(OBJECT_FILES)

# dependencies
.o : $(INCLUDE_FILES)








