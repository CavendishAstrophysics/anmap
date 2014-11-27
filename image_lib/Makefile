#! Makefile for the Anmap map-analysis sub-system

FFLAGC=-u 
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\
	/mrao/anmap/include/anmap_sys_pars.inc

OBJECT_FILES=\
	do_add_flux.o           do_mapprint.o           do_shift.o \
	do_add_maps.o           do_mapscl.o             do_shrink.o \
	do_mask_map.o           do_stretch.o \
	do_chi_map.o            do_strip_flux.o \
	do_chr_chblnk.o         do_multiply_maps.o      do_vflip.o \
	do_conv_pos.o           do_noise_map.o          do_zapmap.o \
	do_convolve.o           do_binary_map.o \
	do_divide_maps.o        map_anal_smooth.o 	map_do_io.o \
	do_exp_map.o            map_stretch.o \
	do_gate_map.o           mapanal_sys.o \
	do_hflip.o              multi_add_sys.o \
	do_loc_max.o            do_reproject.o 		do_angular_flux.o \
	do_reproj2.o\
	do_log_map.o            do_ring_flux.o		do_get.o \
	do_scan_map.o		do_expand.o \
	extract_slice.o		do_rotate.o \
	image_addflux.o		image_moments.o		image_addpoln.o \
	image_polcorr.o		image_addring.o		image_gscan.o \
	image_maxmin.o		image_print.o		image_convolve.o \
	image_copy.o		image_shift.o		image_rotate.o \
	image_addang.o \
	utl_err.o		fun_gammp.o		fun_gammq.o \
	fun_gser.o		fun_gcf.o		fun_gammln.o \
	utl_linfit.o		invtan.o		iswap.o \
	iuv_load.o		iuv_convert.o		uv_posconv.o \
	uv_within.o

# system dependencies
sunos : libimg.a
	ranlib ../lib/libimg.a

solaris : libimg.a


# Dependencies for building the object library
libimg.a : $(OBJECT_FILES) $(INCLUDE_FILES)
	ar ru ../lib/libimg.a $(OBJECT_FILES)

# Target for cleaning up the source directory
clean :
	rm -f $(OBJECT_FILES)

