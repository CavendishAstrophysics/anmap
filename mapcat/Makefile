# Makefile for the MRAO/ANMAP MAPCAT library
#
#   last updated 2/3/92 P. Alexander
#   updated 31/7/98 - added map_alloc_out_s.o - SEGH
#

FFLAGC=-xl
FCOMPL=f77

.f.o:
	$(FCOMPL) -c -u $(FFLAGC) $<

INCLUDE_FILES=\
	/mrao/include/iolib_common.inc /mrao/include/iolib_constants.inc\
	/mrao/include/iolib_errors.inc\
	/mrao/include/iolib_functions.inc /mrao/include/iolib_devices.inc\
	map_stack_errors.inc mapcat_errors.inc\
	mapcat_recdefn.inc mapcat_cat.inc mapcat_pars.inc mapcat_stack.inc\
	map_stack_errors.inc mapcat_errors.inc

OBJECT_FILES=\
	ic2_alloc_area.o	ic2_alloc_in.o		ic2_alloc_new.o\
	ic2_alloc_out.o		ic2_alloc_scr.o		ic2_alloc_toout.o\
	ic2_end_alloc.o		ic2_getmap.o\
	ic2_head_load.o		ic2_enq_minirt.o	ic2_enq_null.o\
	ic2_chckuv.o		ic2_iuvmap.o		ic2_ruvmax.o\
	ic2_ruvval.o		ic2_iuvval.o		ic2_ruvduv.o\
	ic2_scnmap.o		ic2_scnsrc2.o\
	map_alloc_area.o	mapcat_enqrt.o		redt_comp.o \
	map_alloc_in.o		mapcat_enqsr.o		redt_dump.o \
	map_alloc_new.o		mapcat_enqst.o		redt_enqcurr.o \
	map_alloc_out_s.o \
	map_alloc_out.o		mapcat_enqsz.o		redt_load.o \
	map_alloc_scr.o		mapcat_err.o		redt_setfile.o \
	map_alloc_toout.o	mapcat_exist.o		redt_setrt.o \
	map_end_alloc.o		mapcat_exlist.o		redt_setsr.o \
	map_enqdef.o		mapcat_file_manager.o	redt_setxrdt.o \
	map_getmap.o		mapcat_fndmap.o		redt_update.o \
	map_row_read.o		mapcat_get.o		stack_access.o \
	map_row_write.o		mapcat_getmap.o		stack_chk.o \
	map_setdef.o		mapcat_init.o		stack_clracc.o \
	mapcat_acc.o		mapcat_io_nxtfil.o	stack_display.o \
	mapcat_addtocat.o	mapcat_list.o		stack_doset.o \
	mapcat_chk.o		mapcat_mapclose.o	stack_dpredt.o \
	mapcat_close.o		mapcat_mapopen.o	stack_edit.o \
	mapcat_delete.o		mapcat_next.o		stack_enqdat.o \
	mapcat_deltemp.o	mapcat_open.o		stack_enqmap.o \
	mapcat_read.o		stack_enqmnx.o		stack_display1.o \
	mapcat_remove.o		mapcat_mapenq.o		stack_enqmode.o \
	mapcat_setarea.o	stack_enqpnt.o \
	mapcat_setch.o		stack_fndspc.o \
	mapcat_disopts.o	mapcat_setdefdir.o	stack_init.o \
	mapcat_display.o	mapcat_setrm.o		stack_io_setacc.o \
	mapcat_edit.o		mapcat_setrt.o		stack_ldredt.o \
	mapcat_end.o		mapcat_setsr.o		stack_remove.o \
	mapcat_enqch.o		mapcat_setst.o		stack_setdat.o \
	mapcat_enqdefdir.o	mapcat_sys.o		stack_setmnx.o \
	mapcat_enqrec.o		mapcat_verify.o \
	mapcat_enqrm.o		mapcat_write.o \
	mc_enqhdc.o

# system dependencies
sunos : libicl.a
	ranlib ../lib/libicl.a

solaris : libicl.a

# Dependencies for building the object library
libicl.a : $(OBJECT_FILES) 
	ar ru ../lib/libicl.a $(OBJECT_FILES)

# Target for cleaning up the source directory
clean :
	rm -f $(OBJECT_FILES)


