**
* Map/Image Catalogue Include File
* ================================
C
C
C Define standard parameters
C  Maximum number of output maps
       integer     max_out_maps
       parameter  (max_out_maps = 10)
       integer     max_scr_maps
       parameter  (max_scr_maps = 10)
C  Maximum number of catalogue entries
       integer     max_cat_entries
       parameter  (max_cat_entries = 256)
C  Catalogue file name
       character   default_cat_file*18, default_def_dir*8
       parameter  (default_cat_file = 'map-catalogue.mctg')
       parameter  (default_def_dir = '~/images')
       character   cat_dir_file*20
       parameter  (cat_dir_file =     'map-directories.mctg')
       character   cat_file*80
       character   list_cat_file(10)*80
       integer     current_cat_file, defined_cat_file(10)
C
C Define variables used in common
C  Default map
       integer     default_map
C  Catalogue file unit number
       integer     cat_unit
C  Maximum allowed map area for a read_area allocation
       real*4      area_max_read

C  Number of used buffer records
       integer     num_buffers
C
C Place local variables in common
       common /mapcat_images/ cat_unit, default_map,
     *                        num_buffers, area_max_read

       common /mapcat_file_char/ cat_file, list_cat_file
       common /mapcat_file_cont/ current_cat_file,
     *                           defined_cat_file
C
C
C Save current default directory
       character*80             current_def_dir
       common /mapcat_default/  current_def_dir

C
C Save options for reporting, checking and prompting
       logical                    current_report, current_check,
     *                            current_prompt
       common /local_stack_lib_1/ current_report, current_check,
     *                            current_prompt

C
C
C Define current record for the catalogue of the map/stack system
       include '/mrao/anmap/include/mapcat_recdefn.inc'
       common /mapcat_record/  current_record
C
C Define buffers for additional records
       integer     buffer_records(cat_blocksize/4,3)
       common /mapcat_buffers/ buffer_records
C
C Include parameters
       include '/mrao/anmap/include/mapcat_pars.inc'
C

