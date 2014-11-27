C ANMAP System Parameters
C -----------------------
C
C The following file contains system parameters which need to be
C defined for ANMAP and are done so via PARAMETER statements -- these
C are not variables and cannot be changed at run time.
C
C SUNOS Version
C Last updated  15/02/93   PA, MRAO, Cambridge.
*-
C
C Define Size of the ANMAP work arrays used by MAPCAT
C ---------------------------------------------------
C
C Size and number of maps
C  standard values 256*256 ; 20
       INTEGER          MAP_SIZE, MAP_SIDE_SIZE
       PARAMETER       (MAP_SIZE = 256*256, MAP_SIDE_SIZE=256)
*       PARAMETER       (MAP_SIZE = 64*64, MAP_SIDE_SIZE=64)
       INTEGER          NUMBER_MAPS
       PARAMETER       (NUMBER_MAPS = 20)
*       PARAMETER       (NUMBER_MAPS = 10)
C Size and number of row buffers
       INTEGER          BUFFER_SIZE
*       PARAMETER       (BUFFER_SIZE = 128)
       PARAMETER       (BUFFER_SIZE = 1024)
       INTEGER          NUMBER_BUFFERS
       PARAMETER       (NUMBER_BUFFERS = 8)

C
C System Definition
C -----------------
C
C Version number and date
       CHARACTER      SYS_VERSION*7, SYS_DATE*11
       PARAMETER     (SYS_VERSION = '7.4(h)' )
       PARAMETER     (SYS_DATE    = 'May 26 1993')
C
C Queue definition names
       CHARACTER      ANMAP_QUEUE_NAME*5
       PARAMETER     (ANMAP_QUEUE_NAME = 'ANMAP')
C
       CHARACTER      CLEAN_QUEUE_NAME*5
       PARAMETER     (CLEAN_QUEUE_NAME = 'CLEAN')
C
C Initial command file and system initialisation file
       CHARACTER      SYSTEM_INIT*32
       PARAMETER     (SYSTEM_INIT='/mrao/anmap/procs/init_anmap.com')
C
C Synchrotron-spectra file
       CHARACTER      SYNCH_SYS_FILE*34
       PARAMETER     (SYNCH_SYS_FILE=
     *                '/mrao/anmap/etc/synch_spectra.data')
C
C
C
C 3) Help Files
C -------------
C
       CHARACTER*27   ANMAP_HELPFILE
       PARAMETER     (ANMAP_HELPFILE =
     *               '/mrao/help/anmap/anmap.help' )
       CHARACTER*32   VECTORS_HELPFILE
       PARAMETER     (VECTORS_HELPFILE   =
     *               '/mrao/help/anmap/plot_vec.help')
       CHARACTER*32   SYMBOLS_HELPFILE
       PARAMETER     (SYMBOLS_HELPFILE   =
     *               '/mrao/help/anmap/plot_symb.help')
       CHARACTER*32   ADDMAPS_HELPFILE
       PARAMETER     (ADDMAPS_HELPFILE   =
     *               '/mrao/help/anmap/multi_add.help')
       CHARACTER*32   MULTIFIT_HELPFILE
       PARAMETER     (MULTIFIT_HELPFILE   =
     *               '/mrao/help/anmap/multi_fit.help')
       CHARACTER*32   IMAGEFFT_HELPFILE
       PARAMETER     (IMAGEFFT_HELPFILE   =
     *               '/mrao/help/anmap/image_fft.help')
       CHARACTER*33   MAKEIMAGE_HELPFILE
       PARAMETER     (MAKEIMAGE_HELPFILE   =
     *               '/mrao/help/anmap/make_image.help')
       CHARACTER*33   EDITIMAGE_HELPFILE
       PARAMETER     (EDITIMAGE_HELPFILE   =
     *               '/mrao/help/anmap/edit_image.help')
       CHARACTER*30   ANNOTATE_HELPFILE
       PARAMETER     (ANNOTATE_HELPFILE   =
     *               '/mrao/help/anmap/annotate.help')
C
C
C Miscellaneous Files
C -------------------
C
C News file
       CHARACTER      NEWS_FILE*28
       PARAMETER     (NEWS_FILE = '/mrao/help/anmap/anmap.news')
C
C Dump files for plot information
       CHARACTER*22   PLOT_DUMP_FILE
       PARAMETER     (PLOT_DUMP_FILE = '~/mrao/plot_dump.data')
       CHARACTER*24   PGPLOT_DUMP_FILE
       PARAMETER     (PGPLOT_DUMP_FILE = '~/mrao/pgplot_dump.data')
C
C Save file for drawing commands
       CHARACTER*25   DRAW_SAVE_FILE
       PARAMETER     (DRAW_SAVE_FILE='~/mrao/draw_commands.com')
C
C General results file
       CHARACTER*24   GENERAL_RESULTS_FILE
       PARAMETER     (GENERAL_RESULTS_FILE =
     *                  '~/mrao/anmap_results.dat')
C
C General data file
       CHARACTER*21   GENERAL_DATA_FILE
       PARAMETER     (GENERAL_DATA_FILE =
     *                  '~/mrao/anmap_data.dat')
C
C Default results file
       CHARACTER*24   DEFAULT_RESULTS_FILE
       PARAMETER     (DEFAULT_RESULTS_FILE =
     *                  '~/mrao/save_results.dat')
C
C General error file
       CHARACTER*39   ANMAP_ERROR_FILE
       PARAMETER     (ANMAP_ERROR_FILE =
     *                  '/mrao/anmap/include/error_file_full.inc')
C
C Synchrotron system error file
       CHARACTER*36   SYNCH_ERROR_FILE
       PARAMETER     (SYNCH_ERROR_FILE =
     *                  '/mrao/anmap/include/synch_errors.inc')
C
C Plot/graphics errors
       CHARACTER*38   GRAPHIC_ERROR_FILE
       PARAMETER     (GRAPHIC_ERROR_FILE =
     *                  '/mrao/anmap/include/plt_error_defn.inc')
C
C Utilities errors
       CHARACTER*40   UTIL_ERROR_FILE
       PARAMETER     (UTIL_ERROR_FILE =
     *                  '/mrao/anmap/include/utilities_errors.inc')
C
C Map-catalogue/STACK error files
       CHARACTER*37   MCAT_ERROR_FILE
       PARAMETER     (MCAT_ERROR_FILE =
     *                  '/mrao/anmap/include/mapcat_errors.inc')
C
       CHARACTER*40   STACK_ERROR_FILE
       PARAMETER     (STACK_ERROR_FILE =
     *                  '/mrao/anmap/include/map_stack_errors.inc')
C
C log file
       CHARACTER*25   LOG_FILE_DEFAULT
       PARAMETER     (LOG_FILE_DEFAULT = '~/mrao/analysis_log.list')
C
C Root directory where maps are located
       CHARACTER*8    ROOT_MAPS_DIRECTORY
       PARAMETER     (ROOT_MAPS_DIRECTORY = '~/images')
C
C Default file name for crosses file
       CHARACTER*26   DEFAULT_CROSSES_FILE
       PARAMETER     (DEFAULT_CROSSES_FILE =
     *                  '~/mrao/crosses_file.cross')
C
C Default file name for metafile
       CHARACTER*25   DEFAULT_METAFILE
       PARAMETER     (DEFAULT_METAFILE = '~/mrao/pgplot.gmf')
C
C
C System dependent strings
C ------------------------
C
C add maps initiation command
       CHARACTER*32   ADDMAPS_COMMAND
       PARAMETER     (ADDMAPS_COMMAND =
     *                '/mrao/bin/start_anmap multi_add ')
C multi-fit command
       CHARACTER*32   MULTIFIT_COMMAND
       PARAMETER     (MULTIFIT_COMMAND =
     *                '/mrao/bin/start_anmap multi_fit ')
C synchrotron sub-porgram command
       CHARACTER*30   SYNCH_COMMAND
       PARAMETER     (SYNCH_COMMAND =
     *                '/mrao/bin/start_anmap synch ')
C batch sub-system command
       CHARACTER*37   BATCH_COMMAND
       PARAMETER     (BATCH_COMMAND =
     *                '/mrao/bin/start_anmap batch_system ')
C suggest sub-system
       CHARACTER*32   SUGGEST_COMMAND
       PARAMETER     (SUGGEST_COMMAND =
     *                '/mrao/bin/start_anmap suggest ')
C image fft initiation command
       CHARACTER*32   IMAGEFFT_COMMAND
       PARAMETER     (IMAGEFFT_COMMAND =
     *                '/mrao/bin/start_anmap image_fft ')

C Command strings
C ---------------

C command to execute on setting new map in maps-analysis
       character*18   plotsys_setmap_command
       parameter     (plotsys_setmap_command = 
     *                  'new-map-initialise' )

C command to execute on setting new UV-range in maps-analysis
       character*16   plotsys_setuv_command
       parameter     (plotsys_setuv_command = 
     *                  'setuv-initialise' )




