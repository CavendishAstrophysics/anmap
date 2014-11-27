C Synchrotron-Program System Parameters
C -------------------------------------
C
C The following file contains system parameters which need to be
C defined for SYNCHROTRON and are done so via PARAMETER statements.
C These are not variables and cannot be changed at run time.
C
C Last updated 17/02/92   PA, MRAO, Cambridge.
*-
C
C 0) Define Size of the work arrays used by MAPCAT
C ------------------------------------------------
C
C Size and number of maps
       INTEGER          MAP_SIZE
       PARAMETER       (MAP_SIZE = 256*256)
       INTEGER          NUMBER_MAPS
       PARAMETER       (NUMBER_MAPS = 16)
C Size and number of row buffers
       INTEGER          BUFFER_SIZE
       PARAMETER       (BUFFER_SIZE = 1024)
       INTEGER          NUMBER_BUFFERS
       PARAMETER       (NUMBER_BUFFERS = 8)
C
C
C 1) System Definition
C --------------------
C
C Version number and date
       CHARACTER      SYS_VERSION*7, SYS_DATE*18
       PARAMETER     (SYS_VERSION = '3.0(a)' )
       PARAMETER     (SYS_DATE    = '8th April 1992')
C
C
C Program name
       CHARACTER      PROGRAM_NAME*12
       PARAMETER     (PROGRAM_NAME = 'SYNCHROTRON')
C
C
C Scope of the program
       INTEGER        PROGRAM_SCOPE
       PARAMETER     (PROGRAM_SCOPE = 5)
C
C
C Initial command file and system initialisation file
       CHARACTER      INITIAL_COMMANDS*21,  SYSTEM_INIT*31
       PARAMETER     (INITIAL_COMMANDS='~/mrao/synch_init.com')
       PARAMETER     (SYSTEM_INIT ='/mrao/anmap/util/synch_init.com')
C
C 2) Prompts
C ----------
C
       CHARACTER*13   PROMPT_MAIN
       PARAMETER     (PROMPT_MAIN  = 'Synchrotron> ')
C
C 3) Help and News Files
C ----------------------
C
       character*32   help_file
       parameter     (help_file =
     *                '/mrao/help/anmap/synch_anal.help')
       character*20   system_help_file
       parameter     (system_help_file =
     *                '/mrao/help/help.help' )
       character*20   system_news_file
       parameter     (system_news_file =
     *                '/mrao/help/news.help' )
c
c 4) miscellaneous files
c ----------------------
c
c general error file
       character*37   mapcat_error_file
       parameter     (mapcat_error_file=
     *                '/mrao/anmap/include/mapcat_errors.inc')
c
c synchrotron-spectra file
       character      synch_sys_file*39
       parameter     (synch_sys_file=
     *                '/mrao/anmap_v7.5/etc/synch_spectra.data')
c
c
c default for file name to store results
       character      default_bfield_results*18
       parameter     (default_bfield_results =
     *                'bfield_results.dat')
c
