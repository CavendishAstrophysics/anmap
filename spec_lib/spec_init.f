*+ spec_init

       subroutine spec_init( command, definitions, s )
C      -----------------------------------------------
C
C Perform standard initialization for a spectrum analysis program
C
C Given:
C   command name and definitions file
       character*(*)   command, definitions
C Updated:
C   error status
       integer         s
C
C Perform standard initialisation for a spectrum analysis routine.
C
C-
C Include information on standard array sizes etc.
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer     l

       if (s.ne.0) return

C setup for input
       call io_initio
       call cmd_init( s )

C register the spectrum errors file
       call io_setmsg( spec_error_file, s )

C do command line interpretation
       l = chr_lenb(definitions)
       if ( chr_cmatch(definitions(1:lb),'tools') ) then
         call cmd_getiline( command(1:chr_lenb(command)),
     *        define_tools, s )
       elseif ( chr_cmatch(definitions(1:lb),'analysis')) then
         call cmd_getiline( command(1:chr_lenb(command)),
     *        define_analysis, s )
       endif

       end
