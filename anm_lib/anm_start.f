*+ anm_start

       subroutine anm_start( interp, nm, msize, nb, bsize, status )
C      ------------------------------------------------------------
C
C Perform standard initialization for stand-alone program
C
C Given:
C  pointer to Tcl interpreter
       integer      interp(*)
C  number of maps in work-space and size of each map
       integer      nm, msize
C  number of row buffers in work-space and size of each buffer
       integer      nb, bsize
C Updated:
C  error status
       integer      status
C
C This routine provides a standard initialization for programs
C using ANMAP graphics and/or map-catalogue.
C
C If the stack-system is to be used for work space the program
C must:
C  A define an array (conventionally called map_array) of size
C    nm*msize + nb*bsize
C  then call this routine with nm, msize, nb, bsize set.  If the
C  stack system is not to be used to allocate space then call
C  this routine with all arguments set to zero.  Note that even
C  if the stack-system is not to be used this routine does
C  perform the initialization for using the map catalogue.
C
C Examples:
C
C   To initialize a program which does not use command-line
C   interpretation or use the stack then call:
C
C       call anm_start( 0,0,0.0,0,0.0,status )
C
C   To initialize a command-driven program using the stack the
C   following is a standard code fragment.
C
C       integer         status
C       integer         nm, nb
C       integer         msize, bsize
C       paramater      (nm = 8, nb=4, msize=256*256, bsize=1024)
C
C       call anm_start( 101, nm, msize, nb, bsize, status )
C
C Note that in both cases a call to anm_end should be made before
C the program exits.
C
C P. Alexander, MRAO, 09/04/92
*-
       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'

       character*80   value

       if (status.ne.0) return

C I/O libraries
       call io_initio

C Stack
       if (nm.gt.0) then
         call stack_init(nm,msize,nb,bsize,status)
       end if

C error handling
       call io_setmsg(anmap_error_file,status)
       call io_setmsg(synch_error_file,status)
       call io_setmsg(stack_error_file,status)
       call io_setmsg(mcat_error_file,status)
       call io_setmsg(graphic_error_file,status)
       call io_setmsg(util_error_file,status)

C map catalogue
       call mapcat_open(status)
       call mapcat_setrm( .true. )
       call iocmd_err(status,'anm_start',
     *                'Initialisation Failed or Incomplete')
       call io_enqvar('anm_ReportMode',value,status)
       if (chr_cmatch(value,'0')) then
         call mapcat_setrm( .false. )
       else
         call mapcat_setrm( .true. )
       endif
       end






