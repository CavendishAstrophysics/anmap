*+ graphic_open_compound

       subroutine graphic_open_compound( iunit, file, s )
C      --------------------------------------------------
C
C Open a compound object and return file pointer
C
C Returned:
C   file pointer
       integer        iunit
C Given:
C   compound file name
       character*(*)  file
C
C Updated:
C   error status
       integer        s
C
C Open the compound graphical object file.  The following directories
C are searched for the object:
C
C a)  Current working directory
C b)  Default GRAPHIC directory    GRAPHIC
C c)  User GRAPHIC directory       USER-GRAPHIC
C
C-
       include '/mrao/include/iolib_constants.inc'

C local variables
       integer     istat, i
       character   f*(iolen_file), d*(iolen_file), name*(iolen_file)

       istat = 0
       call io_namfil( file, name, 0, istat )
       if (istat.ne.0) then
          istat = 0
          call cmd_enqparam( 'GRAPHIC', d, istat )
          call io_makfil( d, file, ' ', f, i )
          call io_namfil( f, name, 0, istat )
          if (istat.ne.0) then
            istat = 0
            call cmd_enqparam( 'USER-GRAPHIC', d, istat )
            call io_makfil( d, file, ' ', f, i )
            call io_namfil( f, name, 0, istat )
         endif
       endif
       if (istat.eq.0) then
         call io_opefil( iunit, name, 'read', 0, s )
       else
         s = istat 
       endif
       call cmd_err( s, 'graphic_open_compound',' ')
       end
