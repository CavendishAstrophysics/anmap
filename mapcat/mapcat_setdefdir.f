C
C
*+ mapcat_setdefdir

       subroutine mapcat_setdefdir(default_directory,status)
C      -----------------------------------------------------
C
C Set default map directory
C
C Input:
C    default map directory
       character*(*)    default_directory
C Returned:
C    status word
       integer          status
C
C The default map directory is set.  A check is made that the
C directory exists.
C
C Warning this command will affect the destination of PAGED temporary
C maps.  It is essential that this command is used with great care.
C
*-
       include 'mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

       integer    len_dir

       if (status.ne.0) return
       len_dir = chr_lenb(default_directory)
       if (operating_system.eq.'SINTRAN') then
         call io_namusr(default_directory(1:len_dir),
     *                  current_def_dir,0,status)
       else
         call io_namfil(default_directory(1:len_dir),
     *                  current_def_dir,0,status)
       endif
       if (status.ne.0) then
         current_def_dir = ' '
       end if
       call mapcat_err(status,'mapcat_setdefdir',' ')

       end

*+ mapcat_setdefdir1

       subroutine mapcat_setdefdir1(default_directory,status)
C      -----------------------------------------------------
C
C Set default map directory
C
C Input:
C    default map directory
       character*128    default_directory
C Returned:
C    status word
       integer          status

       call mapcat_setdefdir( default_directory,status )
       end
