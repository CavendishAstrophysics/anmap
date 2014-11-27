C
C
*+ mapcat_enqdefdir

       subroutine mapcat_enqdefdir(default_directory,status)
C      -----------------------------------------------------
C
C Enquire default map directory
C
C Returned:
C    default map directory
       character*(*)    default_directory
C    status word
       integer          status
C
C The default map directory is returned.  If there has been no previous
C call to mapcat_setdefdir then the default directory for the logged on user
C is returned.
C
*-
       include 'mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer    len_dir

       if (status.ne.0) return
       len_dir = chr_lenb(current_def_dir)
       if (len_dir.gt.0) then
         default_directory = current_def_dir(1:len_dir)
       else
         call io_namfil(default_def_dir,default_directory,0,status)
       end if
       call mapcat_err(status,'mapcat_enqdefdir',' ')

       end

C
C
*+ mapcat_enqdefdir1

       subroutine mapcat_enqdefdir1(default_directory,status)
C      -----------------------------------------------------
C
C Enquire default map directory
C
C Returned:
C    default map directory
       character*128    default_directory
C    status word
       integer          status
       integer          chr_lenb
       call mapcat_enqdefdir(default_directory,status)
       default_directory =
     *   default_directory(1:chr_lenb(default_directory))//char(0)
       end
