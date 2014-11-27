C
C
*+ mapcat_setrt

       subroutine mapcat_setrt(imap,filename,minirt,status)
C      ----------------------------------------------------
C
C Set the filename and mini redtape for catalogue entry IMAP
C
C Input:
C    Map catalogue entry
       integer             imap
C    Filename for entry IMAP
       character*(*)       filename
C    Mini redtape for entry IMAP
       integer             minirt(*)
C Returned:
C    Status
       integer             status
*-
       include 'mapcat_cat.inc'
       include '/mrao/include/iolib_constants.inc'

       integer   i

       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       do i = 1,8
         current_minirt(i) = minirt(i)
       end do
       current_filename = filename
       if (operating_system.ne.'UNIX') then
         call chr_chucas(current_filename)
       end if
       call mapcat_write(imap,status)
       call mapcat_err(status,'mapcat_setrt',' ')
       end
