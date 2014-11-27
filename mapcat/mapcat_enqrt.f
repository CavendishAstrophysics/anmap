C
C
*+ mapcat_enqrt

       subroutine mapcat_enqrt(imap,filename,minirt,status)
C      ----------------------------------------------------
C
C Enquire the map filename and mini redtape for catalogue entry IMAP
C
C Input:
C    Map catalogue entry
       integer             imap
C Returned:
C    Filename for entry IMAP
       character*(*)       filename
C    Mini redtape for entry IMAP
       integer             minirt(*)
C    Status
       integer             status
*-
       integer   i
       include 'mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       call mapcat_read(imap,status)
       do i = 1,8
         minirt(i) = current_minirt(i)
       end do
       filename = current_filename
       call mapcat_err(status,'mapcat_enqrt',' ')
       end
