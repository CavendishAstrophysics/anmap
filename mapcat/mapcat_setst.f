C
C
*+ mapcat_setst

       subroutine mapcat_setst(imap,map_status,status)
C      -----------------------------------------------
C
C Set the map status word for catalogue entry IMAP
C
C Input:
C    Map catalogue entry
       integer             imap
C    Status word
       integer             map_status(*)
C Returned:
C    Status
       integer             status
C
C Sets the map status word for the specified catalogue entry
*-
       integer   i
       include 'mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       do i = 1,length_map_status
         current_map_status(i) = map_status(i)
       end do
       call mapcat_write(imap,status)
       call mapcat_err(status,'mapcat_setst',' ')
       end
