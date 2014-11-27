C
C
*+ mapcat_enqst

       subroutine mapcat_enqst(imap,map_status,status)
C      -----------------------------------------------
C
C Enquire the map status word for catalogue entry IMAP
C
C Input:
C    Map catalogue entry
       integer             imap
C Returned:
C    Status word
       integer             map_status(*)
C    Status
       integer             status
C
C Return the map status word for the specified catalogue entry
*-
       integer   i
       include 'mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       call mapcat_read(imap,status)
       do i = 1,length_map_status
         map_status(i) = current_map_status(i)
       end do
       call mapcat_err(status,'mapcat_enqst',' ')
       end
