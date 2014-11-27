C
C
*+ mapcat_mapclose

       subroutine mapcat_mapclose(imap,status)
C      ---------------------------------------
C
C Close the physical file associated with map IMAP
C
C Input:
C    Map catalogue entry
       integer            imap
C Returned
C    Status
       integer            status
*-
C status word
       integer   map_status(10)
C
       include 'mapcat_pars.inc'

C check status on entry
       if (status.ne.0) return

C check IMAP for validity
       call mapcat_chk(imap,'NONE',status)
       if (status.ne.0) goto 999

C enquire map status word
       call mapcat_enqst(imap,map_status,status)

C close physical file if open and update status word
       if (map_status(ip_open).eq.true) then
         close (map_status(ip_unit))
       end if
       if  (status.eq.0) then
         map_status(ip_open) = false
         map_status(ip_unit) = 0
       end if
       call mapcat_setst(imap,map_status,status)

C report error
999    call mapcat_err(status,'mapcat_mapclose',' ')

       end
