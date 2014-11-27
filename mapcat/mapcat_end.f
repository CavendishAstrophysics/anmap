C
C
*+ mapcat_end

       subroutine mapcat_end(imap,status)
C      ----------------------------------
C
C End the access state to a map catalogue entry
C
C Input:
C     Map catalogue entry
        integer            imap
C Returned:
C     Status
        integer            status
C
C End the access state to the specified catalogue entry. Physical files
C are closed if they are still open.
*-

       integer   map_status(10)

       include 'mapcat_pars.inc'

       if (status.ne.0) return
       call mapcat_enqst(imap,map_status,status)
       if (status.eq.0) then
         if (map_status(ip_open).eq.true) then
           call mapcat_mapclose(imap,status)
         end if
         map_status(ip_access) = access_clear
         map_status(iP_data)   = true
         call mapcat_setst(imap,map_status,status)
       end if
       call mapcat_err(status,'mapcat_end',' ')
       end
