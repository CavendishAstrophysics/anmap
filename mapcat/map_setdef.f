C
C
*+ map_setdef

       subroutine map_setdef(imap,status)
C      ----------------------------------
C
C Set the default map
C
C Given:
C   Map entry to become the default map
       integer         imap
C Returned:
C   Status word
       integer         status
C
C Define the default map by catalogue entry number.  IMAP is checked
C for a valid map to use as the default map.
*-
       include 'mapcat_cat.inc'
C
       if (status.ne.0) return
C check validity of IMAP
       call mapcat_chk(imap,'NONE',status)
       if (status.eq.0) then
         default_map=imap
       end if
       call mapcat_err(status,'map_setdef','Default-Map not reset')
       end
