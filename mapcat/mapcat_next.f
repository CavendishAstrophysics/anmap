C
C
*+ mapcat_next

       subroutine mapcat_next(next_map,status)
C      ---------------------------------------
C
C Return the next free map entry
C
C Returned:
C   Next free map entry
       integer          next_map
C   Status word
       integer          status
C
C The next available catalogue entry is found. If the catalogue is
C full then the status value is set to ILL_NXTMAP.
*-
       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'

       integer      imap

       if (status.ne.0) return
       next_map = -1
       do imap = 1,max_cat_entries
         call mapcat_read(imap,status)
         if (status.eq.0) then
           if (current_map_status(ip_data).eq.false) then
             if (current_map_status(ip_access).eq.access_clear) then
                next_map = imap
                goto 100
             end if
           end if
         end if
       end do
100    continue
       if (next_map.eq.-1) then
         status = ill_nxtmap
       end if
       call mapcat_err(status,'mapcat_next','Unable to find entry')
       end
