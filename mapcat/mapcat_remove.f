C
C
*+ mapcat_remove

       subroutine mapcat_remove(imap,check_status,status)
C      --------------------------------------------------
C
C Remove an entry from the map catalogue
C Input:
C    Map entry
       integer      imap
C    Check status of catalogue entry before removing it
       logical      check_status
C Returned:
C    Status
       integer      status
C
C Remove an entry from the map catalogue, the disc file will remain.
C If check_status is set to true then the entry must be a permanent
C map file. If set to false this check is not performed.
*-
       include 'mapcat_pars.inc'

       integer   map_status(10)

       if (status.ne.0) return
       call mapcat_end(imap,status)
       call mapcat_enqst(imap,map_status,status)
       if (check_status) then
         if (map_status(ip_page).eq.true) then
           call io_wrout(
     *     '***(CATALOGUE) Catalogue entry is not a permanent file')
           call io_wrout(
     *     '***(CATALOGUE) Use Export-Permanent-Map or Delete-Map-File')
           return
         end if
       end if
       map_status(ip_access) = access_clear
       map_status(ip_data)   = false
       map_status(ip_open)   = false
       map_status(ip_unit)   = 0
       map_status(ip_page)   = false
       call mapcat_setst(imap,map_status,status)
       call stack_remove(imap,status)
       end
