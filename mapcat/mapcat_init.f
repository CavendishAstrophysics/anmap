C
C
*+ mapcat_init

       subroutine mapcat_init(status)
C      ------------------------------
C
C Initialise the map catalogue
C
C Returned:
C    Status
       integer     status
*-
       integer   imap
       include 'mapcat_cat.inc'

       if (status.ne.0) return
       call mapcat_open(status)
       do imap = 1, max_cat_entries
         current_filename = ' '
         current_source   = ' '
         current_program  = ' '
         current_map      = imap
         current_map_status(ip_data)   = false
         current_map_status(ip_open)   = false
         current_map_status(ip_unit)   = 0
         current_map_status(ip_access) = access_clear
         call mapcat_write(imap,status)
       end do
       call mapcat_err(status,'mapcat_init',' ')
       end
