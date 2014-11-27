C
C
*+ do_mapprint

       subroutine do_mapprint(map_array,status)
C      ----------------------------------------
C
C Print a map on the selected output device
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
*-

C Local variables
       integer    minirt(10), imap, ip_map

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
C force input map into core
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call enminirt(minirt,status)
       if (status.ne.0) goto 999

C do the work
       call image_print(minirt,map_array(ip_map),status)

C tidy up
999    call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'PRINT-MAP','Failed ')
       end
