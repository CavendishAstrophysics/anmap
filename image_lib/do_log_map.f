C
C
*+ do_log_map

       subroutine do_log_map(map_array,status)
C      ---------------------------------------
C
C Take the logarithm of a map
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The natural logarithm of a map is calculated. Only points which
C are above the user specified gate are included. An offset may be
C applied to the map to enable negative data to be mapped into the
C log.
*-

C Local variables
       integer    minirt(8), iu, iv
       integer    imap, ip_map, imapo, ip_mapo
       real*4     gate, offset, blank_value

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default-Map',
     *                 'READ',imap,status)
       call ennull(blank_value,status)
       call io_getr('Gate : ','0.0',gate,status)
       call io_getr('Offset : ','0.0',offset,status)

C allocate maps
       call map_alloc_in(imap,'SEQUENTIAL',map_array,ip_map,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C calculate the logarithm of the map
       if (status.eq.0) then
         call enminirt(minirt,status)
         do iv = minirt(3),minirt(4),-1
           call map_row_read(imap,iv,map_array,ip_map,status)
           do iu = 1,minirt(5)
             if ( (map_array(ip_map+iu-1).eq.blank_value)    .or.
     *            ((map_array(ip_map+iu-1)+offset).le.0.0)   .or.
     *            (map_array(ip_map+iu-1).le.gate)         ) then
               map_array(ip_mapo+iu-1) = blank_value
             else
               map_array(ip_mapo+iu-1) =
     *                             log(map_array(ip_map+iu-1)+offset)
             end if
           end do
           call map_row_write(imapo,iv,map_array,ip_mapo,status)
         end do
       end if

C update the text of the redtape
       call redt_load(imap,status)
       call adredt('created','LOG-MAP',status)

C tidy up, make output map new current map etc.
999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'LOG-MAP','Failed ')
       end
