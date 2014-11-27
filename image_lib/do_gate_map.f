C
C
*+ do_gate_map

       subroutine do_gate_map(map_array,status)
C      ----------------------------------------
C
C Gate a map with itself
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C All map values below a specified gate are set to a BLANKing value.
*-
       include '/mrao/include/iolib_functions.inc'
C
       integer    minirt(8), n, iout
       real*4     gate_low, gate_high, value, blank_value
       integer    imap, imapo, ip_mapo


C check status on entry
       if (status.ne.0) return
C read map entry
       call map_getmap('Map : ','Default-Map','READ',imap,status)
C allocate input map to output map
       call map_alloc_toout(imap,'DIRECT',
     *                      map_array,imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C read gate low and high
       call io_enqout(iout)
       call io_getr('Gate-low  : ','-1.0E+30',gate_low,status)
       call io_getr('Gate-high : ','1.0E+30',gate_high,status)
       call ennull(blank_value,status)
       value = blank_value
       call io_getr('Value to replace with [BLANK] : ',
     *           ' ',value,status)
       if (status.ne.0) goto 999

C blank the map region and update the computing redtape
       call enminirt(minirt,status)
       do n=0,minirt(5)*minirt(6)-1
         if ( map_array(ip_mapo+n).lt.gate_low .or.
     *        map_array(ip_mapo+n).gt.gate_high ) then
           map_array(ip_mapo+n) = value
         else
           map_array(ip_mapo+n) = map_array(ip_mapo+n)
         end if
       end do

C Update the TEXT of the redtape
       call adredt('created','GATE-MAP',status)

C Tidy up, make output map new current map etc.
999    call map_end_alloc(imapo,map_array,status)

C check STATUS value
       call cmd_err(status,'GATE-MAP','Failed ')
       end
