C
C
*+ do_stretch

       subroutine do_stretch(map_array,status)
C      ---------------------------------------
C
C reproject a map to sky-plane
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The input map is reprojected to sky-plane.  This routine provides
C a subset of the facilities of reproject-map.
*-

C Local variables
       integer    minirt(8), ip_map, imap, ip_mapo, imapo
       integer    nx, ny

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default_Map','read',imap,status)
       if (status.ne.0) goto 999
C read new range for map
       call enminirt(minirt,status)
       call plot_getuv('UV-range on input : ','*',minirt,status)
       if (status.ne.0) then
         call cmd_wrerr('STRETCH','Invalid range on input map')
         goto 999
       end if
C stetch the specified region of the map
       nx=minirt(2)-minirt(1)+1
       ny=minirt(3)-minirt(4)+1
C pointer to output map
       call map_alloc_out(0,0,'DIRECT',imapo,ip_mapo,status)
C force input map into core -- return pointer
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       if (status.ne.0) goto 999

C extract the map data to the output map
       call map_stretch(map_array(ip_map),minirt,map_array(ip_mapo),
     *                  status)

C update the text of the redtape
       call adredt('CREATED','STRETCH',status)

C Tidy up, make output map new current map etc.
999    continue
       call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'STRETCH','Failed ')
       end
