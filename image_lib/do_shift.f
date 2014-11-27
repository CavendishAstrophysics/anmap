C
C
*+ do_shift

       subroutine do_shift(map_array,status)
C      -------------------------------------
C
C Shift a map on the UV grid
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The map is shifted on the UV grid.  The operation is performed simply
C in terms of the relative positioning of the SKY intensity relative to
C the sampling of the grid.  No astrometric calculations are performed
C and no changes are made to the astrometric redtape of the images.
*-

C Local variables
       integer    ip_map, imap, ip_mapo, imapo, minirt(8)
       real*8     du, dv

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call enminirt(minirt,status)
       if (status.ne.0) goto 999
C read shift in U and V
       call io_getd('Image-shift in U : ','0.0',du,status)
       call io_getd('Image-shift in V : ','0.0',dv,status)
C force input map into core -- return pointer
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
C pointer to output map
       call map_alloc_out(0,0,'DIRECT',imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C shift the map data to the output map
       call image_shift(minirt,map_array(ip_map),du,dv,2,
     *                  map_array(ip_mapo),status)

C update the text of the redtape
       call adredt('CREATED','UVSHIFT',status)

C Tidy up, make output map new current map etc.
999    continue
       call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'UV-SHIFT-MAP','Failed ')
       end
