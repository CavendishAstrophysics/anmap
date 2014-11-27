C
C
*+ do_rotate

       subroutine do_rotate(map_array,status)
C      --------------------------------------
C
C Rotate a map on the UV grid
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The map is rotated on the UV grid.  The operation is performed simply
C in terms of the relative positioning of the SKY intensity relative to
C the sampling of the grid.  No astrometric calculations are performed
C and no changes are made to the astrometric redtape of the images.
C The rotation is defined to be a rotation of the IMAGE relative to the
C grid by the supplied POSITIVE (i.e. counter-clockwise) angle.
C The map is rotated within the supplied grid -- a previous call to expand-map
C may be required if there is insufficient space around the map.
*-

C Local variables
       integer    ip_map, imap, ip_mapo, imapo, minirt(8)
       real*4     rot, uc, vc

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call enminirt(minirt,status)
       if (status.ne.0) goto 999
C read shift in U and V
       call io_getr('Rotation-angle (degrees) : ','0.0',rot,status)
       uc = nint((minirt(2)+minirt(1))*0.5)
       vc = nint((minirt(3)+minirt(4))*0.5)
       call io_getr('Rotation-centre-U : ','*',uc,status)
       call io_getr('Rotation-centre-V : ','*',vc,status)

C force input map into core -- return pointer
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
C pointer to output map
       call map_alloc_out(0,0,'DIRECT',imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C shift the map data to the output map
       call image_rotate(minirt,map_array(ip_map),rot,uc,vc,2,
     *                   map_array(ip_mapo),status)

C update the text of the redtape
       call adredt('CREATED','ROTATE',status)

C Tidy up, make output map new current map etc.
999    continue
       call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'ROTATE','Failed ')
       end
