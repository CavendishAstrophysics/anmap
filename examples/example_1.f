C Example-Prog-1:  Scale a map by a user-supplied value
C
C
C The map is scaled such that:
C
C   "output-map"  =  "scale-factor" * ("input-map"  -  "offset-level")
C
C where scale-factor and offset-level are supplied by the user.
C
C This example program performs the same function as scale-map in
C the map-analysis sub-system of ANMAP.
C
C Document origin and update of this routine
C   P. Alexander, MRAO, Cambridge, 09/04/92
C
*-

C define work array
       integer       nm, nb
       integer        msize, bsize
       parameter    (msize = 256*256)
       parameter    (bsize=1024)
       parameter    (nm=16)
       parameter    (nb=3)
       real*4        map_array( nm*msize + nb*bsize )
C error status
       integer       status

C Local variables used for scaling
       real*4        scale_factor, offset_level, blank_value
C pointers and map identifiers
       integer       imap, ip_map, imapo, ip_mapo
C counter
       integer       i
C mini redtape for map size etc.
       integer       minirt(8)

C perform standard initialization
       call anm_start( 0,nm,msize,nb,bsize,status )

C read input map: imap is the catalogue identifier
       call map_getmap('Map : ','Default_Map','READ',imap,status)
C force input map into core: ip_map is pointer to imap
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
C find output map: imapo is the output map and ip_mapo is pointer
C                  to it
       call map_alloc_out(0,0,'DIRECT',imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C Perform other user interaction
C .. read the scale factor and zero-level offset to apply
       call io_getr('Scale-Factor : ','1.0',scale_factor,status)
       call io_getr('Offset-Level : ','0.0',offset_level,status)
       if (status.ne.0) goto 999

C scale the map: use maplib routines to find the miniredtape and
C                the value of blanked pixels on the input map;
C                we will not scale blank pixels
       call enminirt(minirt,status)
       call ennull(blank_value,status)
C loop through the map array from top-left to bottom right, but
C in this case we can treat the map as a single vector rather than
C a square array; not the use of pointers to the input and output
C maps.
       do i = 1,minirt(5)*minirt(6)
         if (map_array(ip_map+i-1).ne.blank_value) then
           map_array(ip_mapo+i-1) = scale_factor *
     *                            (map_array(ip_map+i-1) - offset_level)
         else
           map_array(ip_mapo+i-1) = blank_value
         end if
       end do

C Update the TEXT of the redtape: this is essential to get a
C sensible entry in the map catalogue
       call adredt('CREATED','SCALE',status)
       call stnull(blank_value,status)

C Tidy up: clear access to the maps.  Note how on error we jump to
C          this location in the code so that the allocation to the
C          input and output maps is always cleared.
999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)

C check status value and report an error to the user
       call cmd_err(status,'SCALE-MAP','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end
