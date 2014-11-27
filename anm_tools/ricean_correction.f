C Ricean Correction : correct an image for ricean bias
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
       real*4        noise, blank_value
C pointers and map identifiers
       integer       imap, ip_map, imapo, ip_mapo
C counter
       integer       i
C mini redtape for map size etc.
       integer       minirt(8)
C functions
       real*4        image_polcorr

C perform standard initialization
       call anm_start( 0,nm,msize,nb,bsize,status )

       call map_getmap('Catalogue-Entry : ','Default_Map',
     *                 'READ',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call map_alloc_out(0,0,'DIRECT',imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C Perform other user interaction
C .. read the estimated noise level
1      call io_getr('Noise-estimate: ','1.0',noise,status)
       if (noise.le.0.0) then
         call io_wrout( '*** Illegal noise estimate must be > 0.0' )
         goto 1
       endif
       if (status.ne.0) goto 999

C correct the map
       call enminirt(minirt,status)
       call ennull(blank_value,status)
       do i = 1,minirt(5)*minirt(6)
         if (map_array(ip_map+i-1).ne.blank_value) then
           map_array(ip_mapo+i-1) = noise *
     *         image_polcorr( map_array(ip_map+i-1), status )
         else
           map_array(ip_mapo+i-1) = blank_value
         end if
       end do

C Update the TEXT of the redtape: this is essential to get a
C sensible entry in the map catalogue
       call adredt('CREATED','RICEAN',status)
       call stnull(blank_value,status)

999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)

C check status value and report an error to the user
       call cmd_err(status,'RICEAN-CORRECTION','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end


