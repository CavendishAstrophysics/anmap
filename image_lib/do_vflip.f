C
C
*+ do_vflip

       subroutine do_vflip(map_array,status)
C      -------------------------------------
C
C perform a vertical flip of an image
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The input map is flipped in the "V" direction.
*-

C Local variables
       integer    minirt(8), ip_map, imap, ip_mapo, imapo
       integer    iu, iv, ipo, ipi

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default_Map','read',imap,status)
       if (status.ne.0) goto 999
C read range for map
       call enminirt(minirt,status)
C pointer to output map
       call map_alloc_out(0,0,'DIRECT',imapo,ip_mapo,status)
C force input map into core -- return pointer
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       if (status.ne.0) goto 999

C do the flip
       do iv=1,minirt(6)
         ipo = (minirt(6)-iv)*minirt(5) + ip_mapo - 1
         ipi = (iv-1)*minirt(5) + ip_map - 1
         do iu=1,minirt(5)
           map_array(ipo+iu) = map_array(ipi+iu)
         end do
       end do

C update the text of the redtape
       call adredt('CREATED','VFLIP',status)

C Tidy up, make output map new current map etc.
999    continue
       call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'VFLIP','Failed ')
       end
