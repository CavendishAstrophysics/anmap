C
C
*+ do_expand

       subroutine do_expand(map_array,status)
C      --------------------------------------
C
C Expand the size of a map
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
       integer       minirt_in(8), minirt_out(8), iblank,
     *               ip_map, imap, ip_mapo, imapo, i, ii
       integer       iu, iv, uv_pix(2), iuvmap2
       real*4        blank
       equivalence  (blank, iblank)
       equivalence  (iu, uv_pix(1))
       equivalence  (iv, uv_pix(2))
       
C check status on entry
       if (status.ne.0) return

C find input map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       if (status.ne.0) goto 999

C read new range for map
       call enminirt(minirt_in,status)
       do i=1,8
         minirt_out(i) = minirt_in(i)
       enddo
       iblank = minirt_in(8)
       call io_getni('UV-range on output : ','*',minirt_out,4,status)
       call io_getr('Value for "blank" regions : ','*',blank,status)
       if (status.ne.0) goto 999

C extract the map region and update the computing redtape
       minirt_out(5)=abs(minirt_out(2)-minirt_out(1)+1)
       minirt_out(6)=abs(minirt_out(3)-minirt_out(4)+1)

C allocate output map
       call map_alloc_out(minirt_out(5),minirt_out(6),'DIRECT',
     *                    imapo,ip_mapo,status)
C read input map
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       if (status.ne.0) goto 999

C initialise the output map and path the input map into the output
       do i=1,minirt_out(5)*minirt_out(6)
         map_array(ip_mapo+i-1) = blank
       enddo
       do iv=minirt_in(3),minirt_in(4),-1
         do iu=minirt_in(1),minirt_in(2),1
           i = iuvmap2(minirt_out,uv_pix)
           ii = iuvmap2(minirt_in,uv_pix)
           map_array(ip_mapo+i-1) = map_array(ip_map+ii-1)
         enddo
       enddo

C create correct redtape for the output map
       call stredt(minirt_out,3,status)

C update the text of the redtape
       call adredt('CREATED','EXPAND',status)

C Tidy up
999    continue
       call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'EXPAND','Failed ')
       end
