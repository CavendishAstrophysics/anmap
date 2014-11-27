C
C
*+ do_multiply_maps

       subroutine do_multiply_maps(map_array,status)
C      ---------------------------------------------
C
C Multiply two maps
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C Returned is the MULTIPLICATION of the two maps.
C
*-

C Local variables
       integer    minirt(8), iu, iv, iwarn
       integer    imap1, imap2, imapo
       integer    ip_map1, ip_map2, ip_mapo
       real*4     blank_value_1, blank_value_2

C check status on entry
       if (status.ne.0) return

C read map catalogue entries
C read input map -- 1
       call map_getmap('Map-1 : ','Default-Map','READ',imap1,status)
       call ennull(blank_value_1,status)
C read input map -- 2
       call map_getmap('Map-2 : ','Default-Map','READ',imap2,status)
       call ennull(blank_value_2,status)

C check redtapes
       iwarn = 2
       call redt_comp(imap1,imap2,iwarn,status)
       if (status.ne.0. .or. iwarn.gt.5) goto 999

C allocate maps
       call map_alloc_in(imap1,'SEQUENTIAL',map_array,ip_map1,status)
       call map_alloc_in(imap2,'SEQUENTIAL',map_array,ip_map2,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C do the division
       if (status.eq.0) then
         call enminirt(minirt,status)
         do iv = minirt(3),minirt(4),-1
           call map_row_read(imap1,iv,map_array,ip_map1,status)
           call map_row_read(imap2,iv,map_array,ip_map2,status)
           do iu = 1,minirt(5)
             if (map_array(ip_map1+iu-1).eq.blank_value_1 .or.
     *           map_array(ip_map2+iu-1).eq.blank_value_2 ) then
               map_array(ip_mapo+iu-1) = blank_value_1
             else
               map_array(ip_mapo+iu-1) = map_array(ip_map1+iu-1)
     *                                 * map_array(ip_map2+iu-1)
             end if
           end do
           call map_row_write(imapo,iv,map_array,ip_mapo,status)
         end do
       end if

C Update the TEXT of the redtape
       call adredt('created','MULTIPLY',status)
       call stnull(blank_value_1,status)

C Tidy up, make output map new current map etc.
999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap1,map_array,status)
       call map_end_alloc(imap2,map_array,status)

C check STATUS value
       call cmd_err(status,'MULTIPLY-MAPS','Failed ')
       end
