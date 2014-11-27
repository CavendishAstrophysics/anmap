C
C
*+ do_divide_maps

       subroutine do_divide_maps(map_array,status)
C      -------------------------------------------
C
C Divide two maps
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C Returned is the DIVISION of the two maps.
C
C A gate (absolute) may be specified so that points in the numerator or
C denominator whose absolute value falls below these gates will not be
C included in the calculation -- corresponding pixels in the output image
C will be set to the NULL value.
*-

C Local variables
       integer    minirt(8), iu, iv, iwarn
       integer    imap1, imap2, imapo
       integer    ip_map1, ip_map2, ip_mapo
       real*4     gate_1, gate_2, blank_value_1, blank_value_2

C check status on entry
       if (status.ne.0) return

C read map catalogue entries
C read input map -- 1
       call io_wrout('.. dividing: map-1 / map-2')
       call map_getmap('Map-1 : ','Default-Map','READ',imap1,status)
       call ennull(blank_value_1,status)
       gate_1 = 0.0
       call io_getr('Gate (absolute) for Map-1 : ','*',gate_1,status)
C read input map -- 2
       call map_getmap('Map-2 : ','Default-Map','READ',imap2,status)
       call ennull(blank_value_2,status)
       gate_2 = 1.0E-50
       call io_getr('Gate (absolute) for Map-2 : ','*',gate_2,status)

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
     *           map_array(ip_map2+iu-1).eq.blank_value_1 .or.
     *           abs(map_array(ip_map1+iu-1)).lt.gate_1   .or.
     *           abs(map_array(ip_map2+iu-1)).lt.gate_2   ) then
               map_array(ip_mapo+iu-1) = blank_value_1
             else
               map_array(ip_mapo+iu-1) = map_array(ip_map1+iu-1)
     *                                 / map_array(ip_map2+iu-1)
             end if
           end do
           call map_row_write(imapo,iv,map_array,ip_mapo,status)
         end do
       end if

C Update the TEXT of the redtape
       call adredt('created','DIVIDE',status)
       call stnull(blank_value_1,status)

C Tidy up, make output map new current map etc.
999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap1,map_array,status)
       call map_end_alloc(imap2,map_array,status)

C check STATUS value
       call cmd_err(status,'DIVIDE-MAPS','Failed ')
       end
