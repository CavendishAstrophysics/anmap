C
C
*+ do_mask_map

       subroutine do_mask_map(map_array,status)
C      ----------------------------------------
C
C Mask one map with another
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The first map is masked by the second -- a user range is specified
C and if the value on the second map is within these limits then the
C value from the first map is copied, if not then the value on the first
C map is replaced by a user specified value.
*-

C Local variables
       integer    minirt(8), iu, iv, iwarn
       integer    imap1, imap2, imapo
       integer    ip_map1, ip_map2, ip_mapo
       real*4     gate_low, gate_high, user_blank, val_test,
     *            blank_value_1, blank_value_2

C cehck status on entry
       if (status.ne.0) return

C read map catalogue entries
C read input map -- 1
       call map_getmap('Map-1   (Input-map) : ','Default-Map',
     *                 'READ',imap1,status)
       call ennull(blank_value_1,status)
C read input map -- 2
       call map_getmap('Map-2 (Masking-map) : ','Default-Map',
     *                 'READ',imap2,status)
       call ennull(blank_value_2,status)
       gate_low  = -1.0e+30
       gate_high =  1.0e+30
       call io_wrout('.. input range for allowed data on map 2')
       call io_getr('Low-gate : ','*',gate_low,status)
       call io_getr('High-gate : ','*',gate_high,status)

C check redtapes
       iwarn = 2
       call redt_comp(imap1,imap2,iwarn,status)
       if (status.ne.0. .or. iwarn.gt.5) goto 999

C read blanking value
       user_blank = blank_value_1
       call io_getr('Value for masked-region [BLANK] : ',' ',
     *           user_blank,status)

C allocate maps
       call map_alloc_in(imap1,'SEQUENTIAL',map_array,ip_map1,status)
       call map_alloc_in(imap2,'SEQUENTIAL',map_array,ip_map2,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C do the comparison
       if (status.eq.0) then
         call enminirt(minirt,status)
         do iv = minirt(3),minirt(4),-1
           call map_row_read(imap1,iv,map_array,ip_map1,status)
           call map_row_read(imap2,iv,map_array,ip_map2,status)
           do iu = 1,minirt(5)
             val_test = map_array(ip_map2+iu-1)
             if (val_test.eq.blank_value_2 .or.
     *           map_array(ip_map1+iu-1).eq.blank_value_1) then
               map_array(ip_mapo+iu-1) = user_blank
             else
               if (val_test.ge.gate_low.and.val_test.le.gate_high) then
                 map_array(ip_mapo+iu-1) = map_array(ip_map1+iu-1)
               else
                 map_array(ip_mapo+iu-1) = user_blank
               end if
             end if
           end do
           call map_row_write(imapo,iv,map_array,ip_mapo,status)
         end do
       end if

C update the text of the redtape
       call redt_load(imap1,status)
       call adredt('created','MASK-MAP',status)
       call stnull(user_blank,status)

C tidy up, make output map new current map etc.
999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap1,map_array,status)
       call map_end_alloc(imap2,map_array,status)

C check status value
       call cmd_err(status,'MASK-MAP','Failed ')
       end
