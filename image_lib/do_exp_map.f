C
C
*+ do_exp_map

       subroutine do_exp_map(map_array,status)
C      ---------------------------------------
C
C Exponentiate a map
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The exponential of map data is calculated. To avoid overflows a check
C is placed on the largest map value. If an overflow would occur a count
C is kept and the pixels are set to the blank value.
*-

C Local variables
       integer    minirt(8), iu, iv, iout, overflows
       integer    imap, ip_map, imapo, ip_mapo
       real*4     blank_value, max_value

C check status on entry
       if (status.ne.0) return

C define the maximum allowed value (with a little to spare)
       max_value = log(1.0E+35)

C read input map
       call map_getmap('Map : ','Default-Map',
     *                 'READ',imap,status)
       call ennull(blank_value,status)

C allocate maps
       call map_alloc_in(imap,'SEQUENTIAL',map_array,ip_map,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C calculate the exponential of the map
       if (status.eq.0) then
         overflows = 0
         call enminirt(minirt,status)
         do iv = minirt(3),minirt(4),-1
           call map_row_read(imap,iv,map_array,ip_map,status)
           do iu = 1,minirt(5)
             if ( map_array(ip_map+iu-1).eq.blank_value ) then
               map_array(ip_mapo+iu-1) = blank_value
             else if ( map_array(ip_map+iu-1).gt.max_value ) then
               map_array(ip_mapo+iu-1) = blank_value
               overflows = overflows + 1
             else
               map_array(ip_mapo+iu-1) = exp(map_array(ip_map+iu-1))
             end if
           end do
           call map_row_write(imapo,iv,map_array,ip_mapo,status)
         end do
       end if

C inform user of any overflows
       if (overflows .gt. 0) then
         call io_enqout(iout)
         write(iout,'(1x/1x,''***(EXP-MAP) Overflows have occured''/
     *         1x,''***(EXP-MAP) '',i8,'' overflows have occured'')')
     *         overflows
         write(iout,'(1x,''***(EXP-MAP) Pixels reset to BLANK''/1x)')
       end if

C update the text of the redtape
       call redt_load(imap,status)
       call adredt('created','EXP-MAP',status)

C tidy up, make output map new current map etc.
999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'EXP-MAP','Failed ')
       end
