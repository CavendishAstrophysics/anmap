C
C
*+ stack_display1

       subroutine stack_display1(option,status)
C      ----------------------------------------
C
C Display stack allocation or active map information
C
C Given:
C    Option to display (STACK, ACTIVE, SIZES)
       integer      option
C Returned:
C    Status
       integer      status
*-

       include '/mrao/include/chrlib_functions.inc'
       include 'mapcat_stack.inc'

       integer   iout, n, i

C check status on entry
       if (status.ne.0) return

C find output unit number
       call io_enqout(iout)

C Either display STACK entries of ACTIVE maps
       if (option.eq.1) then
         write(iout,50)
         do n = 1,number_stack_entries
           write(iout,100) n,
     *          (stack_status(i,n), i=1,length_stack_status)
         end do
         write (iout,*) ' '
 50      format(1x/' Entry   Status-Word '/1x,80('-'))
100      format('  ',i3,'     ',10i7)

       else if (option.eq.2) then
         write(iout,150)
         do n = 1,max_active_maps
           write(iout,200) n,
     *          (active_list(i,n), i=1,length_active_list)
         end do
         write (iout,250) number_map_buffers,
     *                   (buffer_status(i), i=1,number_map_buffers)
         write(iout,*)' '
150      format(1x/' Entry   Active-Word '/1x,80('-'))
200      format('  ',i3,'     ',10i7)
250      format(1x/' Number of buffers = ',I4/
     *             ' Buffer-status     = ',15I2)

       else if (option.eq.3) then
         write(iout,300)max_number_stack_entries,
     *                  max_active_maps
         write(iout,400)'Redtape    ',number_redtape_entries,
     *         size_redtape_entry
         write(iout,400)'Maps       ',number_map_entries,size_map_entry
         write(iout,400)'Map-buffers',number_map_buffers,size_map_buffer
         write(iout,400)'Work       ',number_work_entries,
     *         size_work_entry
300      format(1x/' Maximum number of stack entries : ',I4/
     *             ' Maximum number of active maps   : ',I4/
     *          1x/    ' Type       ',' Number ','     Size ')
400      format(1x,a11,i5,i12)
         write(iout,*)' '

       end if
999    call mapcat_err(status,'stack_display',' ')

       end
