C
C
*+ stack_edit

       subroutine stack_edit(status)
C      -----------------------------
C
C Edit the statck entries
C
C Returned:
C    Status
       integer      status
*-

       include 'mapcat_stack.inc'

       character string*25
       integer   i, is, itype

C check status on entry
       if (status.ne.0) return

C read entry to edit
       itype = 0
       call io_geti('Type (0=stack, 1=active, 2=buffer) : ','*',
     *           itype,status)
       if (itype.eq.0) then
         call io_geti('Entry to edit (0=quit) : ','0',is,status)
         if (is.le.0 .or. is.gt.number_stack_entries) return
         if (status.ne.0) goto 999
         call io_wrout(' ')
         call io_wrout('.. edit stack_status word')
         do i = 1,length_stack_status
           write(string,'(''  Stack_Status('',I1,'') : '')') i
           call io_geti(string(1:20),'*',stack_status(i,is),status)
         end do
         call io_wrout('.. entry replaced')
         call io_wrout(' ')

       else if (itype.eq.1) then
         call io_geti('Entry to edit (0=quit) : ','0',is,status)
         if (is.le.0 .or. is.gt.max_active_maps) return
         if (status.ne.0) goto 999
         call io_wrout(' ')
         call io_wrout('.. edit active_list word')
         do i = 1,length_active_list
           write(string,'(''  Active_List('',I1,'') : '')') i
           call io_geti(string(1:19),'*',active_list(i,is),status)
         end do
         call io_wrout('.. entry replaced')
         call io_wrout(' ')

       else if (itype.eq.2) then
         do i = 1,number_map_buffers
           write(string,'(''  Buffer_Status('',I1,'') : '')') i
           call io_geti(string(1:21),'*',buffer_status(i),status)
         end do

       end if
999    call mapcat_err(status,'stack_edit',' ')

       end
