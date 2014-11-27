C
C
*+ stack_remove

       subroutine stack_remove(imap,status)
C      ------------------------------------
C
C Remove allocation of stack to imap if any
C
C Input:
C    Map allocation to remove
       integer          imap
C Returned
C    Status
       integer          status
C
C All record of map IMAP being allocated space in the stack is removed.
C If IMAP is active then the reference to active map IMAP is also removed.
C
*-
       integer          i, j
       include 'mapcat_stack.inc'

       if (status.ne.0) return
       do i = 1,number_stack_entries
         if (stack_status(ip_allocated,i).eq.imap) then
           stack_status(ip_allocated,i) = false
           stack_status(ip_data,i) = false
           stack_status(ip_access,i) = access_clear
         end if
       end do
       do i = 1,max_active_maps
         if (active_list(1,i).eq.imap) then
           if (active_list(2,i).eq.mode_sequential) then
             buffer_status(active_list(6,i)) = 0
           end if
           do j=1,6
             active_list(j,i) = 0
           end do
           active_zmnx(1,i) = -1.0E+30
           active_zmnx(2,i) =  1.0E+30
         end if
       end do
       call mapcat_err(status,'stack_remove',' ')

       end
