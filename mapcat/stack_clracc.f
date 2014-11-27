C
C
*+ stack_clracc

       subroutine stack_clracc(imap,status)
C      ------------------------------------
C
C Clear the access states to map imap
C
C Given:
C     Map catalogue entry
        integer      imap
C Returned:
C     Status
        integer      status
C
C The access state to map IMAP is cleared.  If the map was paged in
C then a record of its presence in the stack will remain for possible
C future reference.
C
*-
       include 'mapcat_stack.inc'

       integer   i, j

       if (status.ne.0) return
       do i = 1,number_stack_entries
         if (stack_status(ip_allocated,i).eq.imap) then
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
       call mapcat_err(status,'stack_clracc',' ')
       end
