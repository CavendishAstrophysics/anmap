C
C
*+ stack_enqmap

       subroutine stack_enqmap(imap,is_map,status)
C      -------------------------------------------
C
C Find stack entry for map IMAP if one exists
C
C Input:
C    Map catalogue entry
       integer        imap
C Returned
C    Stack entry
       integer        is_map
C    Status
       integer        status
C
C The stack entry of map IMAP is returned if the map is paged into the
C stack.  If map IMAP is not paged in then the returned value of
C IS_MAP is less than zero.
C
*-
       include 'mapcat_stack.inc'

       integer   is

C check status on entry
       if (status.ne.0) return

C look through the allocated maps and return the redtape and the map
C pointers
       is_map  = -1
       do is = 1,number_stack_entries
         if (stack_status(ip_type,is).eq.type_map) then
           if (stack_status(ip_allocated,is).eq.imap) then
             if (is_map.eq.-1) then
               is_map = is
             end if
           end if
         end if
       end do

       call mapcat_err(status,'stack_enqmap',' ')

       end
