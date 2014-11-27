C
C
*+ stack_enqpnt

       subroutine stack_enqpnt(is,ip,status)
C      -------------------------------------
C
C Enquire the pointer to the data array for the stack entry
C
C Input:
C    Stack entry
       integer             is
C Returned
C    Pointer
       integer             ip
C    Status
       integer             status
*-
       include 'mapcat_stack.inc'

       if (status.ne.0) return
       call stack_chk(is,status)
       if (status.ne.0) then
         call mapcat_err(status,'stack_enqpnt',' ')
       else
         ip = stack_status(ip_pointer,is)
       end if
       end
