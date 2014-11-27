C
C
*+ stack_doset

       subroutine stack_doset(is,ip,status)
C      ------------------------------------
C
C Update all INTERLOCKED stack_status entries
C
C Input:
C    Stack entry
       integer          is
C    Stack_status word entry
       integer          ip
C Returned
C    Status
       integer          status
C
C For maps larger than the nominal size of a map in the map stack more
C than one slot of the stack will be allocated to this map.  This
C routine ensures that all stack entries allocated to the same map have
C consistent stack_status work entries.
*-
       integer          i
       include 'mapcat_stack.inc'

       if (status.ne.0) return
       do i = is,is+stack_status(ip_interlock,is)-1
         stack_status(ip,i) = stack_status(ip,is)
       end do
       call mapcat_err(status,'stack_doset',' ')

       end
