C
C
*+ stack_setdat

       subroutine stack_setdat(is,data_code,status)
C      --------------------------------------------
C
C Set the data code for stack entry is
C
C Input:
C    Stack entry
       integer          is
C    Data code
       integer          data_code
C Returned:
C    Status word
       integer          status
C
*-

       include 'mapcat_stack.inc'
       include 'mapcat_errors.inc'

       if (status.ne.0) return

       if (is.ge.1 .and. is.le.number_stack_entries) then
         stack_status(ip_data,is) = data_code
       else
          status = ill_stkent
       end if
       call stack_doset(is,ip_data,status)
       call mapcat_err(status,'STACK_SETDAT','Data code not reset')

       end
