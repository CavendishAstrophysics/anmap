C
C
*+ stack_enqdat

       subroutine stack_enqdat(is,data_code,status)
C      --------------------------------------------
C
C Enquire the data code for the stack entry
C
C Input:
C    Stack entry
       integer             is
C Returned
C    Data code
       integer             data_code
C    Status
       integer             status
C
C Enquire whether data is present in catalogue entry IMAP.  If data is
C present DATA_CODE is returned with the value 1 -- TRUE and 0 -- FALSE
C if data is not present.
C
*-
       include 'mapcat_stack.inc'

       if (status.ne.0) return
       call stack_chk(is,status)
       if (status.ne.0) then
         call mapcat_err(status,'stack_enqdat',' ')
       else
         data_code = stack_status(ip_data,is)
       end if
       end
