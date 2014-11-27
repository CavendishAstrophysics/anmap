C
C
*+ stack_io_setacc

       subroutine stack_io_setacc(is,access,status)
C      -----------------------------------------
C
C Set the access state code for stack entry is
C
C Input:
C    Stack entry
       integer          is
C    Access
       character*(*)    access
C Returned:
C    Status word
       integer          status
C
C Set the access state to a stack entry.  If the requested access is
C "CLEAR" then the associated map is also removed from the active list.
*-

       include 'mapcat_stack.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (status.ne.0) return

       if (is.ge.1 .and. is.le.number_stack_entries) then
         if (chr_cmatch(access,'READ')) then
            stack_status(ip_access,is) = access_read
         else if (chr_cmatch(access,'WRITE')) then
            stack_status(ip_access,is) = access_write
         else if (chr_cmatch(access,'SCRATCH')) then
            stack_status(ip_access,is) = access_scratch
         else if (chr_cmatch(access,'CLEAR')) then
            stack_status(ip_access,is) = access_clear
         else
            status = ill_stkacc
         end if
       else
          status = ill_stkent
       end if
       call stack_doset(is,ip_access,status)
       call mapcat_err(status,'STACK_io_setacc','Access not reset')

       end
