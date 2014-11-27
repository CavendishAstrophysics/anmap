*$ 5) Stack Allocation Routines
*  ----------------------------

*+ stack_chk

       subroutine stack_chk(is,status)
C      -------------------------------
C
C Checks that the specified stack entry is valid
C
C Input:
C    Stack entry
       integer      is
C Returned:
C    Status
       integer      status
C
C Check that the specified stack entry is valid.  If the entry is not
C valid status is returned with a non-zero value of ILL_STKENT. No
C error message is produced by this routine.
C
*-

       include 'mapcat_stack.inc'
       include 'mapcat_errors.inc'

       if (status.ne.0) return
       if (is.le.0 .or. is.gt.
     * (number_map_entries+number_redtape_entries)
     *    ) then
         is = ill_stkent
       end if
       end
