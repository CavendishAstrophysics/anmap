C
C
*+ synch_enqwn

       subroutine synch_enqwn(warning_flag)
C      ------------------------------------
C
C Enquire the value of the warning flag after a synchrotron fit
C
C Returned:
C   warning flag (0=OK; 1=some trouble; -1=injection_index too large)
       integer      warning_flag
C
*-
       integer      local_warn, mode, icount
       common /local_synch_warn/ local_warn
       common /local_lsfun1_mode/ mode, icount

       warning_flag = local_warn
*       print *,'icount = ',icount

       end
