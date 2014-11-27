C
C
*+ anm_end

       subroutine anm_end( status )
C      ----------------------------
C
C Perform standard close-down for stand-alone programs
C
C Returned:
C  error status
       integer    status
C
C This routine will close down map catalogue & Anmap plotting.
C STATUS is NOT inherited by this routine.  If status is non-zero 
C on entry it is set to zero and the shut-down continues.  This 
C routine must be called as the last routine in any program.  
C Note this does not cause the program to actually halt execution.
C
C P. Alexander MRAO, Cambridge 22/02/94
*-

       if (status.ne.0) status = 0
       call pgend
       call mapcat_close(status)
       end

