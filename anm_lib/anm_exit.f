C
C
*+ anm_exit

       subroutine anm_exit(status)
C      ---------------------------
C
C Leave ANMAP in a tidy fashion
C
C Updated:
C  error status
       integer       status
*-
       logical       check_status

C check for a non-zero status value on entry
       if (status.ne.0) return

C check temporary maps
       call mapcat_enqch(check_status)
       if (check_status) then
         call mapcat_deltemp(.true.,'no',status)
       end if
       call anm_end( status )
       end
