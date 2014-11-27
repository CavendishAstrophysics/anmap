C
C
*+ cln_result

       subroutine cln_result(iout,status)
C      ----------------------------------
C
C Report on the results of previous CLEANs to unit IOUT
C
C Given:
C    output unit number
       integer     iout
C Returned:
C    error status
       integer     status
C
C Report details of the results of completed CLEAN runs.
C
*-
       include '../include/clean_record.inc'

C check status on entry
       if (status.ne.0) return

       if (times_cleaned .eq. 0) then
         write(iout,'(1x/1x,''.. clean not run on this map''/1x)')
         return
       end if
       write(iout,120) times_cleaned,number_it_done,number_cc,
     >                 max_on_resid
120    FORMAT(1X/1X,'.. CLEAN completion information:'/1X/
     >           1X,'   Number of times CLEANed     =',I8/
     >           1X,'   Number iterations completed =',I8/
     >           1X,'   Number of CLEAN components  =',I8/
     >           1X,'   Maximum on residual map     =',1PE12.3/1X)
       if (how_ended .lt. 0) then
         WRITE(IOUT,'(1X,''.. CLEAN terminated abnormally STATUS = '',
     >                i4)')how_ended
       else if (how_ended .eq. 1) then
         WRITE(IOUT,'(1X,''.. CLEAN Terminated at iteration limit = '',
     >                I8/'' .. Residual-maximum = '',1PE12.3)')
     >                number_it_done,max_on_resid
       else if (how_ended .eq. 2) then
         WRITE(IOUT,'(1X,''.. CLEAN Terminated at flux limit = '',
     >                1PE12.3/'' .. Number of iterations = '',I8)')
     >                max_on_resid,number_it_done
       else if (how_ended .eq. 3) then
         WRITE(IOUT,'(1X,''.. CLEAN terminated at negative residual'')')
       else if (how_ended .eq. 4) then
         WRITE(IOUT,'(1X,''.. CLEAN terminated on user intervention'')')
       end if
       write(iout,*)' '

       end
