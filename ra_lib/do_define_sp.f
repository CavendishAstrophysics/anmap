*+ do_define_sp

       subroutine do_define_sp(status)
C      -------------------------------
C
C Prompt user for spectral parameters
C
C Returned:
C     status
       integer      status
*-
       integer    spectral_type
       real*4     injection_index_E

C check status on entry
       if (status.ne.0) return

C prompt for the spectral parameters
10     call io_geti('Spectral-type (1-5) : ','5',spectral_type,status)
       if (status.ne.0) goto 999
       if (spectral_type.lt.1 .or. spectral_type.gt.5) then
         call io_wrout('*** invalid spectral type')
         goto 10
       end if
       call io_getr('Injection-index (GAMMA, electron spectrum) : ','*',
     *            injection_index_E,status)

C define spectrum
       call synch_setsp(injection_index_E,spectral_type,status)

C error report
999    call cmd_err(status,'DEFINE-SYNCHROTRON-SPECTRUM',' ')

       end
