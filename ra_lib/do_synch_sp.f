C
C
*+ do_synch_sp

       subroutine do_synch_sp(status)
C      ------------------------------
C
C Plot a theoretical synchrotron spectrum
C
C Returned:
C     status
       integer      status
C
C Plot the currently defined standard spectrum.
C
*-
       include '../include/anmap_sys_pars.inc'

C local variables
       real*4      xmax, x1, x2, x, y
       integer     iunit
       real*4      synch_bfun
       real*8      xmax8
       integer     number_line_segments, i
       parameter  (number_line_segments = 200)
       character   string*80

       include '/mrao/include/chrlib_functions.inc'


C check status on entry
       if (status.ne.0) return

C prompt user for range
       call synch_enqlim(xmax8)
       xmax = xmax8
       call io_getr('Range in dimensionless-frequency (Xmin) : ',
     *              '0.1',x1,status)
       x2 = min(xmax,10.0)
       write (string,'(a,f6.1,a)')
     *        'Range in dimensionless-frequency (Xmax <',xmax,') : '
10     call io_getr(string(1:50),'*',x2,status)
       if (x2.gt.xmax) then
         call io_wrout('*** Xmax exceeds upper limit, re-enter')
         x2 = xmax
         goto 10
       end if
       if (status.ne.0) goto 999
       x1 = log10(x1)
       x2 = log10(x2)

C open results file
       call io_opefil(iunit,general_results_file,'write',0,status)
       write(iunit,*)'%ndata ',number_line_segments
       write(iunit,*)'%ncols   2'

C output function
       x = x1
       y = log10(synch_bfun(10.0**x1,status))
       do i = 1,number_line_segments
         x = x1 + i*(x2-x1)/float(number_line_segments)
         y = log10(synch_bfun(10.0**x,status))
         write(iunit,*) x, y
       end do
       close(iunit)

C error report
999    call iocmd_err(status,'OUTPUT-SYNCHROTRON-SPECTRUM',' ')

       end



