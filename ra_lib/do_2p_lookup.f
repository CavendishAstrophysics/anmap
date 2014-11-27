C
C
*+ do_2p_lookup

       subroutine do_2p_lookup(status)
C      -------------------------------
C
C Construct a two-point spectral index vs X table for the current spectrum
C
C Returned:
C     status
       integer      status
C
C A table of spectral-index as a function of X (X = sqrt(f1/f_break))
C is constructed for the supplied frequencies f1, f2.  This may be
C plotted.  The lookup table is available for future fitting of spectral
C index results.
*-
       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'


C local variables
       real*4      f1, f2, x1, x2, x, y
       real*4      synch_2p_val
       integer     i
       integer     number_line_segments, iunit
       parameter  (number_line_segments = 100)
       save        f1, f2

C check status on entry
       if (status.ne.0) return

C prompt user for frequencies
       if (f1.le.0.0) f1 = 1.4
       if (f2.le.0.0) f2 = 5.0
       call io_getr('Lower-frequency (GHz) : ','*',f1,status)
       call io_getr('Upper-frequency (GHz) : ','*',f2,status)

C make lookup table
       call synch_2p_make( f1, f2, status )
       if (status.ne.0) goto 999


C open results file
       call io_opefil(iunit,general_results_file,'write',0,status)
       write(iunit,*)'%ndata ',number_line_segments
       write(iunit,*)'%ncols   2'

C plot function
       x = x1
       y = synch_2p_val(x)
       do i = 1,number_line_segments
         x = x1 + i*(x2-x1)/float(number_line_segments)
         y = synch_2p_val(x)
         write(iunit,*) x, y
       end do

C close results file
       close (iunit)

C error report
999    call iocmd_err(status,'MAKE-ALPHA-X-TABLE',' ')

       end

