C NMR: NMR_FIT2T1
C ----
C
C Fit two saturatikjon recovery T1 curves to a dataset
C
C Input:
C   - file for fitting
C Notes:
C   The fitting assumes a function of the form:
C
C      A(1-exp(-t/tr1)) + B(1-exp(-t/tr2))
C
C For all fits the NAG routine E04FDF is used for the least squares
C minimization.
C
C
       implicit   NONE
C
C Data file definitions
       integer    max_size
       parameter (max_size = 1024)
       integer    npts
C Estimates for results and limits on the data
       real*4     scale, step, xx
C array for fitted data spectrum
       real*4     xfit(max_size), yfit(max_size), x_max, x_min,
     *            y_max, y_min
C data arrays passed in common
       real*4     x(max_size), y(max_size)
       common    /fitdat/ x, y, scale
C
C Output unit
       integer    iout
       parameter (iout = 6)
C
C Local variables used in NAG fit
       integer    len_work
       parameter (len_work = 400 + 16*max_size)
       real*8     fsumsq, fit(4), work(len_work)
       integer    nvals, ndata, iwork(100), ifail
C loop counter
       integer    n
C file and directory name
       character*256  dir, file
C unit and spectrum identifiers
       integer        iunit, id
C status
       integer        status

C standard include files
       include '/mrao/include/chrlib_functions.inc'
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'

C initialise
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'nmr_fit2T1',
     *      dir(1:chr_lenb(dir))//anal_definitions, status)

C find input file name and allocate file
       call cmd_items( 'file', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id, status)

C read data
       call spec_get_data( id, 1, x, npts, status)
       call spec_get_data( id, 2, y, npts, status)
       x_max = -1.0E+30
       x_min = 1.0E+30
       y_max = -1.0E+30
       y_min = 1.0E+30
       do n = 1,npts
         x_max = max(x_max,x(n))
         x_min = min(x_min,x(n))
         y_max = max(y_max,y(n))
         y_min = min(y_min,y(n))
       enddo

C deallocate
       call spec_deallocate( id, status )

C sort out options
       scale = 1.0/y_max
       fit(1) = 1.0
       fit(2) = x_max/float(npts)
       fit(3) = 0.0
       fit(4) = x_max/float(npts)
       ndata = npts

C fit a relaxation curve to the data
       ifail = 1
       nvals = 2
C .. find initial guesses
       call e04fdf(ndata, nvals, fit, fsumsq,
     *             iwork, 100, work, len_work, ifail)
       nvals = 4
       fit(3) = fit(1)/10.0
       fit(4) = fit(2)
       ifail = 1
       print *,'STARING-GUESS: ',fit
       call e04fdf(ndata, nvals, fit, fsumsq,
     *             iwork, 100, work, len_work, ifail)
       if (ifail.ne.0) then
         print *,'***(FIT2T1) Ifail non-zero from NAG = ',ifail
         if (ifail.eq.1) goto 999
       end if
       write( iout, 110 ) ndata, fsumsq,
     *      fit(1)/scale,fit(2),fit(3)/scale,fit(4)
110      format(1X/
     *          1X,'.. Results of fitting  A(1-exp(-t/tr1)) + '/
     *          1X,'                          B(1-exp(-t/tr1))'/
     *          1X,'.. Number data points = ',I12,
     *          1X,'   Sum of squares     = ',1PE12.3/
     *          1X,'..                A   = ',1PE12.3,
     *          1X,'                  tr1 = ',1PE12.3/
     *          1X,'..                B   = ',1PE12.3,
     *          1X,'                  tr2 = ',1PE12.3)

C construct output spectrum as a fit to these data
       step = (1.1*x_max-x_min*0.9)/float(10*ndata-1)
       call cmd_items('fit',file,status)
       call io_opefil( iunit, file(1:chr_lenb(file)), 'WRITE', 0,
     *                 status )
       write(iunit,*)'%ncols 2 '
       write(iunit,*)'%ndata ',10*ndata
         do n=1,10*ndata
           xx = x_min*0.9 + float(n-1)*step
           xfit(n) = xx
           yfit(n) = fit(1)*(1.0-exp(-xx/fit(2))) +
     *               fit(3)*(1.0-exp(-xx/fit(4)))
           yfit(n) = yfit(n)/scale
           write(iunit,*) xfit(n), yfit(n)
         end do
       close (iunit)

999    continue

       end
C
C
       subroutine LSFUN1( ndata, nvals, fit, f )
C
C Routine required by NAG -- see definition in NAG manual for details
C
       integer    nvals, ndata
       real*8     fit(nvals), f(ndata)

C Data passed in common
       integer    max_size
       parameter (max_size = 1024)
       real*4     x(max_size), y(max_size), scale
       common    /fitdat/ x, y, scale

       integer    n
       if (nvals.eq.2) then
         do n=1,ndata
             f(n) = y(n)*scale -
     *                fit(1)*(1.0-exp(-x(n)/fit(2)))
         end do
       else
         do n=1,ndata
             f(n) = y(n)*scale -
     *                fit(1)*(1.0-exp(-x(n)/fit(2))) -
     *                fit(3)*(1.0-exp(-x(n)/fit(4)))
         end do
       endif
       end
