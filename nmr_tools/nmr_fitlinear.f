C NMR: NMR_FITLINEAR
C ----
C
C Fit a linear to a data set
C
C Input:
C   - file for fitting
C Notes:
C   The fitting assumes a function of the form:
C
C      mx +c  between the limits X1 to X2
C
C   X1/X2 default to the entire data range
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
       real*4     m, c, x1, x2, xx, step
       integer    n1, n2
C array for fitted data spectrum
       real*4     xfit(max_size), yfit(max_size), x_max, x_min
C data arrays passed in common
       real*4     x(max_size), y(max_size)
       common    /fitdat/ x, y, n1, n2
C
C Output unit
       integer    iout
       parameter (iout = 6)
C
C Local variables used in NAG fit
       integer    len_work
       parameter (len_work = 400 + 16*max_size)
       real*8     fsumsq, fit(2), work(len_work)
       integer    nvals, ndata, iwork(100), ifail
C loop counter
       integer    n
C file and directory name
       character*256  dir, file
C unit and spectrum identifiers
       integer        iunit, id
C status
       integer        status
C test variable
       logical        test

C standard include files
       include '/mrao/include/chrlib_functions.inc'
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'

C initialise
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'nmr_fitlinear',
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
       do n = 1,npts
         x_max = max(x_max,x(n))
         x_min = min(x_min,x(n))
       enddo

C read in the control data
       call cmd_itemset( 'x1',test,status)
       if (test) then
         call cmd_itemr( 'x1', 1, x1, status )
       else
         x1 = x_min
       endif
       call cmd_itemset( 'x2',test,status)
       if (test) then
         call cmd_itemr( 'x2', 1, x2, status )
       else
         x2 = x_max
       endif
C sort out the limits
       do n = 1,npts
         if (x(n).le.x1) then
           n1 = n
         endif
         if (x(n).le.x2) then
           n2 = n
         endif
         x_max = max(x_max,x(n))
       enddo
C deallocate
       call spec_deallocate( id, status )

C sort out options
       fit(1) = 0.0
       fit(2) = (y(n2)-y(n1))/(x(n2)-x(n1))
       ndata = n2 - n1 + 1
       nvals = 2

C fit a relaxation curve to the data
       ifail = 1
       call e04fdf(ndata, nvals, fit, fsumsq,
     *             iwork, 100, work, len_work, ifail)
       if (ifail.ne.0) then
         print *,'***(FITLINEAR) Ifail non-zero from NAG = ',ifail
         if (ifail.eq.1) goto 999
       end if
       c = fit(1)
       m = fit(2)
       write( iout, 110 ) ndata, fsumsq, m, c, x1, x2
110      format(1X/
     *          1X,'.. Results of fitting  mx + c'/
     *          1X,'.. Number data points = ',I12,
     *          1X,'   Sum of squares     = ',1PE12.3/
     *          1X,'..                 m  = ',1PE12.3,
     *          1X,'                   c  = ',1PE12.3/
     *          1X,'.. Data range:     x1 = ',1PE12.3,
     *          1X,'                   x2 = ',1PE12.3)

C construct output spectrum as a fit to these data
       step = (1.1*x_max-x_min)/float(10*ndata-1)
       call cmd_items('fit',file,status)
       call io_opefil( iunit, file(1:chr_lenb(file)), 'WRITE', 0,
     *                 status )
       write(iunit,*)'%ncols 2 '
       write(iunit,*)'%ndata ',10*ndata
         do n=1,10*ndata
           xx = x_min + float(n-1)*step
           xfit(n) = xx
           yfit(n) = m*xx + c
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
       real*4     x(max_size), y(max_size)
       integer    n1, n2
       common    /fitdat/ x, y, n1, n2

       integer    n, nn

       nn = n1 - 1
       do n=1,ndata
         nn = nn + 1
              f(n) = y(nn) - fit(1) - fit(2)*x(nn)
       end do
       end
