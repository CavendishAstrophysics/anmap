C SPECTRUM: FITGAUSS
C ------------------
C
C Fit a model consisting of a number of gaussians to a data set
C
C Input:
C   - file for fitting
C Output:
C   - a the fitted function as a standard data file
C Notes:
C   The fitting assumes a function of the form:
C
C      Sum_{i} ( A(i) exp( (x-xc(i))**2/2.0sigma(i)**2 )
C
C   X1/X2 default to the entire data range to fit; estaimates of
C   all the initial values may be supplied by the user.  Output
C   is in the form of a multi-column data file with the columns arranged
C   as:
C    1)       Original X-column
C    2)       Final fitted data to original Y-column
C    3->2+n)  "n" component gaussians
C
C For all fits the NAG routine E04FDF is used for the least squares
C minimization.
C
C
       implicit   NONE
C
C Data file definitions
       integer    max_size
       parameter (max_size = 2048)
       integer    npts
C Estimates for results and limits on the data
       real*4     x1, x2, x_min, x_max
       integer    n1, n2
C number of gaussians to fit
       integer    max_gauss
       parameter (max_gauss = 6)
       integer    ngauss
C array for fitted data spectrum
       real*4     yfit(0:max_gauss), a(max_gauss),
     *            s(max_gauss), x0(max_gauss) 
       logical    fix_a(max_gauss), fix_s(max_gauss),
     *            fix_x0(max_gauss) 
C data arrays passed in common
       real*4     x(max_size), y(max_size)
       common    /fitdat/ x, y, a, s, x0, n1, n2, ngauss,
     *                    fix_a, fix_s, fix_x0
C
C columns on input data file
       integer    xc, yc
C
C Output unit
       integer    iout
       parameter (iout = 6)
C
C Local variables used in NAG fit
       integer    len_work
       parameter (len_work = (3*max_gauss+2)*3*max_gauss + 
     *                       (6*max_gauss+3)*max_size     )
       real*8     fsumsq, fit(3*max_gauss), work(len_work),
     *            ff(3,max_gauss)
       integer    nvals, ndata, iwork(100), ifail
C loop counter
       integer    n, nn
C file and directory name
       character*256  dir, file
C string
       character*3    string
       character*7    str
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
       call cmd_getiline( 'fitgauss',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)

C find input file name and allocate file
       call cmd_items( 'file', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id, status)

C read data
       call cmd_itemi( 'xc', 1, xc, status ) 
       call cmd_itemi( 'yc', 1, yc, status ) 
       call spec_get_data( id, xc, x, npts, status)
       call spec_get_data( id, yc, y, npts, status)
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
       n1 = npts
       n2 = 1
       do n = 1,npts
         if ( x(n).le.max(x1,x2) .and. x(n).ge.min(x1,x2) ) then
           n1 = min(n,n1)
           n2 = max(n,n2)
         endif
       enddo
C deallocate
       call spec_deallocate( id, status )

C sort out options adn read in initial values
       call cmd_itemi( 'ngauss', 1, ngauss, status )
       nvals = 0
       do n=1,ngauss
         string = ' '
         write(string,'(A1,I1)') 'a',n
         call cmd_itemr( string(1:2), 1, a(n), status )
         str = 'fix_'//string
         call cmd_itemset( str(1:6), test, status )
         if (test) then
           fix_a(n) = .true.
         else
           fix_a(n) = .false.
           nvals = nvals + 1
           fit(nvals) = a(n)
         endif

         write(string,'(A1,I1)') 's',n
         call cmd_itemr( string(1:2), 1, s(n), status )
         str = 'fix_'//string
         call cmd_itemset( str(1:6), test, status )
         if (test) then
           fix_s(n) = .true.
         else
           fix_s(n) = .false.
           nvals = nvals + 1
           fit(nvals) = s(n)
         endif

         write(string,'(A2,I1)') 'x0',n
         call cmd_itemr( string(1:3), 1, x0(n), status )
         str = 'fix_'//string
         call cmd_itemset( str(1:7), test, status )
         if (test) then
           fix_x0(n) = .true.
         else
           fix_x0(n) = .false.
           nvals = nvals + 1
           fit(nvals) = x0(n)
         endif
       enddo
       ndata = n2 - n1 + 1

C fit a multiple-gaussian model to the data
       ifail = 1
       if (nvals.ge.1) then
         call e04fdf(ndata, nvals, fit, fsumsq,
     *               iwork, 100, work, len_work, ifail)
         if (ifail.ne.0) then
           print *,'***(FITGAUSS) Ifail non-zero from NAG = ',ifail
           if (ifail.eq.1) goto 999
         endif
       endif
       write( iout, 100 ) ndata, fsumsq,  x1, x2
100    format(  1X/
     *          1X,'.. Results of fitting gaussians'/
     *          1X,'.. Number data points = ',I12,
     *          1X,'   Sum of squares     = ',1PE12.3/
     *          1X,'.. Data range:     x1 = ',1PE12.3,
     *          1X,'                   x2 = ',1PE12.3)
       nvals = 0
       do n=1,ngauss
         if (fix_a(n)) then
           ff(1,n) = a(n)
         else
           nvals = nvals + 1
           ff(1,n) = fit(nvals)
         endif
         if (fix_s(n)) then
           ff(2,n) = s(n)
         else
           nvals = nvals + 1
           ff(2,n) = fit(nvals)
         endif
         if (fix_x0(n)) then
           ff(3,n) = x0(n)
         else
           nvals = nvals + 1
           ff(3,n) = fit(nvals)
         endif
         write( iout, 110 ) n, (ff(nn,n), nn=1,3) 
110      format(1X/
     *          1X,'.. component ',i1,' A;sigma;x0 = ',
     *          1PE12.3,1PE12.3,1PE12.3)

       enddo

C construct output spectrum as a fit to these data
       call cmd_items('fit',file,status)
       call io_opefil( iunit, file(1:chr_lenb(file)), 'WRITE', 0,
     *                 status )
       write(iunit,*)'%ncols ',ngauss+2
       write(iunit,*)'%ndata ',npts
       do n=1,npts
          yfit(0) = 0.0
          do nn = 1,ngauss
             yfit(nn) = ff(1,nn) * 
     *                  exp(-(x(n)-ff(3,nn))**2/(2.0*ff(2,nn)**2))
             yfit(0) = yfit(0) + yfit(nn)
          enddo
          write(iunit,*) x(n), (yfit(nn), nn = 0,ngauss)
       enddo
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
       integer    max_size, max_gauss
       parameter (max_size = 2048, max_gauss = 6)
       real*4     x(max_size), y(max_size), yfit(0:max_gauss)
       real*8     ff(3,max_gauss)
       integer    n1, n2, ngauss
       real*4     a(max_gauss),
     *            s(max_gauss), x0(max_gauss) 
       logical    fix_a(max_gauss), fix_s(max_gauss),
     *            fix_x0(max_gauss) 
       common    /fitdat/ x, y, a, s, x0, n1, n2, ngauss,
     *                    fix_a, fix_s, fix_x0

       integer    n, nn, nf, nv
       nv = 0
       do n=1,ngauss
         if (fix_a(n)) then
           ff(1,n) = a(n)
         else
           nv = nv + 1
           ff(1,n) = fit(nv)
         endif
         if (fix_s(n)) then
           ff(2,n) = s(n)
         else
           nv = nv + 1
           ff(2,n) = fit(nv)
         endif
         if (fix_x0(n)) then
           ff(3,n) = x0(n)
         else
           nv = nv + 1
           ff(3,n) = fit(nv) 
         endif
       enddo
       nf = 0
       do n=n1,n2
          nf = nf + 1
          yfit(0) = 0.0
          do nn = 1,ngauss
             yfit(nn) = ff(1,nn) * 
     *                  exp(-(x(n)-ff(3,nn))**2/(2.0*ff(2,nn)**2))
             yfit(0) = yfit(0) + yfit(nn)
          enddo
          f(nf) = y(n) - yfit(0)
       end do
       nn = n1 - 1
       end
