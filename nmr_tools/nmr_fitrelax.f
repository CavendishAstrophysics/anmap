C NMR: NMR_FITRELAX
C ----
C
C Fit a relaxation type curve to the supplied data
C
C Input:
C   - file for fitting
C Output:
C   - graphical output
C
C Notes:
C   The fitting assumes a relaxation curve of one of the following forms:
C   "opt_code"
C   ---------------------------------------------------------------------
C       0         A (1 - exp(-t/tr))
C       1         B + A (1 - exp(-t/tr))
C       2         A (1 - exp(-(t-t0)/tr))
C       3         B + A (1 - exp(-(t-t0)/tr))
C       4         A (1 - exp(-t/tr))/(1 - cos(theta)exp(-t/tr))
C       5         A (1 - exp(-t/tr))/(1 - cos(theta)exp(-t/tr))
C       6         B + A (1 - exp(-t/tr))/(1 - cos(theta)exp(-t/tr))
C       7         A (1 - 2 exp(-t/tr))
C   ---------------------------------------------------------------------
C   where the "A", "B" and "t0" are constants, tr is the relaxation time
C   and the final form is used for fitting progressive saturation
C   experiments.
C
C The following steps are taken depending on what data are supplied:
C  opt_code 0:
C   The user supplies estimates for A and tr which are then iterated
C   until a best fit is found.
C  opt_code 1:
C   As for 0, but the user supplies and initial estimate of B
C  opt_code 2:
C   As for 0, but the user supplies and initial estimate of t0
C  opt_code 3:
C   As for 0, but the user supplies and initial estimate of B & t0
C  opt_code 4:
C   The user explicitly requests fitting for progressive saturation data.
C   Initial estimates for A, tr and THETA must be supplied.
C  opt_code 5:
C   As for opt_code=4, but theta is not iterated.
C  opt_code 6:
C   As for opt_code=5, but another parameter, an offset B, is iterated.
C  opt_code 7:
C   Inversion recovery form for relaxation
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
C Estimates for relaxation curve
       real*4     A, B, tr, t0, theta
       integer    iset_A, iset_B, iset_tr, iset_t0, iset_theta
C options
       integer    opt_code
       integer    iprogsat, fixed_theta, invrec
C array for fitted data spectrum
       real*4     xfit(max_size), yfit(max_size), x_max
C data arrays passed in common
       real*4     x(max_size), y(max_size), xx, xx1, scale, step
       common    /fitdat/ x, y, scale, opt_code, theta
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
C test variable
       logical        test

C standard include files
       include '/mrao/include/chrlib_functions.inc'
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'

C initialise
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'nmr_fitrelax',
     *      dir(1:chr_lenb(dir))//anal_definitions, status)

C find input file name and allocate file
       call cmd_items( 'file', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id, status)
C read data
       call spec_get_data( id, 1, x, npts, status)
       call spec_get_data( id, 2, y, npts, status)
       scale = y(npts)
       x_max = -1.0E+30
       do n = 1,npts
         x_max = max(x_max,x(n))
       enddo
C deallocate
       call spec_deallocate( id, status )

C read in the control data
       call cmd_itemr( 'a', 1, A, status )
       call cmd_itemr( 'b', 1, B, status )
       call cmd_itemr( 'tr', 1, tr, status )
       call cmd_itemr( 't0', 1, t0, status )
       call cmd_itemr( 'theta', 1, theta, status )
       theta = theta*3.14159265/180.0
       call cmd_itemset( 'a', test, status )
       iset_A = 0
       if (test) iset_A = 1
       call cmd_itemset( 'b', test, status )
       iset_B = 0
       if (test) iset_B = 1
       call cmd_itemset( 'tr', test, status )
       iset_tr = 0
       if (test) iset_tr = 1
       call cmd_itemset( 't0', test, status )
       iset_t0 = 0
       if (test) iset_t0 = 1
       call cmd_itemset( 'theta', test, status )
       iset_theta = 0
       if (test) iset_theta = 1
       call cmd_itemset( 'progsat', test, status )
       iprogsat = 0
       if (test) iprogsat = 1
       call cmd_itemset( 'fixed_theta', test, status )
       fixed_theta = 0
       if (test) fixed_theta = 1
       call cmd_itemset( 'invrec', test, status )
       invrec = 0
       if (test) invrec = 1

C read in the data
       if ( status.ne.0 ) goto 999

C ensure zero values for offsets
       if (iset_B.eq.0)  B  = 0.0
       if (iset_t0.eq.0) t0 = 0.0

C sort out options
       fit(1) = A/scale
       fit(2) = tr
       nvals = 2
       opt_code = 0
       if (iset_B.eq.1 .and. iset_t0.eq.0 .and. iprogsat.eq.0) then
         fit(3) = B/scale
         nvals = 3
         opt_code = 1
       elseif (iset_B.eq.0 .and. iset_t0.eq.1 .and. iprogsat.eq.0) then
         fit(3) = t0
         nvals = 3
         opt_code = 2
       elseif (iset_B.eq.1 .and. iset_t0.eq.1 .and. iprogsat.eq.0) then
         fit(3) = B/scale
         fit(4) = t0
         nvals = 4
         opt_code = 3
       elseif (iprogsat.eq.1) then
         if (fixed_theta.eq.0) then
           fit(3) = theta
           nvals = 3
           opt_code = 4
         else
           nvals = 2
           opt_code = 5
           if (iset_B.eq.1) then
             fit(3) = B/scale
             nvals = 3
             opt_code = 6
           end if
         end if
       end if
       if (invrec.eq.1) then
         nvals = 2
         opt_code = 7
       end if

C fit a relaxation curve to the data
       ndata = npts
       ifail = 1
       call e04fdf(ndata, nvals, fit, fsumsq,
     *             iwork, 100, work, len_work, ifail)
       if (ifail.ne.0) then
         print *,'***(FITRELAX) Ifail non-zero from NAG = ',ifail
         if (ifail.eq.1) goto 999
       end if
       A = fit(1)*scale
       tr = fit(2)
       if (opt_code.eq.1) then
         B = fit(3) * scale
       else if (opt_code.eq.2) then
         t0 = fit(3)
       else if (opt_code.eq.3) then
         B = fit(3) * scale
         t0= fit(4)
       else if (iprogsat.eq.1) then
         if (fixed_theta.eq.0) then
           theta = fit(3)
         else if (opt_code.eq.6) then
           B = fit(3) * scale
         end if
       end if
       theta = theta/(3.14159265/180.0)
       if (iprogsat.eq.0) then
         write( iout, 100 ) ndata, fsumsq, A, tr, B, t0
100      format(1X/
     *          1X,'.. Results of fitting B + A(1-exp(-(t-t0)/tr))'/
     *          1X,'.. Number data points = ',I12,
     *          1X,'   Sum of squares     = ',1PE12.3/
     *          1X,'..                 A  = ',1PE12.3,
     *          1X,'   Relaxation time tr = ',1PE12.3/
     *          1X,'..                 B  = ',1PE12.3,
     *          1X,'                   t0 = ',1PE12.3)
       else
         write( iout, 110 ) ndata, fsumsq, A, tr, theta, B
110      format(1X/
     *          1X,'.. Results of fitting      A(1-exp(-t/tr))'/
     *          1X,'..                     ------------------------'/
     *          1X,'..                     (1-cos(theta)exp(-t/tr))'/
     *          1X,'.. Number data points = ',I12,
     *          1X,'   Sum of squares     = ',1PE12.3/
     *          1X,'..                 A  = ',1PE12.3,
     *          1X,'   Relaxation time tr = ',1PE12.3/
     *          1X,'..              theta = ',1PE12.3,
     *          1X,'                   B  = ',1PE12.3)
       end if

C construct output spectrum as a fit to these data
       xx1 = 0.0
       step = (1.1*x_max-xx1)/float(10*ndata-1)
       theta = theta*3.14159265/180.0
       call cmd_items('fit',file,status)
       call io_opefil( iunit, file(1:chr_lenb(file)), 'WRITE', 0,
     *                 status )
       write(iunit,*)'%ncols 2 '
       write(iunit,*)'%ndata ',10*ndata
       if (iprogsat.eq.0) then
         do n=1,10*ndata
           xx = xx1 + float(n-1)*step
           xfit(n) = xx
           if (invrec.eq.1) then
             yfit(n) = A*(1.0 - 2.0*exp(-xx/tr))
           else
             yfit(n) = B + A*(1.0 - exp(-(xx-t0)/tr))
           end if
           write(iunit,*) xfit(n), yfit(n)
         end do
       else
         do n=1,10*ndata
           xx = xx1 + float(n-1)*step
           xfit(n) = xx
           yfit(n) = B + A*(1.0 - exp(-xx/tr)) /
     *                  (1.0 - cos(theta)*exp(-xx/tr))
           write(iunit,*) xfit(n), yfit(n)
         end do
       end if
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
       integer    opt_code
       real*4     theta
       common    /fitdat/ x, y, scale, opt_code, theta

       integer    n, nn

       nn = 0
       if (opt_code.eq.0) then
         do n=1,ndata
            if (abs( x(n)/fit(2) ).lt.100.0) then
              f(n) = y(n)/scale -
     *               (fit(1)*(1.0D+0-exp(-(x(n))/fit(2))))
            else
              f(n) = y(n) - fit(1)
            end if
         end do
       else if (opt_code.eq.1) then
         do n=1,ndata
            if (abs( x(n)/fit(2) ).lt.100.0) then
              f(n) = y(n)/scale - fit(3) -
     *               (fit(1)*(1.0D+0-exp(-(x(n))/fit(2))))
            else
              f(n) = y(n) - fit(1) - fit(3)
            end if
         end do
       else if (opt_code.eq.2) then
         do n=1,ndata
            if (abs( (x(n)-fit(3))/fit(2) ).lt.100.0) then
              f(n) = y(n)/scale -
     *               (fit(1)*(1.0D+0-exp(-(x(n)-fit(3))/fit(2))))
            else
              f(n) = y(n) - fit(1)
            end if
         end do
       else if (opt_code.eq.3) then
         do n=1,ndata
            if (abs( (x(n)-fit(4))/fit(2) ).lt.100.0) then
              f(n) = y(n)/scale - fit(3) -
     *               (fit(1)*(1.0D+0-exp(-(x(n)-fit(4))/fit(2))))
            else
              f(n) = y(n) - fit(1) - fit(3)
            end if
         end do
       else if (opt_code.eq.4) then
         do n=1,ndata
            if (abs( x(n)/fit(2) ).lt.100.0) then
              f(n) = y(n)/scale -
     *               (fit(1)*(1.0D+0-exp(-(x(n))/fit(2))))/
     *               (1.0D+0-cos(fit(3))*exp(-(x(n))/fit(2)))
            else
              f(n) = y(n) - fit(1)
            end if
         end do
       else if (opt_code.eq.5) then
         do n=1,ndata
            if (abs( x(n)/fit(2) ).lt.100.0) then
              f(n) = y(n)/scale -
     *               (fit(1)*(1.0D+0-exp(-(x(n))/fit(2))))/
     *               (1.0D+0-cos(theta)*exp(-(x(n))/fit(2)))
            else
              f(n) = y(n) - fit(1)
            end if
         end do
       else if (opt_code.eq.6) then
         do n=1,ndata
            if (abs( x(n)/fit(2) ).lt.100.0) then
              f(n) = y(n)/scale - fit(3) -
     *               (fit(1)*(1.0D+0-exp(-(x(n))/fit(2))))/
     *               (1.0D+0-cos(theta)*exp(-(x(n))/fit(2)))
            else
              f(n) = y(n) - fit(1) - fit(3)
            end if
         end do
       elseif (opt_code.eq.7) then
         do n=1,ndata
            if (abs( x(n)/fit(2) ).lt.100.0) then
              f(n) = y(n)/scale -
     *               (fit(1)*(1.0D+0-2.0*exp(-(x(n))/fit(2))))
            else
              f(n) = y(n) - fit(1)
            end if
         end do
       end if
       end
