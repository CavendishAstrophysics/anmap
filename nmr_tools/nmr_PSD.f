C NMR_PSD: Determine an NMR pore-size distribution from $T_{1}$ data
C --------
C
C NMR relaxation data are used to determine a PSD using a method based on
C Munn and Smith [J. Colloid and Interface Sci., 119, 117, 1987]. In the
C Munn and Smith algorithm it was necessary to know M0 (magnetization as
C t --> infinity); this restriction is not necessary in the formulation
C employed in this program and therefore data not sampled to where the
C magnetization levels off at large t may be fitted.
C
C The problem is the solution of the 1st-order Fredholm equation:
C
C   m(t) = M(t))/M0 = \int_{Tmin}^{Tmax} (1 - 2 exp(-t/T)) f(T) dT
C
C where 1/T = 1/T1(bulk) + gamma/( r*T1(surface) ) and f(T) is therefore
C directly related to the pore-size distribution for r.  The relationship
C is in fact parameterized by:
C
C   1/T = alpha + beta/r
C
C The method of solution is to discretize the above integral to yield:
C
C   m(i) = K(i,j) f(j)
C
C   j = 1, NJ
C   i = 1, NI
C
C where m(i) = m(ti); K(i,j) = (1-2 exp(-ti/Tj)) dT; f(j) = f(Tj).  The step
C dT is such that dT = (Tmax-Tmin) / (NJ - 1) and Tj = Tmin + (j-1)*dT.
C
C The additional criterion on f(j) is that f(j) >= 0 for all j, therefore
C the solution to the above matrix equation is formed in terms of minimizing
C the sum, S:
C
C   S = sum( i=1,NI ) sum( j=1,NJ ) [ m(i) - K(i,j) abs(f(j)) ]
C
C This is done using the NAG routine E04FDF.
C
C
C The user must supply the following:
C  Number of data points and data values at the time steps
C    NI, M(i), t(i)
C  The range in T and number of steps
C    Tmin, Tmax, NJ (NJ < NI)
C  The parameters of the T <==> T1 transformation
C    alpha, beta
C  Estimates of
C    M0, T1 (single value)
C
C-

C define experimental data
       integer     max_NI
       parameter  (max_NI = 100)
       integer     NI, i
       real*4      x(max_NI), y(max_NI), xx, yy
       real*8      m(max_NI), t(max_NI)
C .. initial guess of mean T1 and m normalization
       real*8      guess_T1, M0
C define steps
       integer     max_NJ
       parameter  (max_NJ = 20)
       integer     NJ, j
       real*8      f(max_NJ), TT(max_NJ), Tmin, Tmax, dT,
     *             g(max_NJ), rr(max_NJ)
C define matrix
       real*8      K(max_NI,max_NJ)
C details of the transformation
       real*8      alpha, beta
C arrays used in the minimization
       integer     max_work, max_iwork
       parameter  (max_work = 7*max_NJ + 2*max_NJ*max_NJ +
     *                        2*max_NJ*max_NI + 3*max_NI )
       parameter  (max_iwork = 500)
       real*8      fsumsq, work( max_work )
       integer     iwork( max_iwork ), redo_count

C file name character string
       character   file*256, dir*256
C file unit number
       integer     iunit, id
C error status
       integer     status, ifail
C local R*4 variables
       real*4      rm0, rt1, rtmin, rtmax, ralpha, rbeta

C local arrays passed in common
       common /local_data/ m, t, K, TT

C standard include files
       include '/mrao/include/chrlib_functions.inc'
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'

C initialise
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'nmr_PSD',
     *      dir(1:chr_lenb(dir))//anal_definitions, status)

C find input file name and allocate file
       call cmd_items( 'file', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id, status)

C report status
       print *,' '
       print *,'PSD Version 2.0 file: ',file(1:chr_lenb(file))

C read data
       call spec_get_data( id, 1, x, NI, status)
       call spec_get_data( id, 2, y, NI, status)
C deallocate
       call spec_deallocate( id, status )

C read in the control data
       call cmd_itemi( 'nj', 1, nj, status )
       call cmd_itemr( 'tmin', 1, rtmin, status )
       Tmin = rtmin
       call cmd_itemr( 'tmax', 1, rtmax, status )
       Tmax = rtmax
       call cmd_itemr( 'm0', 1, rm0, status )
       M0 = rm0
       call cmd_itemr( 'alpha', 1, ralpha, status )
       alpha = ralpha
       call cmd_itemr( 'beta', 1, rbeta, status )
       beta = rbeta
       call cmd_itemr( 't1', 1, rt1, status )
       guess_T1 = rt1

C modify input data to normalize
       do i=1,NI
         t(i) = x(i)
         m(i) = y(i) / M0
       end do

C setup the TT array
       dT = (Tmax - Tmin) / float( NJ-1 )
       do j=1,NJ
         TT(j) = Tmin + float(j-1)*dT
       end do

C setup the matrix
       do i=1,NI
         do j=1,NJ
           K(i,j) = ( 1.0 - 2.0*exp( -t(i)/TT(j) ) ) * dT
         end do
       end do

C setup initial guess for f array
       do j=1,NJ
         f(j) = 0.0
       end do
       j = nint( (guess_T1-Tmin)/dT + 1.0 )
       f(j) = 1.0

C do the minimization, looping if a better solution could be found
       redo_count = 0
1      ifail = 1
       redo_count = redo_count + 1
       call e04fdf( NI, NJ, f, fsumsq,
     *              iwork, max_iwork, work, max_work, ifail )
       if (ifail.eq.2 .and. redo_count.lt.10) goto 1

C report results
       call io_enqout( iunit )
       write( iunit, 10 ) ifail, redo_count, fsumsq
10     format( ' On exit IFAIL  = ',I2,'  E04FDF called NTIMES = ',I2 /
     *         ' Sum-of-squares = ',1PD12.3 / )

C write output
       call cmd_items( 'psd', file, status)
       call io_opefil( iunit, file(1:chr_lenb(file)),
     *                 'WRITE', 0, status )
       write(iunit,*) '\ncols 4'
       write(iunit,*) '\ndata ',NJ
       do j=1,NJ
         rr(j) = beta/( (1.0/TT(j)) - alpha )
         g(j) = M0 * abs(f(j))* (beta*TT(j)*TT(J)) / (rr(j)*rr(j))
         write(iunit,*) rr(j), g(j), TT(j), M0*abs(f(j))
       end do
       close (iunit)

C write fit
       call cmd_items( 'fit', file, status)
       call io_opefil( iunit, file(1:chr_lenb(file)),
     *                 'WRITE', 0, status )
       write(iunit,*) '\ncols 2'
       write(iunit,*) '\ndata ',5*NI
       do i=1,5*NI
         xx = t(1) + float(i-1)*(t(NI)-t(1))/float(5*NI-1)
         yy = 0.0
         do j=1,NJ
           yy = yy + abs(f(j)) * (1.0 - 2.0 * exp(-xx/TT(j)) ) * dT
         end do
         yy = M0*yy
         write(iunit,*) xx, yy
       end do
       close (iunit)

       end
C
C
       subroutine LSFUN1( NI, NJ, f, df )
C      ----------------------------------
C
C Supplied routine to calculate residuals
       integer     NI, NJ
       real*8      f(NJ), df(NI)

       integer     max_NI
       parameter  (max_NI = 100)
       integer     i
       real*8      m(max_NI), t(max_NI)
C define steps
       integer     max_NJ
       parameter  (max_NJ = 20)
       integer     j
       real*8      TT(max_NJ)
C define matrix
       real*8      K(max_NI,max_NJ)
C local arrays passed in common
       common /local_data/ m, t, K, TT

C local variables
       real*8      tot

       do i=1,NI
         tot = 0.0D+0
         do j=1,NJ
           tot = tot + K(i,j)*abs(f(j))
         end do
         df(i) = (m(i) - tot)
       end do
       end
