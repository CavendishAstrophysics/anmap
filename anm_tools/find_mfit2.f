*+ find_mfit

       subroutine find_mfit( nn, xd, yd, nc, cd, model, res, status )
C      --------------------------------------------------------------
C
C Perform the fitting
C
C Given:
C  number of data points
       integer    nn
C  input "x" and "y" data
       real*4     xd(*), yd(*)
C  number of user-specified constants
       integer    nc
C  user-supplied constants
       real*4     cd(*)
C  type of fitting model to use
       integer    model
C Returned:
C  results of the fit
       real*4     res(*)
C Updated:
C  return code
       integer    status
C
C Perform one of number of different fitting models:
C
C model   Fit-decription
C -------------------------------------------------------------------
C   1     y = res(1) exp( -x/res(2) )
C   2     y = res(1) [ 1 - exp( -x/res(2) ) ]
C   3     y = res(1) exp( -res(2) [cd(1) x**2 - cd(2) x**3] )
C   4     y = res(1) + res(2) x
C   5     y = res(1) + res(2) [ cd(1)x + cd(2)x**2 + cd(3)x**3 ]
C -------------------------------------------------------------------
C
C The fit is performed using a least-squares minimization using the
C NAG routine E04FDF.
C
C P.Alexander MRAO, 06/04/92
*-

C local variables
       integer     len_work, len_iwork
       parameter  (len_work=1000, len_iwork=100)
       real*8      xx(5), fsumsq, work(len_work)
       integer     iwork(len_iwork), nvar, istat

C counters etc.
       integer     n

C data passed in common to LSFUN1
       real*8      x(50), y(50), c(10)
       integer     itype, ndata, nconst
       common /local_fitr8/ x, y, c
       common /local_fiti4/ itype, ndata, nconst

       if (status.ne.0) return

C set itype
       if (model.le.0) then
         itype = 1
       else if (model.gt.5) then
         itype = 3
       else
         itype = model
       end if
C copy data
       ndata = nn
       nconst = nc
       do n=1,nn
         x(n) = xd(n)
         y(n) = yd(n)
       end do
       do n=1,nc
         c(n) = cd(n)
       end do

C call minimization routine
       nvar = 2
       istat = 1
       xx(1) = yd(1)
       xx(2) = xd(2) - xd(1)
       call e04fdf( ndata, nvar, xx, fsumsq,
     *              iwork, len_iwork, work, len_work, istat )
       do n=1,nvar
         res(n) = xx(n)
       end do
       res(1) = res(1)

       end
C
C
       subroutine lsfun1( m, n, xx, f )
       integer    n, m, i
       real*8     xx(n), f(m)
C data passed in common to LSFUN1
       real*8      x(50), y(50), c(10)
       integer     itype, ndata, nconst
       common /local_fitr8/ x, y, c
       common /local_fiti4/ itype, ndata, nconst

       if (itype.eq.1) then
         do i = 1,ndata
           f(i) = xx(1)*exp(-x(i)/xx(2)) - y(i)
         end do
       elseif (itype.eq.2) then
         do i = 1,ndata
           f(i) = xx(1)*(1.0D+0 - exp(-x(i)/xx(2))) - y(i)
         end do
       elseif (itype.eq.3) then
         do i = 1,ndata
           f(i) = xx(1)*exp(-xx(2)*(c(1)*(x(i)**2)-c(2)*(x(i)**3) ))
     *            - y(i)
         end do
       elseif (itype.eq.4) then
         do i = 1,ndata
           f(i) = xx(1) + xx(2)*x(i) - y(i)
         end do
       elseif (itype.eq.5) then
         do i = 1,ndata
           f(i) = xx(1) +
     *            xx(2)*(c(1)*x(i) + c(2)*(x(i)**2) + c(3)*(x(i)**3))
     *            - y(i)
         end do
       endif
       end
