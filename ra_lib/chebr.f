C
C
       subroutine chebr(x,y,npts,kp1,ak)
C      ---------------------------------
C
C Routine to parameterize the function represented by X and Y
C
C Given:
C   data arrays
       real*8     x(*), y(*)
C   number of points
       integer    npts
C   fitting depth
       integer    kp1
C
C Returned:
C   fitted coefficients   
       real*8     ak(*)
C
C-
       integer    nptmax, kp1max, nrows, ifail, k
       parameter (nptmax=2000, kp1max=100)
       real*8     a(kp1max,kp1max), work1(3,nptmax), work2(2,kp1max)
       real*8     s(kp1max), w(nptmax)
       data       w /nptmax*1.0d+0/

       if (npts.gt.nptmax) then
        print *,'***(CHEBR) Error NPTS exceeds max VALUE = ',npts
        print *,'***(CHEBR) Maximum Allowed = NPTMAX = ',nptmax
        return
       end if
       if (kp1.gt.kp1max) then
         print *,'***(CHEBR) Maximum Fitting depth exceeded'
         print *,'***(CHEBR) Maximum = ',kp1max,'  VALUE = ',kp1
         return
       end if
       nrows=kp1max
       IFAIL=1
       call e02adf(npts,kp1,nrows,x,y,w,work1,work2,a,s,ifail)
       if (ifail.ne.0) then
         print *,'***(CHEBR) IFAIL non-zero NAG routine Error'
         print *,'***(CHEBR) IFAIL = ',ifail,'  in E02ADF'
         return
       end if
       do k=1,kp1
        ak(k)=a(kp1,k)
       end do
       end
