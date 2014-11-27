C
C
*+ noise_map

       subroutine noise_map(map,n,m,i1,i2,j1,j2,xmean,xsd,npts,status)
C      ---------------------------------------------------------------
C
C Calculate the mean and noise in specified map window
C
C Input
C    MAP          -      R*4(array) -    map data
C    M,N          -      I4         -    range of map data
C    I1,I2        -      I4         -    range in I
C    J1,J2        -      I4         -    range in J
C
C Returned
C    XMEAN        -      R4         -    mean in window
C    XSD          -      R4         -    standard deviation in window
C    NPTS         -      I4         -    number of points in window
C
C  This routine calculates the mean and noise in the window I1-I2,J1-J2
C  of the input map.  No rejection is used.
*-
       integer    status, m, n, i1, i2, j1, j2, i, j, npts
       real*4     map(m,n), xmean, xsd, x
       real*4     xsum1, xsum2, fn

       xsum1=0.0
       xsum2=0.0
       npts=0
       do j=j1,j2
         do i=i1,i2
           x=map(i,j)
           xsum1=xsum1+x
           xsum2=xsum2+x*x
           npts=npts+1
         end do
       end do

C calculate statistics
       fn=float(npts)
       xmean=xsum1/fn
       xsd=sqrt((xsum2-xmean*xsum1)/(fn-1.0))
       end
