*+ maxmin

       subroutine maxmin(map,m,n,i1,i2,j1,j2,xmax,imax,jmax,
     *                   xmin,imin,jmin,status)
C      -----------------------------------------------------
C
C find max and min on input map
C
C Input
C    MAP          -      R*4(array) -    map data
C    M,N          -      I4         -    range of map data
C    I1,I2        -      I4         -    range in I
C    J1,J2        -      I4         -    range in J
C
C Returned
C    XMAX         -      R4         -     maximum map-value in window
C    IMAX,JMAX    -      I4         -     max. position
C    XMIN         -      R4         -     minimum map-value in window
C    IMIN,JMIN    -      R4         -     min. position
C
C MAP(M,N) is examined in window I1-I2, J1-J2.
C XMAX is the maximum map-value in the window and is at IMAX,JMAX.
C XMIN is the minimum map-value in the window and is at IMIN,JMIN.
*-
       integer   status, m, n, i1, i2, j1, j2, imax, jmax, imin, jmin
       integer   i, j
       real*4    xmin, xmax
       real*4    x
       real*4    map(m,n)

       if (status.ne.0) return

       xmax=map(i1,j1)
       imax=i1
       jmax=j1
       xmin=xmax
       imin=imax
       jmin=jmax
       do j=j1,j2
         do i=i1,i2
           x=map(i,j)
           if(x.lt.xmin) then
             xmin=x
             imin=i
             jmin=j
           else if (x.gt.xmax) then
             xmax=x
             imax=i
             jmax=j
           end if
         end do
       end do
       end
