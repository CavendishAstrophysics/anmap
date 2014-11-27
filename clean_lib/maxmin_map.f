C
C
*+ maxmin_map

       subroutine maxmin_map(map,m,n,i1,i2,j1,j2,xmax,imax,jmax,
     *                       xmin,imin,jmin,status)
C      ---------------------------------------------------------
C
C Scan a map and update XMAX and XMIN if appropriate
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
C The map is scanned and each map value tested against XMAX and XMIN.
C XMAX, IMAX, JMAX and XMIN, IMIN, JMIN are updtades if required, the
C routine requires that XMAX and XMIN should be set to either limiting
C values (if say the region is to be tested alone), or already set to
C max, min values for another region.
C
C The calling routine MUST set XMAX, XMIN before a call.
*-
       integer     status, m, n, i1, i2, j1, j2, imin, jmin, imax, jmax
       integer     i, j
       real*4      x, xmin, xmax
       real*4      map(m,n)

       if (status.ne.0) return

       do j=j1,j2
         do i=i1,i2
           x=map(i,j)
           if(x.lt.xmin)then
             xmin=x
             imin=i
             jmin=j
           end if
           if(x.gt.xmax)then
             xmax=x
             imax=i
             jmax=j
           end if
         end do
       end do
       end
