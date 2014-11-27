C
C
*+ make_trbm

       subroutine make_trbm(beam,m,n,bmax,ibc,jbc,i1,i2,j1,j2,
     *                       sratio,status)
C      -------------------------------------------------------
C
C Make a restoring beam truncated at first zero.
C
C Input
C   M,N       -        I4       -      size beam
C   CBSIG1/2  -        R4       -      beam size (grid-points)
C   BMAX      -        R4       -      beam height
C   I/JBC     -        I4       -      beam centre
C   I1,I2     -        I4       -      region on beam to use (I)
C   J1,J2     -        I4       -                "           (J)
C
C Input/Returned
C   BEAM      -        R4(array)-      beam array
C
C Returned
C   SRATIO    -        R4       -      beam normalization factor
C
C This routine truncates the old dirty beam at the first zero.
C The beam outside the first zero is set equal to zero.
*-
       integer     status, m, n, i1, i2, j1, j2, ibc, jbc
       real*4      beam(1-ibc:m-ibc,1-jbc:n-jbc)
       real*4      bmax, sratio, bsum

       integer     i, ii, j, imax, imin, jmax, jmin
       integer     ib1, jb1, jb2

       if (status.ne.0) return

C  Assume elliptic beam to first order.
       imax=m-ibc
       imin=1-ibc
       jmax=n-jbc
       jmin=1-jbc

       do 10 j=0,jmax
         do i=0,imax
           if (beam(i,j) .le. 0) then
             do ii=i,imax
               beam(ii,j) = 0.0
               beam(ii,-j)= 0.0
             end do
             do ii= -i,imin,-1
               beam(ii,j) = 0.0
               beam(ii,-j)= 0.0
             end do
             goto 10
           end if
         end do
 10    continue

       bsum = 0.0
       ib1  = imin
       jb1  = jmin
       do j=jmin,jmax
         do i=imin,imax
           if (beam(i,j) .gt. 0.0) then
             bsum = bsum + beam(i,j)
             if(i.gt.ib1) ib1 = i
             if(j.gt.jb1) jb1 = j
           end if
         end do
       end do
       i1 = ibc - ib1
       i2 = ibc + ib1
       j1 = jbc - jb1
       j2 = jbc + jb2
       sratio = bsum / bmax

       end
