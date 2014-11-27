C
C
*+ make_clbm

      subroutine make_clbm(beam,m,n,cbsig1,cbsig2,a,bmax,ibc,jbc,i1,i2,
     +                     j1,j2,sratio,status)
C     -----------------------------------------------------------------
C
C Calculate the clean beam for the restore process
C
C Input
C   M,N       -        I4       -      size beam
C   CBSIG1/2  -        R4       -      beam size (grid-points)
C   A         -        R4       -      position-angle (radians)
C   BMAX      -        R4       -      beam height
C
C Returned
C   BEAM      -        R4(array)-      beam array
C   I/JBC     -        I4       -      beam centre
C   I1,I2     -        I4       -      region on beam to use (I)
C   J1,J2     -        I4       -                "           (J)
C   SRATIO    -        R4       -      beam normalization factor
C
C This routine calculates the clean beam for the restoring process.
C The beam is made to be gaussian, and circular in the aperture
C projection.  CBSIG1,2 are the values of sigma for this beam, and are
C in units of grid-points.
C
C Also, the routine calculates the factor by which sums of numbers on a CLEAN
C map have to be multiplied in order to get the flux density (SRATIO).
C
*-
      integer     status, m, n, i1, i2, j1, j2, ibc, jbc
      real*4      beam(m,n), a, bmax, cbsig1, cbsig2, sratio
      real*4      cb1, cb2, x1, x2, bsum
      integer     na1, na2, i, j

C calculate geometrical quantities for the gaussian
      ibc = n/2
      jbc = m/2

      cb1=1.0/(cbsig1*cbsig1*2.0)
      cb2=1.0/(cbsig2*cbsig2*2.0)
      na1=6.324*cbsig1+2.5
      na2=6.324*cbsig2+2.5
      i1=ibc-max(na1,na2)
      i2=ibc+max(na1,na2)
      j1=jbc-max(na2,na1)
      j2=jbc+max(na2,na1)
C zero old array
      do j=1,n
        do i=1,m
          beam(i,j)=0.0
        end do
      end do

C Calculate beam
      bsum=0.0
      do j=j1,j2
        do i=i1,i2
          x1 = float(i-ibc)*cos(A) + float(j-jbc)*sin(A)
          x2 = float(j-jbc)*cos(A) - float(i-ibc)*sin(A)
          x1 = x1 * x1 * cb1
          x2 = x2 * x2 * cb2
          beam(i,j) = bmax*exp(-x1-x2)
          bsum=bsum+beam(i,j)
        end do
      end do

C determine normalization factor
      sratio=bmax/bsum

      end
