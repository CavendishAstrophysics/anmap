C
C
C
C
      real*4 function image_polcorr( pmeas, s )
C     -----------------------------------------
C
C Perform a correction for ricean bias of polarized flux density
C
C Given:
C  measured flux density
       real*4    pmeas
C Updated:
C  error status
       integer   s
C
C KMB/PA 4/6/93
C-
       real*8      mmin, mmax, register(10), ptrue, temp,
     *             centre, range
       complex*16  z, cy
       integer     i, j, nz, ifail, mark

       if (s.ne.0) return

C do nothing for large SNR case
       if (pmeas.gt.10.0) then
         image_polcorr = pmeas
         return
       endif

C calculate correction
       mmin = 0.0d0
       mmax = 10.0d0
       do i = 1, 4
         do j = 1, 10
            ptrue = mmin + (mmax - mmin)*dble(j-1)/dble(10-1)
            z = ptrue*pmeas*(0.0d0,1.0d0)
            ifail = 0
            call s17def(0.0D+0,z,1,'U',cy,nz,ifail)
            register(j) = pmeas*cy*exp(-(pmeas**2 + ptrue**2)/2.0d0)
          enddo
          temp = 0.0D+0
          do j=1,10
            if (register(j).gt.temp) then
               temp = register(j)
               mark = j
            endif
          enddo
          centre = mmin + (mmax - mmin)*dble(mark-1)/dble(10-1)
          range = (mmax - mmin)/10.0d0
          mmin = centre - 0.5d0*range
          if (mmin.lt.0.0d0) then
              mmin = 0.0d0
          endif
          mmax = centre + 0.5d0*range
       enddo
       image_polcorr = mmin + (mmax - mmin)*dble(mark-1)/dble(10-1)
       end
