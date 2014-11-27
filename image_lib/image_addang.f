C
C
*+ image_addang

      subroutine image_addang(minirt,data,uv,centre,n_angle,r_1,r_2,
     *                        gate,flux,ndata,frac_area,status      )
C     --------------------------------------------------------------
C
C To calculate the average flux in elliptical annuli
C
C Given:
C   mini redtape of image
       integer     minirt(8)
C   image data
       real*4      data(*)
C   range on image to analyse
       integer     uv(4)
C   centre of circular region
       real*4      centre(2)
C   number of angular regions
       integer     n_angle
C   minimum and maximum angular extents
       real*4      r_1, r_2
C   value above which averaging is performed
       real*4      gate
C Returned:
C   results for each annulus
C     flux
       real*4      flux(n_angle)
C     number of data points
       integer     ndata(n_angle)
C     fraction of correct annular area found by cell averaging
       real*4      frac_area(n_angle)
C   error return code
       integer     status
C
C The distribution of flux on an image centred at a given location
C and in angular bins between radii r_1 and r_2 is determined.
C
C Local variables
C  counters and sums
      integer      iu, iv, n, uv_point(2)
      equivalence (uv_point(1), iu)
      equivalence (uv_point(2), iv)
      real*4       x, y, value, rsq, areaconst, a
      real*4       r2_1, r2_2
C  constant pi
      real*4       pi
      data         pi/3.14159265/

C check status on entry
      if (status.ne.0) return

C setup regions
      r2_1 = r_1**2
      r2_2 = r_2**2

C initialise arrays
      do n = 1,n_angle
        flux(n) = 0.0
        ndata(n) = 0
      end do

      do iv = uv(3),uv(4),-1
        do iu = uv(1),uv(2)
C .. find values
          call iuvval2(minirt,data,uv_point,value,status)
C .. find bin (n) containing point (x,y)
          x = float(iu) - centre(1)
          y = float(iv) - centre(2)
          rsq = x**2 + y**2
          if ( (rsq.ge.r2_1) .and. (rsq.le.r2_2)) then
             a = atan2(y,x)
             if (a.lt.0.0) then
                a = 2.0*pi + a
             endif
             n = (a/(2.0*pi)) * n_angle + 1
C .. add flux to correct bin
             if ((n.le.n_angle) .and. (abs(value).gt.gate)) then
               flux(n) = flux(n) + value
               ndata(n) = ndata(n) + 1
             end if
          endif
        end do
      end do

C normalize and calculate fractional areas
      areaconst = pi*(r2_2 - r2_1) / float( n_angle )
      do n = 1,n_angle
        if (ndata(n) .gt. 0 ) then
          flux(n) = flux(n)/float(ndata(n))
        else
          flux(n) = 0.0
        end if
        frac_area(n) = ndata(n)/areaconst
      end do
      call cmd_err(status,'image_addang',' ')
      end
