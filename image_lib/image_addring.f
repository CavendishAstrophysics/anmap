C
C
*+ image_addring

      subroutine image_addring(minirt,data,uv,ellipse_centre,
     *                         ellipse_theta, ellipse_eccen,
     *                         n_annuli, r_annuli,
     *                         flux,ndata,frac_area,status   )
C     --------------------------------------------------------
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
C   centre of ellipse
       real*4      ellipse_centre(2)
C    eccentricity and position-angle of ellipse
       real*4      ellipse_theta, ellipse_eccen
C   number of annuli
       integer     n_annuli
C   spacing of annuli along major axis
       real*4      r_annuli
C   value above which averaging is performed
       real*4      gate
C Returned:
C   results for each annulus
C     flux
       real*4      flux(n_annuli)
C     number of data points
       integer     ndata(n_annuli)
C     fraction of correct annular arear foundd by cell averaging
       real*4      frac_area(n_annuli)
C   error return code
       integer     status
C
C The distribution of flux on an image centred at a given location
C within elliptical annuli is calculated according to the following
C algorithm.
C
C Divide image into elliptical bins and evaluate the mean flux density
C within each bin.  All the flux within a cell contributes to the bin
C enclosing the centre of that cell.  Two quantities are provided to
C estimate the usefulness of the output; the number of cells which have
C been averaged per bin; and the fraction of the correct area which has
C been evaluated by these points per bin.  Data values .le. the level
C 'gate' are not counted as valid data points
*-
C [A.J. Fitt, MRAO, Cambridge. June 1988.]
C [to NORD and ANMAP by P. Alexander, MRAO, Cambridge. June 1988.]
C [improved interface and standard coordinated, PA. 4/4/91]
C
C Local variables
C  coordinate along ellipse
       real*4      a, b
C  counters and sums
      integer      iu, iv, n, uv_point(2)
      equivalence (uv_point(1), iu)
      equivalence (uv_point(2), iv)
      real*4       x, y, value, rsq, areaconst, annarea
      real*4       sin_th, cos_th
C  constant pi
      real*4       pi
      data         pi/3.14159265/

C check status on entry
      if (status.ne.0) return

C calculate geometric values
      sin_th = sin(ellipse_theta)
      cos_th = cos(ellipse_theta)

C initialise arrays
      do n = 1,n_annuli
        flux(n) = 0.0
        ndata(n) = 0
      end do
      gate = 0.0
      do iv = uv(3),uv(4),-1
        do iu = uv(1),uv(2)
C .. find values
          call iuvval2(minirt,data,uv_point,value,status)
C .. find bin (n) containing point (x,y)
          x = float(iu) - ellipse_centre(1)
          y = float(iv) - ellipse_centre(2)
          a = -x*sin_th + y*cos_th
          b = x*cos_th + y*sin_th
          rsq = a**2 + (b*ellipse_eccen)**2
          n = sqrt(rsq)/r_annuli + 1.0
C .. add flux to correct bin
          if ((n.le.n_annuli) .and. (abs(value).gt.gate)) then
            flux(n) = flux(n) + value
            ndata(n) = ndata(n) + 1
          end if
        end do
      end do

C normalize and calculate fractional areas
      areaconst = pi*(r_annuli**2)/ellipse_eccen
      do n = 1,n_annuli
        if (ndata(n) .gt. 0 ) then
          flux(n) = flux(n)/float(ndata(n))
        else
          flux(n) = 0.0
        end if
        annarea = areaconst*float(2*n-1)
        frac_area(n) = ndata(n)/annarea
      end do
      call cmd_err(status,'image_addring',' ')
      end
