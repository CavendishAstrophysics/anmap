C Segment-flux
C   calculate average flux in annuli bounded by a circular segment
C
C Document origin and update of this routine
C   P. Alexander, MRAO, Cambridge, 1/11/93
C
*-

C define work array
       integer       nm, nb
       integer        msize, bsize
       parameter    (msize = 256*256)
       parameter    (bsize=1024)
       parameter    (nm=16)
       parameter    (nb=3)
       real*4        map_array( nm*msize + nb*bsize )
C error status
       integer       status

C perform standard initialization
       call anm_start( 0,nm,msize,nb,bsize,status )

C read input map: imap is the catalogue identifier
       call do_segment_flux(map_array,status)

C check status value and report an error to the user
       call cmd_err(status,'SEGMENT-FLUX','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end
C
C
*+ do_segment_flux

       subroutine do_segment_flux(map_array,status)
C      -------------------------------------------------
C
C Add flux in elliptical annuli as defined by the user
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The flux is calculated in annuli bounded by a specified segment.
C The annuli are defined by an eccentricity, position-angle of the
C major axis and a width of the annulus.
C
C Output is to the standard output file which may be plotted or printed
C as required.
*-

       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'

C Local variables:
C   mini redtape
       integer    minirt(8)
C   used to hold the date (day, month, year)
       integer    idate(3)
C   input map and data pointer
       integer    imap, ip_map
C   results output variables
       integer    iout, n
       real*4     mean_radius
       integer    uv_range(4), number_annuli
       real*4     uv_centre(2)
C   radius of annulus (arcsec), position angle of ellipse (degrees),
C   eccentricity of ellipse
       real*4     radius_annulus, position_angle, eccentricity
C .. and in pixel coordintes / radians
       real*4     pix_rad_annulus, rad_pos_angle
C .. bounding angles of segment (degrees)
       real*4     seg_a1, seg_a2
C   maximum number of annuli the user may specify,  flux in each region,
C   error in each region, number of points found in each annulus
       integer    max_annuli
       parameter (max_annuli = 100)
       integer    number_points(max_annuli)
       real*4     average_flux(max_annuli), area_error(max_annuli)
C   redtape infortion used locally
       integer    equatorial_projection
       parameter (equatorial_projection = 1)
C   projection information
       integer    iproj, data_type
       real*8     usamp, skew, epoch, prang
C   source name and program name
       character  source*40, program*8

C check status
       if (status.ne.0) return

C read in the map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call mapcat_enqsr(imap,source,program,status)
       if (status.ne.0) then
         call cmd_err(status,'SEGMENT-FLUX',' ')
         goto 999
       end if

C check that this image is a sky-plane image
       call enmapj(iproj,usamp,skew,epoch,prang,status)
       call enminirt(minirt,status)
       if (iproj.eq.equatorial_projection) then
         call cmd_wrerr('RING-FLUX',
     *             'Unable to work on EQUATORIAL-PLANE projection')
         call cmd_wrerr('SEGMENT-FLUX','Use REPROJECT-MAP')
         goto 999
       end if

C find user defined parameters
       call io_geti('Number of annuli : ','20',number_annuli,status)
       if (number_annuli.gt.max_annuli) then
         call cmd_wrerr('SEGMENT-FLUX','Too many annuli > 100')
         goto 999
       end if
       radius_annulus = usamp*1.0
       call io_getr('Radius of annulus (arcsec) : ','*',radius_annulus,
     *           status)
C .. convert to pixels
       pix_rad_annulus = radius_annulus/usamp

C find uv-range to analyse and centre of ellipse
       call enredt(uv_range,data_type,status)
       call plot_getuv('UV-range : ','*',uv_range,status)
       call plot_getpos2(minirt,'Centre of Ellipse : ','*',
     *                   uv_centre,status)

C read eccentricity and position angle of the major axis
       call io_getr('Eccentricity of ellipse: ','1.0',
     *               eccentricity,status)
       call io_getr('Position-angle (N thro'' E) : ','0.0',
     *           position_angle,status)
       call io_getr('Lower-segment-angle (+ve from X-axis, degrees) : ',
     *              '0.0',seg_a1,status)
       call io_getr('Upper-segment-angle (+ve from X-axis, degrees) : ',
     *              '360.0',seg_a2,status)
       if (seg_a1.lt.0.0) seg_a1 = seg_a1 + 360.0
       if (seg_a2.lt.0.0) seg_a2 = seg_a2 + 360.0
C .. convert to radians
       rad_pos_angle = position_angle*3.14159265/180.0

C do the work
      call image_addseg(minirt,map_array(ip_map),uv_range,uv_centre,
     *                   rad_pos_angle,eccentricity,
     *                   seg_a1, seg_a2,
     *                   number_annuli, pix_rad_annulus,
     *                   average_flux,number_points,area_error,status)
C
C write this out to the results file
       call io_opefil(iout,general_results_file,'WRITE',0,status)
       if (status.ne.0) then
         call cmd_err(status,'SEGMENT-FLUX',
     *                 'Error accessing results file')
         goto 999
       end if
       call util_enqdat(idate)
       write (iout,*)'%ndata ',number_annuli
       write (iout,*)'%ncols 4'
       write (iout,*)'%title RING FLUX output on ',idate
       write (iout,*)'%source ',source(1:chr_lenb(source))
       write (iout,*)'%title-1 Mean Radius of Annulus (arcsec)'
       write (iout,*)'%title-2 Average Flux (arbitrary units)'
       write (iout,*)'%title-3 No. pixels averaged in annulus'
       write (iout,*)'%title-4 Fractional Error in Area'
       do n = 1,number_annuli
         mean_radius = (float(n)-0.5)*radius_annulus
         write (iout,*) mean_radius, average_flux(n),
     *                  number_points(n), area_error(n)
       end do
       close (iout)

999    call map_end_alloc(imap,map_array,status)
       end
C
C
*+ image_addseg

      subroutine image_addseg(minirt,data,uv,ellipse_centre,
     *                         ellipse_theta, ellipse_eccen,
     *                         seg_a1, seg_a2,
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
C   eccentricity and position-angle of ellipse
       real*4      ellipse_theta, ellipse_eccen
C   angles of acceptance for segment (degrees)
       real*4      seg_a1, seg_a2 
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
      real*4       sin_th, cos_th, seg_a
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

      do iv = uv(3),uv(4),-1
        do iu = uv(1),uv(2)
C .. find values
          call iuvval2(minirt,data,uv_point,value,status)
C .. find bin (n) containing point (x,y)
          x = float(iu) - ellipse_centre(1)
          y = float(iv) - ellipse_centre(2)
          seg_a = 180.0*atan2(y,x)/3.14159265
          if (seg_a.lt.0.0) seg_a = seg_a + 360.0
          a = -x*sin_th + y*cos_th
          b = x*cos_th + y*sin_th
          rsq = a**2 + (b*ellipse_eccen)**2
          n = sqrt(rsq)/r_annuli + 1.0
C .. add flux to correct bin
          if ((n.le.n_annuli) .and. (value.gt.gate) .and.
     *         (seg_a.ge.seg_a1 .and. seg_a.le.seg_a2) ) then
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
      call cmd_err(status,'image_addseg',' ')
      end
C



