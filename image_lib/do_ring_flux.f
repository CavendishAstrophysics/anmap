C
C
*+ do_ring_flux

       subroutine do_ring_flux(map_array,status)
C      -----------------------------------------
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
C The flux is calculated in elliptical annuli for the specified map.
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
       call map_getmap('Map : ','Default-Map','READ',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call mapcat_enqsr(imap,source,program,status)
       if (status.ne.0) then
         call cmd_err(status,'RING-FLUX',' ')
         goto 999
       end if

C check that this image is a sky-plane image
       call enmapj(iproj,usamp,skew,epoch,prang,status)
       call enminirt(minirt,status)
       if (iproj.eq.equatorial_projection) then
         call cmd_wrerr('RING-FLUX',
     *             'Unable to work on EQUATORIAL-PLANE projection')
         call cmd_wrerr('RING-FLUX','Use REPROJECT-MAP')
         goto 999
       end if

C find user defined parameters
       call io_geti('Number of annuli : ','20',number_annuli,status)
       if (number_annuli.gt.max_annuli) then
         call cmd_wrerr('RING-FLUX','Too many annuli > 100')
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
C .. convert to radius
       rad_pos_angle = position_angle*3.14159265/180.0

C do the work
      call image_addring(minirt,map_array(ip_map),uv_range,uv_centre,
     *                   rad_pos_angle,eccentricity,
     *                   number_annuli, pix_rad_annulus,
     *                   average_flux,number_points,area_error,status)
C
C write this out to the results file
       call io_opefil(iout,general_results_file,'WRITE',0,status)
       if (status.ne.0) then
         call cmd_err(status,'RING-FLUX',
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
       call cmd_err(status,'RING-FLUX','Failed')

       end



