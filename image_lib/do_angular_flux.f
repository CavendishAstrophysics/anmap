C
C
*+ do_angular_flux

       subroutine do_angular_flux(map_array,status)
C      --------------------------------------------
C
C Add flux in anglar regions as defined by the user
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The flux is calculated in angular sections.
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
       real*4     mean_angle
       integer    uv_range(4), number_angles
       real*4     uv_centre(2)
C   radius of annulus and gate
       real*4     r_1, r_2, gate
C   maximum number of angles the user may specify,  flux in each region,
C   error in each region, number of points found in each annulus
       integer    max_angles
       parameter (max_angles = 100)
       integer    number_points(max_angles)
       real*4     average_flux(max_angles), area_error(max_angles)
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
         call cmd_err(status,'ANGULAR-FLUX',' ')
         goto 999
       end if

C check that this image is a sky-plane image
       call enmapj(iproj,usamp,skew,epoch,prang,status)
       call enminirt(minirt,status)
       if (iproj.eq.equatorial_projection) then
         call cmd_wrerr('ANGULAR-FLUX',
     *             'Unable to work on EQUATORIAL-PLANE projection')
         call cmd_wrerr('ANGULAR-FLUX','Use REPROJECT-MAP')
         goto 999
       end if

C find user defined parameters
       call io_geti('Number of angular segments : ',
     *              '20',number_angles,status)
       if (number_angles.gt.max_angles) then
         call cmd_wrerr('ANGULAR-FLUX','Too many angles > 100')
         goto 999
       end if
       r_1 = 0.0
       r_2 = 256.0
       call io_getr('Inner radius (pixels): ','*',r_1,
     *           status)
       call io_getr('Outer radius (pixels): ','*',r_2,
     *           status)

C find uv-range to analyse and centre of ellipse
       call enredt(uv_range,data_type,status)
       call plot_getuv('UV-range : ','*',uv_range,status)
       call plot_getpos2(minirt,'Centre of region : ','*',
     *                   uv_centre,status)


C do the work
      call image_addang(minirt,map_array(ip_map),uv_range,uv_centre,
     *                   number_angles, r_1, r_2, gate, 
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
       write (iout,*)'%ndata ',number_angles
       write (iout,*)'%ncols 4'
       write (iout,*)'%title ANGULAR FLUX output on ',idate
       write (iout,*)'%source ',source(1:chr_lenb(source))
       write (iout,*)'%title-1 Mean Angle of Region (degrees)'
       write (iout,*)'%title-2 Average Flux (arbitrary units)'
       write (iout,*)'%title-3 No. pixels averaged in annulus'
       write (iout,*)'%title-4 Fractional Error in Area'
       do n = 1,number_angles
         mean_angle = (float(n)-0.5)*360.0/float(number_angles)
         write (iout,*) mean_angle, average_flux(n),
     *                  number_points(n), area_error(n)
       end do
       close (iout)
999    call map_end_alloc(imap,map_array,status)
       call cmd_err(status,'ANGULAR-FLUX','Failed')

       end



