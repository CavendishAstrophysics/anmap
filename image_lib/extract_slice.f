C
C
*+ extract_slice

       subroutine extract_slice(map_array,status)
C      ------------------------------------------
C
C Extract a slice from a map and place it in the results file
C
C Given:
C   map space array
       real*4         map_array(*)
C Returned:
C   error status
       integer        status
C
C The user specifies two uv pairs (the cursor may be used for this
C operation) and a section is extracted to the results file.
C
C [PA, March 1993]
*-
       include '../include/anmap_sys_pars.inc'

C local variables
       integer     points_max
       parameter  (points_max = 2000)
       integer     n_points, n, i, iuv_1(2), iuv_2(2)
       real*4      slice(points_max), x_axis(points_max),
     *             uv_1(2), uv_2(2)
       real*4      u, v, slice_max,  slice_min
       real*8      du, dv
       integer     iout, imap, ip_map, minirt(10)

       if (status.ne.0) return

C prompt for map
       call map_getmap('Map : ','Default-Map','READ',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call enminirt( minirt, status )

C read positions on map
       call plot_getpos2(minirt,'Left-end-slice  : ','*',uv_1,status)
       call plot_getpos2(minirt,'Right-end-slice : ','*',uv_2,status)
       if (status.ne.0) then
         call cmd_err(status,'SLICE',' ')
         return
       end if
       do i = 1,2
         iuv_1(i) = nint(uv_1(i))
         iuv_2(i) = nint(uv_2(i))
       end do

C check for a valid slice
       if ( (iuv_1(1).eq.iuv_2(1)) .and.
     *      (iuv_1(2).eq.iuv_2(2)) ) then
          call cmd_wrerr('SLICE','Slice is of zero length')
          goto 999
        end if

C construct x-axis
       if (status.eq.0) then
       n_points = sqrt((uv_1(1)-uv_2(1))**2 + (uv_1(2)-uv_2(2))**2)
       n_points = min(points_max,n_points*3)
       slice_max = -1.0E+30
       slice_min = 1.0E+30
       do n = 1,n_points
         u = float(n-1)*((uv_2(1)-uv_1(1))/float(n_points-1)) + uv_1(1)
         v = float(n-1)*((uv_2(2)-uv_1(2))/float(n_points-1)) + uv_1(2)
         x_axis(n) = sqrt( (u-uv_1(1))**2 + (v-uv_1(2))**2 )
         du = u
         dv = v
         call ruvval(map_array(ip_map),du,dv,2,slice(n),status)
         if (slice(n).lt.slice_min) then
           slice_min = slice(n)
         end if
         if (slice(n).gt.slice_max) then
           slice_max = slice(n)
         end if
       end do
       end if

C release map
       call map_end_alloc(imap,map_array,status)
       if (status.ne.0) then
          call cmd_err(status,'SLICE',' ')
          goto 999
       end if


C open results file
       call io_opefil(iout,general_results_file,'WRITE',0,status)
       if (status.ne.0) then
         call cmd_err(status,'SLICE',
     *                 'Error accessing results file')
         goto 999
       end if
       write (iout,*)'%ndata ',n_points
       write (iout,*)'%ncols 2'
       write (iout,*)'%UV-limits ',iuv_1,iuv_2
       write (iout,*)'%title-x Relative UV'
       write (iout,*)'%title-y Intensity (Map-Units)'
       do n = 1,n_points
         write (iout,*) x_axis(n), slice(n)
       end do
       close (iout)

999    continue
       end
C
