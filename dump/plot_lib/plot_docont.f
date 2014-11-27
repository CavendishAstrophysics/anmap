C
C
*+ plot_docont

       subroutine plot_docont(map_array,ip_map,ip_scr,status)
C      ------------------------------------------------------
C
C Plot contour on the image plot
C
C Given:
C   map data array
        real*4      map_array(*)
C   pointers to the map data and a cratch array
        integer     ip_map, ip_scr
C Upadted:
C   error status
       integer status
C
C Contour levels are added to the plot; if the contours have
C already been plotted then they are only re-plotted if the option
C is plot_refresh is set to true.
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

C Local variables
C    pointers and other integer variables
       integer    l, ic, nc, ns, test_size
C    transformation array
       real*4     trans(6)
C    contour levels max/min on map
       real*4     work_max, work_min, cont_val
C    UV-range of plot
       integer    uv_full(4)
C    logical variable pspecifying that interpolation is actually used
       logical    use_interp
C    local list of contours
       real*4     clevs(max_contours)
C    pointer to data used in contouring
       integer    ip
C    range on data
       integer    i1, i2, j1, j2, ius, ivs

C check status on entry
       if (status.ne.0) return

C turn off immediate update buffering
       call pgbbuf

C if required interpolate the map data using the standard work array
C then define the matrix to transform to map coordinates appropriately
       call iuv_load(uv_full,status)
       test_size = abs( (uv_range(2)-uv_range(1)+1)
     *                 *(uv_range(3)-uv_range(4)+1) )
       use_interp = interpolate_opt .and. test_size.le.(256*256)
       if ( use_interp ) then
         call intmap(map_array(ip_map),uv_range,
     *               map_array(ip_scr),ius,ivs,status)
       endif

C define transformation matrix
       if (use_interp) then
C .. define transformation for interpolated map
         trans(1) = uv_range(1) - 0.5
         trans(2) = 0.5
         trans(3) = 0.0
         trans(4) = uv_range(3) + 0.5
         trans(5) = 0.0
         trans(6) = -0.5
         i1 = 1
         i2 = ius
         j1 = 1
         j2 = ivs
         ip = ip_scr
       else
C .. define transformation for non-interpolated data
         trans(1) = uv_full(1) - 1
         trans(2) = 1.0
         trans(3) = 0.0
         trans(4) = uv_full(3) + 1
         trans(5) = 0.0
         trans(6) = -1.0
         ius = abs(uv_full(2) - uv_full(1))+1
         ivs = abs(uv_full(3) - uv_full(4))+1
         i1 = uv_range(1) - uv_full(1) + 1
         i2 = uv_range(2) - uv_full(1) + 1
         j1 = uv_full(3) - uv_range(3) + 1
         j2 = uv_full(3) - uv_range(4) + 1
         ip = ip_map
       end if

C scan data array plotted for MAX and MIN
       work_min = 1.0e+30
       work_max = -1.0e+30
       do l = 1,ius*ivs
         if (map_array(ip+l-1).gt.work_max) work_max=map_array(ip+l-1)
         if (map_array(ip+l-1).lt.work_min) work_min=map_array(ip+l-1)
       end do

C loop for each style of contour to plot
       do ns = 1,max_contour_styles

C .. load style
         call graphic_set_line_opt(contour_styles(1,ns),status)

C .. record plotting of specified levels of this style
         nc = 0
         do ic = 1,max_contours
           if (abs(contour_status(ic)).eq.imap_current .and.
     *         contour_type(ic).eq.ns ) then
             cont_val = contour_list(ic)
             if (cont_val.ge.work_min .and. cont_val.le.work_max) then
               if (plot_refresh) then
                 nc = nc + 1
                 clevs(nc) =  contour_list(ic)
               else if (contour_status(ic).gt.0) then
                 nc = nc + 1
                 clevs(nc) =  contour_list(ic)
               endif
               contour_status(ic) = -abs(contour_status(ic))
             endif
           endif
         enddo
         if (nc.gt.0) then
           call pgcont(map_array(ip),ius,ivs,i1,i2,
     *                 j1,j2,clevs,nc,trans )
         endif
       end do


C return to immediate update state
       call pgebuf

C report error messages
       call cmd_err(status,'PLOT_DOCONT',' ')

       end
