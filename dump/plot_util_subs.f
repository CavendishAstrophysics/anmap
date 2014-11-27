*$ Plot Utility Routines
*  ---------------------
C
C Plot utility routines include the following
C
C PA, last updated 25/02/93
C
*+ do_hist_map

       subroutine plot_histmap(map_array,status)
C      -----------------------------------------
C
C Plot a histogram of map values
C
C Given:
C   map space array
       real*4         map_array(*)
C Returned:
C   error status
       integer        status
C
C The user must supply;
C  Map-entry          -       map to use
C  UV-range           -       region of map to analyse
C  Gate_low           -       lower gate
C  Gate_high          -       higher gate
C
C Only map values satisfying the following are included in the histogram
C
C within(uv_range) .and. Gate_low < abs(map_value) < Gate_high
C
C PA 14/5/93 Version 2.5
*-
       include '/mrao/include/chrlib_functions.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_scratch_defn.inc'


C Local variables
       integer    izmnx(4), uv_range(4), minirt(8), iu, iv
       integer    npts, isum, iwork, ip_work, imap, ip_map, ihist
       real*4     gate_low, gate_high
       real*4     zmnx(4),  my_hist(30), hist_max, val0
       real*4     range_low, range_high
       integer    data_type, ipoln
       real*4     freq
       character  name*16, unit*16, x_label*80, source*40

C check status on entry
       if (status.ne.0) return

C prompt for map
       call map_getmap('Map : ','Default-Map','READ',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)

C prompt for UV range
       call enredt(uv_range,data_type,status)
       call plot_getuv('UV-range : ','*',uv_range,status)

C scan map then prompt for gates
       call scnmap(map_array(ip_map),uv_range,zmnx,izmnx,status)
       gate_low = 0.0
       call io_getr('Gate-low : ','*',gate_low,status)
       gate_high = zmnx(1)
       call io_getr('Gate-high : ','*',gate_high,status)
       if (gate_low.ge.0.0) then
         range_high = gate_high
         range_low  = -gate_high
       else
         range_high = gate_high
         range_low  = gate_low
       end if

C check for error
       call cmd_err(status,'MAP-HISTOGRAM','Failed')
       if (STATUS.eq.0) then

C .. construct array to plot
         call enminirt(minirt,status)
C .. reserve work space for gated data
         call map_alloc_scr(minirt(5),minirt(6),
     *                      'DIRECT',iwork,ip_work,status)
         if (status.ne.0) then
           call cmd_err(status,'MAP-HISTOGRAM','Failed')
           goto 999
         end if
         npts = 0
         isum = ip_work - 1

C .. initialise histogram array (needed to find range for plot)
         do ihist = 1,30
           my_hist(ihist) = 0
         end do
         do iv = uv_range(3),uv_range(4),-1
           do iu = uv_range(1),uv_range(2)
             call iuvval(map_array(ip_map),iu,iv,val0,status)
             if (abs(val0).ge.gate_low .and. abs(val0).le.gate_high)
     *         then
               npts = npts + 1
               isum = isum + 1
               map_array(isum) = val0
               ihist = 25.0*(val0-range_low)/
     *                      (range_high - range_low) + 1
               my_hist(ihist) = my_hist(ihist) + 1.0
             end if
           end do
         end do
         hist_max = 0
         do ihist = 1,25
           if (my_hist(ihist).gt.hist_max) then
             hist_max = my_hist(ihist)
           end if
         end do
         hist_max = hist_max*1.2

C .. open plot
         call graphic_init( graphic_scratch, scratch_defn, status )
         call graphic_open( scratch_defn, status )

C .. define plotting range
         call pgwindow(range_low,range_high,0.0,hist_max)
         scratch_coords(1) = range_low
         scratch_coords(2) = range_high
         scratch_coords(3) = 0.0
         scratch_coords(4) = hist_max
         scratch_coord_opt = 1

C .. plot frame and data
         call graphic_frame(scratch_frame_opt,status)
         call graphic_set_line_opt(scratch_line_opt,status)
         call pghist(npts,map_array(ip_work),range_low,range_high,25,1)
C .. release the space in the work array used for the gated data
         call map_end_alloc(iwork,map_array,status)

C .. construct labels
         call entype(freq,ipoln,name,unit,status)
         write(x_label,100)name(1:chr_lenb(name)),unit(1:chr_lenb(unit))
 100     format(a,'(',a,')')
         call graphic_set_text_opt(scratch_text_opt,status)
         call pglabel(x_label(1:chr_lenb(x_label)),
     *                'Frequency',source(1:chr_lenb(source)))

C .. end the plot
         scratch_command = ' '
         write(scratch_command,'(A,5I7,1PE12.3,1PE12.3)')
     *     'map-analysis map-histogram ',
     *     imap,uv_range,gate_low,gate_high
         call graphic_end(graphic_scratch, scratch_defn, status)
       end if

999    call map_end_alloc(imap, map_array, status)
       end
C
C
*+ plot_scatter

       subroutine plot_scatter(map_array,status)
C      -----------------------------------------
C
C Plot a scatter plot of one map value against a second
C
C Given:
C   map space array
       real*4         map_array(*)
C Returned:
C   error status
       integer        status
C
C A scatter plot of the map values of map-1 against the map values of
C map-2 is drawn. A gate may be applied to both maps - only points
C selected in both maps are plotted - the user may specify the range of
C data values to plot.
C
C [PA, May 88]
*-
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_scratch_defn.inc'

C maps 1 and 2 and their pointers
       integer     imap1, imap2, ip_map1, ip_map2
C uv-ranges and sizes for maps 1 and 2
       integer     uv_range(4), minirt_1(8), minirt_2(8)
C counters
       integer     i1, i2, iu, iv
C output unit
       integer     iout
C limits on data for maps 1 and 2
       real*4      gates_1(2), gates_2(2), hold
       real*4      val_x, val_y
C plot limits
       real*4      x1, x2, y1, y2
C statistical quantities and functions
       real*4      s, sx, sy, sxx, syy, sxy, del, Q, chi2, df,
     *             x_bar, y_bar, sigma_xy, r,
     *             a, sigma_a,
     *             b, sigma_b
       integer     ndata
C text for plot titles and responses
       character   text*80, x_axis*80, y_axis*80, ans*3
C lengths of text strings
       integer     len_t, len_xt, len_yt
C data type
       integer     data_type
C debug routine
       logical     uv_within
C statistical analysis routine
       real*4      fun_gammq

C check status on entry
       if (status.ne.0) return

C read maps and ranges
       call map_getmap('Map for x-axis values : ','Default-Map',
     *                 'READ',imap1,status)
       call enminirt(minirt_1,status)
       call enrnge(gates_1,status)
       call io_getnr('Lower and Upper gates : ','*',gates_1,2,status)
       if (gates_1(1).gt.gates_1(2)) then
         hold = gates_1(1)
         gates_1(1) = gates_1(2)
         gates_1(2) = hold
       end if
       call map_getmap('Map for y-axis values : ','Default-Map',
     *                 'READ',imap2,status)
       call enminirt(minirt_2,status)
       call enrnge(gates_2,status)
       call io_getnr('Lower and Upper gates : ','*',gates_2,2,status)
       if (gates_2(1).gt.gates_2(2)) then
         hold = gates_2(1)
         gates_2(1) = gates_2(2)
         gates_2(2) = hold
       end if
       call io_getr('Error on y-axis data : ','0.001',sigma_xy,status)
       if (status.ne.0) goto 999

C define region of map on which to work
       call enredt(uv_range,data_type,status)
       call plot_getuv('UV-range to analyse : ','*',uv_range,status)
C check this range
       if (.not.uv_within(uv_range,minirt_1) .or.
     *     .not.uv_within(uv_range,minirt_2)     ) then
         call cmd_wrerr('SCATTER','UV-range not on both maps')
         goto 999
       end if

C open the plot device
       call graphic_init( graphic_scratch, scratch_defn, status )
       call graphic_open( scratch_defn, status )

C define plotting range
       call pgrnge(gates_1(1),gates_1(2),x1,x2)
       call pgrnge(gates_2(1),gates_2(2),y1,y2)
       call pgwindow(x1,x2,y1,y2)
       scratch_coords(1) = x1
       scratch_coords(2) = x2
       scratch_coords(3) = y1
       scratch_coords(4) = y2
       scratch_coord_opt = 1


       call pgbbuf
       call graphic_frame(scratch_frame_opt,status)
       call graphic_set_line_opt(scratch_line_opt,status)

C loop through the map data, plot and determine statistics
       call map_alloc_in(imap1,'SEQUENTIAL',map_array,ip_map1,status)
       call map_alloc_in(imap2,'SEQUENTIAL',map_array,ip_map2,status)
       x_bar = 0.0
       y_bar = 0.0
       s = 0.0
       sx = 0.0
       sy = 0.0
       sxy = 0.0
       sxx = 0.0
       syy = 0.0
       chi2 = 0.0
       del = 0.0
       b = 0.0
       ndata = 0
       call graphic_set_text_opt(scratch_text_opt,s)
       do iv = uv_range(3),uv_range(4),-1
         call map_row_read(imap1,iv,map_array,ip_map1,status)
         call map_row_read(imap2,iv,map_array,ip_map2,status)
         if (status.ne.0) goto 999
         do iu = uv_range(1),uv_range(2)
           i1 = ip_map1 + iu - minirt_1(1)
           i2 = ip_map2 + iu - minirt_2(1)
           val_x = map_array(i1)
           val_y = map_array(i2)
           if (val_x.ge.gates_1(1) .and. val_x.le.gates_1(2) .and.
     *         val_y.ge.gates_2(1) .and. val_y.le.gates_2(2) ) then
             call pgpoint(1,val_x,val_y,scratch_symbol)
             x_bar = x_bar + val_x
             y_bar = y_bar + val_y
             sx = sx + val_x
             sxx = sxx + val_x*val_x
             sy = sy + val_y
             syy = syy + val_y*val_y
             sxy = sxy + val_x*val_y
             ndata = ndata + 1
           end if
         end do
       end do
       call pgebuf
C find statistics
       s = float(ndata)
       x_bar = x_bar / float(ndata)
       y_bar = y_bar / float(ndata)
       do iv = uv_range(3),uv_range(4),-1
         call map_row_read(imap1,iv,map_array,ip_map1,status)
         call map_row_read(imap2,iv,map_array,ip_map2,status)
         if (status.ne.0) goto 999
         do iu = uv_range(1),uv_range(2)
           i1 = ip_map1 + iu - minirt_1(1)
           i2 = ip_map2 + iu - minirt_2(1)
           val_x = map_array(i1)
           val_y = map_array(i2)
           if (val_x.ge.gates_1(1) .and. val_x.le.gates_1(2) .and.
     *         val_y.ge.gates_2(1) .and. val_y.le.gates_2(2) ) then
             b = b + (val_x - x_bar)*(val_y - y_bar)
             del = del + (val_x - x_bar)**2
           end if
         end do
       end do
       b = b / del
       a = y_bar - b*x_bar
       del = s*sxx - sx**2
       sigma_a = sqrt( sxx/del )
       sigma_b = sqrt( s/del )
C find chi squared and determine probability
       do iv = uv_range(3),uv_range(4),-1
         call map_row_read(imap1,iv,map_array,ip_map1,status)
         call map_row_read(imap2,iv,map_array,ip_map2,status)
         if (status.ne.0) goto 999
         do iu = uv_range(1),uv_range(2)
           i1 = ip_map1 + iu - minirt_1(1)
           i2 = ip_map2 + iu - minirt_2(1)
           val_x = map_array(i1)
           val_y = map_array(i2)
           if (val_x.ge.gates_1(1) .and. val_x.le.gates_1(2) .and.
     *         val_y.ge.gates_2(1) .and. val_y.le.gates_2(2) ) then
             chi2 = chi2 + ((val_y-a-b*val_x)/sigma_xy)**2
           end if
         end do
       end do
       df = (s-2)/2
       Q = fun_gammq( df, chi2/2.0, status )
       r = (s*sxy - sx*sy) /
     *     sqrt( (s*sxx-sx*sx)*(s*syy-sy*sy) )
C report results
       call io_enqout(iout)
       write(iout,100) r, a, sigma_a, b, sigma_b, chi2, df, Q
100    format(1X,'.. regression fit (y=a+bx): r = ',F7.4/
     *        1X,'.. a      = ',1PE12.3,' +/- ',1PE12.3/
     *        1X,'.. b      = ',1PE12.3,' +/- ',1PE12.3/
     *        1X,'.. Chi**2 = ',1PE12.3,' with ',1PE12.3,' d.f.'/
     *        1X,'.. Probability that Chi**2 is this bad = ',1PE12.3)
       if (io_yesno('Plot this regression line ? ','yes',status)) then
         call pgmove( x1, a+b*x1 )
         call pgdraw( x2, a+b*x2 )
         ans ='yes'
       else
         ans = 'no'
       end if

C add titles to the plot
       call io_getstr('X-axis title : ',' ',x_axis,status)
       len_xt = chr_lenb(x_axis)
       call io_getstr('Y-axis title : ',' ',y_axis,status)
       len_yt = chr_lenb(y_axis)
       call io_getstr('Main title : ',' ',text,status)
       len_t = chr_lenb(text)
       if (status.eq.0) then
         call pglabel(x_axis(1:len_xt),y_axis(1:len_yt),text(1:len_t))
       end if

C end the plot
       scratch_command = ' '
       write(scratch_command,
     *  '(A,I4,1PE12.3,1PE12.3,I4,1PE12.3,1PE12.3,1PE12.3,4I7,
     *    '' '',A,'' '',A,'' '',A,'' '',A)')
     *    'map-analysis scatter-plot ',
     *    imap1, gates_2, imap2, gates_2,
     *    sigma_xy, uv_range, ans, x_axis(1:len_xt),
     *    y_axis(1:len_yt), text(1:len_t)
       call graphic_end(graphic_scratch, scratch_defn, status)

C release the maps
999    continue
       call map_end_alloc(imap1,map_array,status)
       call map_end_alloc(imap2,map_array,status)
       call cmd_err(status,'SCATTER','No plot produced')

       end
C
C
*+ plot_surface

       subroutine plot_surface(map_array,status)
C      -----------------------------------------
C
C Plot a surface representation of a map
C
C Given:
C   map space array
       real*4         map_array(*)
C Returned:
C   error status
       integer        status
C
C The current map is displayed as a relief surface plot.
C Full plot overlay is supported in this mode.
C
C [PA, December 1991]
*-
       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_scratch_defn.inc'

C local variables
       integer     max_size
       parameter  (max_size = 1024)
       real*4      x_axis(max_size), work(max_size)
       integer     minirt(10), ixr(4), uv_range(4), data_type
       real*4      bias, zmnx(4), x1, x2, y1, y2
       integer     ioff, iscr, ip_scr, i, j, izmnx(4)
       integer     imap, ip_map

C read map
       call map_getmap('Map : ','Default-Map','READ',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call enminirt( minirt, status )
C prompt for UV range
       call enredt(uv_range,data_type,status)
       call plot_getuv('UV-range : ','*',uv_range,status)

C get range to plot
       do i=1,2
         ixr(i) = uv_range(i) - minirt(1) + 1
       end do
       do i=3,4
         j = 3
         if (i.eq.3) j=4
         ixr(j) = uv_range(i) - minirt(4) + 1
       end do
C get other information
       call scnmap2(minirt,map_array(ip_map),uv_range,zmnx,izmnx,status)
       bias = abs(zmnx(1)-zmnx(2))*3.0/float(uv_range(3)-uv_range(4))
       call io_getr('Offset between rows (map-units) : ',
     *              '*',bias,status)
       ioff = 1
       call io_geti('Offset between columns (pixels) : ',
     *              '*',ioff,status)
C flip image
       call map_alloc_scr( map_side_size*2, map_side_size*2,
     *                       'DIRECT', iscr, ip_scr, status )
       do j=1,minirt(6)
         do i=1,minirt(5)
           map_array(ip_scr+i-1+(j-1)*minirt(5)) =
     *              map_array(ip_map+i-1+(minirt(6)-j)*minirt(5))
         end do
       end do
       call map_end_alloc( iscr, map_array, status )
       call map_end_alloc( imap, map_array, status )

C check errors
       if (status.ne.0) then
          call cmd_err(status,'SURFACE',' ')
          return
       end if

C open the plot device
       do i=uv_range(1),uv_range(2)
         x_axis(i-uv_range(1)+1) = i
       end do
       if (ioff.ge.0) then
         x1 = x_axis(1)
         x2 = x_axis(ixr(2)-ixr(1)+1)
         x2 = x2 + (x2-x1)*ioff
       else
         x1 = x_axis(1)
         x2 = x_axis(ixr(2)-ixr(1)+1)
         x1 = x1 + (x1-x2)*ioff
       end if
       y1 = zmnx(2)
       y2 = zmnx(2)+bias*float(ixr(4)-ixr(3))
       call io_getr('X-low : ','*',x1,status)
       call io_getr('X-high : ','*',x2,status)
       call io_getr('Y-low : ','*',y1,status)
       call io_getr('Y-high : ','*',y2,status)


C open plot
       call graphic_init( graphic_scratch, scratch_defn, status )
       call graphic_open( scratch_defn, status )

C define plotting range
       scratch_coords(1) = x1
       scratch_coords(2) = x2
       scratch_coords(3) = y1
       scratch_coords(4) = y2
       scratch_coord_opt = 1
       if (status.ne.0) goto 999

C do plot
       call pgbbuf
       call pgwindow( x1, x2, y1, y2 )
       call graphic_copy_graphic(scratch_defn,graphic,status)
       call graphic_set_line_opt(scratch_line_opt,status)
       call pghi2d(map_array(ip_scr),minirt(5),minirt(6),
     *             ixr(1),ixr(2),ixr(3),ixr(4),x_axis,ioff,bias,
     *             .true.,work)
       call pgebuf
       write(scratch_command,10)imap,uv_range,bias,ioff,x1,x2,y1,y2
  10   format('map-display surface-plot ',5i7,1PE12.3,i4,
     *        1PE12.3,1PE12.3,1PE12.3,1PE12.3)
       call graphic_end(graphic_scratch, scratch_defn, status)
999    call cmd_err( status,'SURFACE-PLOT',' ')
       end
C
C
*+ plot_slice

       subroutine plot_slice(map_array,status)
C      ---------------------------------------
C
C Plot a slice through the map data
C
C Given:
C   map space array
       real*4         map_array(*)
C Returned:
C   error status
       integer        status
C
C The user specifies two uv pairs (the cursor may be used for this
C operation) and a section is drawn between these points. Full plot
C overlay is supported in this mode. The map MUST be the map currently
C selected for contouring (using set-map).
C
C [PA, April 88, September 1988, December 1989]
*-
       include '../include/anmap_sys_pars.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_scratch_defn.inc'

C local variables
       integer     points_max
       parameter  (points_max = 2000)
       integer     n_points, n, i, iuv_1(2), iuv_2(2)
       real*4      slice(points_max), x_axis(points_max),
     *             uv_1(2), uv_2(2), u1, u2, v1, v2
       real*4      u, v, slice_max,  slice_min
       real*8      du, dv
       character   text*80
       integer     len_t, iout, imap, ip_map, minirt(10)

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

C open the plot device
       call graphic_init( graphic_scratch, scratch_defn, status )
       call graphic_open( scratch_defn, status )


C define plot size
       call pgrnge(slice_min,slice_max,v1,v2)
       call pgrnge(x_axis(1),x_axis(n_points),u1,u2)
       call pgwindow(u1,u2,v1,v2)
       scratch_coords(1) = u1
       scratch_coords(2) = u2
       scratch_coords(3) = v1
       scratch_coords(4) = v2
       scratch_coord_opt = 1

C plot
       call pgbbuf
       call graphic_frame(scratch_frame_opt,status)
       call graphic_set_line_opt(scratch_line_opt,status)
       call pgline(n_points,x_axis(1),slice(1))
       write(text,'(''Slice from '',2i5,'' to '',2i5)')iuv_1,iuv_2
       call graphic_set_text_opt(scratch_text_opt,status)
       call pglabel('Relative UV','Intensity (Map-Units)',text(1:len_t))

C end the plot
       call pgebuf
       scratch_command = ' '
       write(scratch_command,'(A,I7,1PE12.3,1PE12.3,1PE12.3,1PE12.3)')
     *       'map-analysis plot-slice ',
     *       imap, uv_1, uv_2
       call graphic_end(graphic_scratch, scratch_defn, status)

C open results file
       call io_opefil(iout,general_results_file,'WRITE',0,status)
       if (status.ne.0) then
         call cmd_err(status,'Plot-Slice',
     *                 'Error accessing results file')
         goto 999
       end if
       write (iout,*)'%ndata ',n_points
       write (iout,*)'%ncols 2'
       write (iout,*)'%title Slice: ',text(1:len_t)
       write (iout,*)'%UV-limits ',iuv_1,iuv_2
       write (iout,*)'%title-1 Relative UV'
       write (iout,*)'%title-2 Intensity (Map-Units)'
       do n = 1,n_points
         write (iout,*) x_axis(n), slice(n)
       end do
       close (iout)

999    continue
       end
C
C
C
*+ plot_isometric

       subroutine plot_isometric(map_array,status)
C      -------------------------------------------
C
C Plot an isometric representation of a map
C
C Given:
C   map space array
       real*4         map_array(*)
C Returned:
C   error status
       integer        status
C
C The current map is displayed as a relief surface plot.
C Full plot overlay is supported in this mode.
C
C [PA, August 1993]
*-
       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_scratch_defn.inc'

C local variables
       integer     max_size
       parameter  (max_size = 1024)
       integer     minirt(10)
       real*4      x1, x2, y1, y2, angle, s2(2), size
       integer     imap, ip_map, iscr, ip_scr, i

C read map
       call map_getmap('Map : ','Default-Map','READ',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call enminirt( minirt, status )
       call map_alloc_scr( minirt(5),minirt(6),
     *                    'DIRECT', iscr, ip_scr, status )
       do i=1,minirt(5)*minirt(6)
         map_array(i+ip_scr-1) = map_array(i+ip_map-1)
       enddo
       call map_end_alloc( iscr, map_array, status )
       call map_end_alloc( imap, map_array, status )
       call io_getr('Viewing-angle : ','45.0',angle,status)
       s2(1) = 0.0
       s2(2) = 0.0
       call io_getnr('Scaling-range (default=auto-scale) : ','0.0',
     *               s2,2,status )
       call io_getr('Size-scale : ','1.0',size,status)

C check errors
       if (status.ne.0) then
          call cmd_err(status,'ISOMETRIC',' ')
          return
       end if

C open the plot device
       x1 = minirt(1)
       x2 = minirt(2)
       y1 = minirt(4)
       y2 = minirt(3)

C open plot
       call graphic_init( graphic_scratch, scratch_defn, status )
       call graphic_open( scratch_defn, status )

C define plotting range
       scratch_coords(1) = x1
       scratch_coords(2) = x2
       scratch_coords(3) = y1
       scratch_coords(4) = y2
       scratch_coord_opt = 1
       if (status.ne.0) goto 999

C do plot
       call pgwindow( 0.0, 1.0, 0.0, 1.0 )
       call graphic_copy_graphic(scratch_defn,graphic,status)
       call graphic_set_line_opt(scratch_line_opt,status)
       call pgisom(map_array(ip_scr),minirt(5),minirt(6),size,angle,
     *             s2(1),s2(2))
       scratch_command = ' '
       write(scratch_command,10)imap,angle
  10   format('map-display isometric-plot ',i7,F14.2)
       call graphic_end(graphic_scratch, scratch_defn, status)
999    call cmd_err( status,'ISOMETRIC-PLOT',' ')
       end
C
C


