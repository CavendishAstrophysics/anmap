*$ ANMAP Plot-Library Routines
*  ---------------------------
C
C Last modified P. Alexander     02/07/91
C
C The routines in this library provide the interface to PGPLOT
C with respect to opening and closing devices, manipulating windows
C and viewports and managing the graphics overlay facility.
C Routines also exist to define contour levels, do the actual plotting
C of frames, grey-scales and contours as well as cursor input routines.
C
C Futher details on the use of these routines can be found in the
C programmers guide to plot routines in ANMAP.
C   ND file:  (anmap)info-prog-guide
C
C
*$ 1) Plot Management Routines
C
*+ plt_open

       subroutine plt_open(status)
C      ---------------------------
C
C Open the next plot segment and plot device if required
C
C If the plot is closed then it is opened and if no default device
C is given one is prompted for.
C
C The next segment for the current plot is then chosen. If the number of
C segments is 1 then the routine simply initialises the plot without
C any user intervention.
C
C The default view-surface is also initialised
*-
       $include (anmap)anmap-plot-sys:incl
       $include (library)chrlib-functions:incl

C error flag and integer functions
       integer    status
C integer variables for string manipulation
       integer    len_o, i1

C check status on entry
       if (status.ne.0) return

       if (.not.plot_open) then

C .. initialise plotting device
         len_o = lenb(output_device)
         if (len_o.eq.0) then
           status = 0
           call enqplt(0,output_device)
           call getplt('Output-device (?=list) : ',
     *                 '*',output_device,status)
         end if
         call pgbegin(0,output_device,segments,segments)
         i1 = index(output_device,'/')
         if (len_o.eq.0) output_device = ' '
         call pgask(.false.)
         plot_open = .true.

       end if

       if (.not.graphics_overlay) then
         call pgadvance
         view_port(1) = 0.0
         view_port(2) = 1.0
         view_port(3) = 0.0
         view_port(4) = 1.0
       else
         view_port(1) = overlay_vp(1)
         view_port(2) = overlay_vp(2)
         view_port(3) = overlay_vp(3)
         view_port(4) = overlay_vp(4)
       end if
       call pgvport(view_port(1),view_port(2),
     *              view_port(3),view_port(4))
       call pgwindow(0.0,1.0,0.0,1.0)
       if (graphics_overlay .and. screen_clear) then
         call pgsci(0)
         call pgrect(0.0,1.0,0.0,1.0)
       end if
       call pgsci(1)
       call pgsls(1)
       call cmd_err(status,'PLT_OPEN',' ')

       end
C
C
*+ plt_frinit

       subroutine plt_frinit(reset_frame, status)
C      ------------------------------------------
C
C Initialise the plot frame for contouring and grey-scaling
C
C Option to control resetting the frame, viewport and window
       logical    reset_frame
C Error return
       integer    status
C
*-
$include (anmap)anmap-plot-sys:incl

C windows on viewport and window
       real*4     wx1, wx2, wy1, wy2, xv1,  xv2,  yv1,  yv2,
     *            vx1, vx2, vy1, vy2, xuv1, xuv2, yuv1, yuv2
C space for the plot within the view-surface
       real*4     space_x, space_y
C scaling parameters for the plot
       real*4     x_uv_size, y_uv_size, size_x, size_y, size_plot,
     *            ratio, x_convert

C check status on entry
       if (status.ne.0) return

C open plot
       call plt_open(status)

C define space to be occupied by the plot and the position
       space_x = view_port(2) - view_port(1)
       space_y = view_port(4) - view_port(3)

C calculate ratio of sides of plot to give equal scales (mm/gp)
       y_uv_size = uv_range(3) - uv_range(4) + 1.0
       x_uv_size = uv_range(2) - uv_range(1) + 1.0
       ratio = x_uv_size/y_uv_size

C set frame plotting parameters to NO-FRAME-PLOTTED
       cont_plot_s = 0
       cont_plot_d = 0
       frame_done  = .false.
       cont_done   = .false.
       title_done  = .false.
       date_done   = .false.

C enquire actual size of plotting surface (mm)
       call pgqvp(2,vx1,vx2,vy1,vy2)

C ensure a plot with equal scales in X and Y (physical)
       x_convert = (vy2-vy1)/(vx2-vx1)
       if (x_convert.gt.1.0) then
         if (frame_space.le.0.0) then
           space_y = space_y/1.2
           space_x = space_x/1.2
         else
           space_y = space_y*frame_space
           space_x = space_x*frame_space
         end if
       else if (ratio.gt.1.0) then
         if (frame_space.gt.0.0) then
           space_x = space_x*frame_space/x_convert
           space_y = space_y/x_convert
         end if
       end if

C check for scaling in Y or X direction
       if (x_convert.gt.1.0) then
         yv2 = view_port(4) - space_y*0.05/x_convert
         xv1 = view_port(1) + space_x*0.025
         if (ratio.le.1.0) then
           yv1 = yv2 - space_y*0.9/x_convert
           xv2 = ratio*space_x*0.9 + xv1
         else
           yv1 = yv2 - space_y*0.9/(ratio*x_convert)
           xv2 = space_x*0.9 + xv1
         end if
       else
         yv2 = view_port(4) - space_y*0.05
         xv1 = view_port(1) + space_x*0.025*x_convert
         if (ratio.le.1.0) then
           yv1 = yv2 - space_y*0.9
           xv2 = ratio*space_x*x_convert*0.9 + xv1
         else
           yv1 = yv2 - space_y*0.9/ratio
           xv2 = space_x*x_convert*0.9 + xv1
         end if
       end if

C .. set the viewport
       call pgvport(xv1+frame_offset(1),xv2+frame_offset(1),
     *              yv1+frame_offset(2),yv2+frame_offset(2))

C .. set the window
       xuv1 = uv_range(1)
       xuv2 = uv_range(2)
       yuv1 = uv_range(4)
       yuv2 = uv_range(3)
       call pgwindow(xuv1,xuv2,yuv1,yuv2)
       if (reset_frame) then
         call pgqvp(0,frame_vp(1),frame_vp(2),
     *                frame_vp(3),frame_vp(4))
         call pgqwin(frame_win(1),frame_win(2),
     *               frame_win(3),frame_win(4))
       end if
       frame_init = .true.
       call cmd_err(status,'PLT_FRINIT',' ')

       end
C
C
*+ plt_frset

       subroutine plt_frset(status)
C      ----------------------------
C
C Define the view-port and window appropriate for contouring/grey-scale
C
C Error return
       integer    status
C
*-
$include (anmap)anmap-plot-sys:incl

       if (status.ne.0) return
       call pgvport(frame_vp(1),frame_vp(2),frame_vp(3),frame_vp(4))
       call pgwindow(frame_win(1),frame_win(2),
     *               frame_win(3),frame_win(4))
       call cmd_err(status,'PLT_FRSET',' ')

       end
C
C
*+ plt_frend

       subroutine plt_frend(status)
C      ----------------------------
C
C redefine the view-port and window after contouring/grey-scale
C
C Error return
       integer    status
C
*-
$include (anmap)anmap-plot-sys:incl

       if (status.ne.0) return
       call pgvport(view_port(1),view_port(2),
     *              view_port(3),view_port(4))
       call pgwindow(0.0,1.0,0.0,1.0)
       call cmd_err(status,'PLT_FREND',' ')

       end
C
C
*+ plt_grinit

       subroutine plt_grinit(status)
C      -----------------------------
C
C initialise a graphics plot - overlayed if required on the screen
C
C A new plot is requested - if the graphic-overlay option is in effect
C then the plot will be overlayed on the current plot in the current
C window, else approprate calls will be made to advance to the next
C window or start a new plot.
C
*-
$include (anmap)anmap-plot-sys:incl
C
       integer              status
       real*4               old_char_size, new_char_size
       real*4               vp_offset
       common /local_plot/  old_char_size, new_char_size

       old_char_size = 1.0
       new_char_size = 1.0

       if (status.ne.0) return

       call plt_open(status)

C test for graphics_overlay mode
       if (graphics_overlay) then

C .. reset viewport to allow for labelling
           vp_offset = (sqrt((view_port(2)-view_port(1))*
     *                       (view_port(4)-view_port(3))))
           new_char_size = vp_offset
           vp_offset = 0.15*vp_offset
           call pgsch(text_size*new_char_size)
           call pgscf(text_font)
           call pgvport(view_port(1)+vp_offset,view_port(2)-vp_offset,
     *                  view_port(3)+vp_offset,view_port(4)-vp_offset)

       else
           call pgsch(text_size*new_char_size)
           call pgscf(text_font)
           call pgvport(0.1,0.9,0.1,0.9)
       end if
       call cmd_err(status,'PLT_GRINIT',' ')

       end
C
C
*+ plt_grend

       subroutine plt_grend(status)
C      ----------------------------
C
C Close a graphics plot
C
C If overlay mode is operational then the plot returns to the state
C before the call to plt_grinit, else the device is closed.
C
$include (anmap)anmap-plot-sys:incl
C
       integer              status
       real*4               old_char_size, new_char_size
       common /local_plot/  old_char_size, new_char_size

       if (status.ne.0) return

       call pgvport(view_port(1),view_port(2),
     *              view_port(3),view_port(4))
       call pgwindow(0.0,1.0,0.0,1.0)
       if (graphics_overlay) then
          call pgsch(old_char_size)
       else
          frame_init = .false.
          plot_open = .false.
          call pgend
          call pgsch(old_char_size)
       end if
       call pgscf(1)

       end
C
C
*+ plt_end

       subroutine plt_end(status)
C      --------------------------
C
C End current plot
*-
$include (anmap)anmap-plot-sys:incl

       integer   status

       if (status.ne.0) return

       call pgend
       plot_open  = .false.
       frame_init = .false.
       frame_done = .false.
       title_done = .false.
       cont_done  = .false.
       date_done  = .false.
       cont_plot_d = 0
       cont_plot_s = 0

       end
C
C
*+ plt_reset

       subroutine plt_reset(status)
C      ----------------------------
C
C Reset the plot options to default values
*-
$include (anmap)anmap-plot-sys:incl

       integer   status

       if (status.ne.0) return

C .. options
       title_opt = .false.
       date_opt  = .true.
       cont_opt  = .true.
       pips_opt  = .true.
       grid_opt  = .false.
       int_opt   = .false.

C .. contours
       solid_style  = 1
       solid_colour = 1
       dash_style   = 2
       dash_colour  = 1

C .. pips
       pip_size  = 1.0
       mark_size = 1.0
       mark_font = 1
       text_size = 2.0
       text_font = 1

C .. scale bar
       scale_bar_opt = .false.
       scale_bar_width = 0.05

C .. overlay style
       screen_clear = .true.
       frame_offset(1) = 0.0
       frame_offset(2) = 0.0

       end
C
C
*+ plt_dump

       subroutine plt_dump(status)
C      ---------------------------
C
C Dump all plot information to a save file
C
C Returned:
C    error return code
       integer    status
C-
       $include (anmap)anmap-sys-pars:incl
       $include (anmap)anmap-plot-sys:incl
       $include (library)io-constants:incl

       integer   iunit, i, len_file, mode, termno
       character file_name*(iolen_file), user_name*(iolen_user)

       user_name = ' '
       call enqexe(user_name,mode,termno)
       call makfil(user_name,plot_dump_file,' ',file_name,len_file)
       call opefil(iunit,file_name(1:len_file),'WRITE',0,status)
       write (iunit) (plot_sys_record(i), i=1,iplt_length)
       close (iunit)
       call makfil(user_name,pgplot_dump_file,' ',file_name,len_file)
       call pgdump(file_name(1:len_file))
       call cmd_err(status,'plt_dump',status)
       end
C
C
*+ plt_load

       subroutine plt_load(status)
C      ---------------------------
C
C Load plot information from a save file
C
C Returned:
C    error return code
       integer    status
C-
       $include (anmap)anmap-sys-pars:incl
       $include (anmap)anmap-plot-sys:incl
       $include (library)io-constants:incl

       character value*40
       logical   exist
       integer   iunit, i, len_value, len_file, mode, termno, iprint
       character file_name*(iolen_file), user_name*(iolen_user)

       user_name = ' '
       iprint = 0
       call enqexe(user_name,mode,termno)
       call makfil(user_name,plot_dump_file,' ',file_name,len_file)
       inquire (file=file_name(1:len_file), exist=exist)
       if (exist) then
         call opefil(iunit,file_name(1:len_file),'READ',0,status)
         read (iunit) (plot_sys_record(i), i=1,iplt_length)
         close (iunit)
         call delfil(file_name(1:len_file),iprint,status)
       end if
       call makfil(user_name,pgplot_dump_file,' ',file_name,len_file)
       inquire (file=file_name(1:len_file), exist=exist)
       if (exist) then
         call pgload(file_name(1:len_file))
         call pgqinf('STATE',value,len_value)
         if (value(1:len_value).eq.'OPEN' .and. plot_open) then
           call pgqinf('DEV/TYPE',value,len_value)
           call pgbegin(iunit,value(1:len_value),segments,segments)
           call pgload(file_name(1:len_file))
         end if
         call delfil(file_name(1:len_file),iprint,status)
       end if
       call cmd_err(status,'plt_dump',status)
       end

C
C
*$ 2) Routines to plot Map Data
C
C
*+ plt_doframe

       subroutine plt_doframe(status)
C      ------------------------------
C
C Plot frame, pips and grid
*-
$include (anmap)anmap-plot-sys:incl

C box options and scaling for pips
       character*10   opt_x, opt_y
       real*4         x_scl, y_scl
C current colour index, line style
       integer        old_colour, old_style
C error flag
       integer        status

C check status on entry
       if (status.ne.0) return

C check for map
       if (.not.map_read) then
         call cmd_wrerr('PLOT-FRAME','No map set')
         return
       end if

C initialise frame if required
       if (.not.frame_init) then
         call plt_frinit(.true.,status)
       end if

C open frame
       call plt_frset(status)

C enquire colour and line style
       call pgqci(old_colour)
       call pgqls(old_style)

C define colour for frame
       call pgsci(frame_colour)

C plot frame if not already plotted
       if (.not.frame_done) then

C .. switch to deffered update state
         call pgbbuf

C .. box
         opt_x = ' '
         opt_y = ' '
         x_scl = 0.0
         y_scl = 0.0
         if (.not.pips_opt) then
           if (uvpip_opt) then
             if (uvgrid_opt) then
               x_scl = grid_u
               y_scl = grid_v
               opt_x = 'BCNTSG'
               opt_y = 'BCMTSGV'
             else
               opt_x = 'BCNTS'
               opt_y = 'BCMTSV'
             end if
           else
             if (uvgrid_opt) then
               x_scl = grid_u
               y_scl = grid_v
               opt_x = 'BCNG'
               opt_y = 'BCMGV'
             else
               opt_x = 'BC'
               opt_y = 'BC'
             end if
           end if
         else
           if (uvgrid_opt) then
             x_scl = grid_u
             y_scl = grid_v
             opt_x = 'BCG'
             opt_y = 'BCG'
           else
             opt_x = 'BC'
             opt_y = 'BC'
           end if
         end if

         call pgbox(opt_x,x_scl,0,opt_y,y_scl,0)
C .. pips
         if (pips_opt) call plt_pips(status)
C .. grid
         if (grid_opt) call plt_grid(status)
C .. frame
         frame_done = .true.

       end if

C return to immediate update state
       call pgebuf
C restore colour and line style
       call pgsci(old_colour)
       call pgsls(old_style)
C close frame
       call plt_frend(status)

C report error message if required
       call cmd_err(status,'PLT_DOFRAME',' ')

       end
C
C
*+ plt_dotext

       subroutine plt_dotext(status)
C      -----------------------------
C
C Add text to contour/grey-scale plot
C
*-

$include (anmap)anmap-plot-sys:incl
$include (library)maplib-redtape:incl

C error flag
       integer   status
C variables used to store attribute settings
       integer   old_font
       real*4    old_ch_size
C windows and window scaling
       real*4    wx1, wx2, wy1, wy2, vx1, vx2, vy1, vy2,
     *           wx_size, wy_size, vx_size, vy_size,
     *           a_val, b_val
C pointers to start of text output in world coordinates
       real*4    xstart, ystart, ydrop, xend
C loop counters
       integer   nc, i_total
C lengths of character strings
       integer   len_tit, len_ctit, len_nam, len_uni, len_prog,
     *           len_s, len_date, lenb
C index to contour arrays
C index_dash   --     index: pointer to sorted dashed contours
C index_solid  --     index: pointer to sorted solid contours
C hist_date    --     date from redtape history of last map update
       integer   index_dash(50), index_solid(50), hist_date(3)
C Character variables
C string        --    general output character string
C c_title       --    title for plot
C c_date        --    date on which map was created (character string)
C program_name  --    program or routine used to create map
       character string*20, c_title*80, c_date*80, program_name*40
C character size
       real*4    ch_abs
C Count of levels plotted
       integer   cont_total_plotted
C debug control function
       logical   cmd_dblev
C test on drop in y
       real*4    wy_test

C check status on entry
       if (status.ne.0) return

C check for frame plotted (this must be the case for text to be added)
       if (.not.frame_done) return

C open frame
       call plt_frset(status)

C switch to deferred update state
       call pgbbuf

C define plot window and size
       call pgqwin(wx1,wx2,wy1,wy2)
       wx_size = wx2 - wx1
       wy_size = wy2 - wy1

C define extreme right hand side of the plot
       call pgqvp(0,vx1,vx2,vy1,vy2)
       vx_size = vx2 - vx1
       vy_size = vy2 - vy1
       a_val   = (wx1-wx2)/(vx1-vx2)
       b_val   = wx1 - a_val*vx1
       xend    = a_val*(view_port(2)-0.02*(view_port(2)-view_port(1)))
     *         + b_val

C save and reset plot size and font
       call pgqch(old_ch_size)
       ch_abs = max(wx_size/vx_size,wy_size/vy_size)/40.0
       call pgsch(mark_size)
       call pgqcf(old_font)
       call pgscf(mark_font)

C define inter-line spacing for text
       ydrop  = ch_abs*mark_size*1.05

C do actual plotting
C .. title
       if (title_opt .and. .not.title_done) then
         xstart = wx1
         ystart = wy2 + wy_size*0.025
         len_tit = lenb(title_plot)
         call pgptext(xstart,ystart,0.0,0.0,title_plot(1:len_tit))
         title_done = .true.
       end if

C .. contour levels
       if (cont_opt .and. .not.cont_done) then

C ... output title then levels
         ystart  = wy2 + wy_size*0.025 - ydrop
         wy_test = wy1 - (vy1-0.025)*(wy2-wy1)/(vy2-vy1)
         len_nam = lenb(name)
         len_uni = lenb(exunit)
         call pgptext(xend,ystart,0.0,1.0,name(1:len_nam))
         ystart = ystart - ydrop
         write(c_title,100)exunit(1:len_uni)
100      format('(',a,')')
         len_ctit = lenb(c_title)
         call pgptext(xend,ystart,0.0,1.0,c_title(1:len_ctit))
         ystart = ystart - ydrop

C ... sort levels
         if (cont_total_d.gt.1) then
           call qindxr(dashed_cont,index_dash,cont_total_d)
         else
           index_dash(1) = 1
         end if
         call qindxr(solid_cont,index_solid,cont_total_s)

C ... count levels
         cont_total_plotted = 0
         do nc = 1,cont_total_d
           if (dashed_done(index_dash(nc)).eq.1) then
             cont_total_plotted = cont_total_plotted + 1
           end if
         end do
         do nc = 1,cont_total_s
           if (solid_done(index_solid(nc)).eq.1) then
             cont_total_plotted = cont_total_plotted + 1
           end if
         end do
         write(string,150)cont_total_plotted
150      format(i3,' contours')
         call pgptext(xend,ystart,0.0,1.0,string(1:12))
         ystart = ystart - ydrop

C ... do the plotting
         do nc = 1,cont_total_d
           if (ystart.lt.wy_test) goto 10
           if (dashed_done(index_dash(nc)).eq.1) then
             call char_setlev(dashed_cont(index_dash(nc)),
     *                        string,len_s)
             call pgptext(xend,ystart,0.0,1.0,string(1:len_s))
             ystart = ystart - ydrop
           end if
         end do
         do nc = 1,cont_total_s
           if (ystart.lt.wy_test) goto 10
           if (solid_done(index_solid(nc)).eq.1) then
             call char_setlev(solid_cont(index_solid(nc)),
     *                        string,len_s)
             call pgptext(xend,ystart,0.0,1.0,string(1:len_s))
             ystart = ystart - ydrop
           end if
         end do
  10     cont_done = .true.
       end if

C .. date
       if (date_opt .and. .not.date_done) then
         ystart = wy2 + wy_size*0.025
         xstart = xend
         c_date = ' '
         call enhist(hist_date,program_name,status)
         len_prog = lenb(program_name)
         write(c_date,200)uv_range,hist_date,program_name(1:len_prog)
200      format('UV-range: ',4I5,' Created: ',
     *          I2,':',I2,':',I4,' By: ',A)
         len_date = lenb(c_date)
         call pgptext(xstart,ystart,0.0,1.0,c_date(1:len_date))
         date_done = .true.
       end if

C reset character size
       call pgsch(old_ch_size)
       call pgscf(old_font)

C return to immediate update state
       call pgebuf

C close frame
       call plt_frend(status)

C report any errors
       call cmd_err(status,'PLT_DOTEXT',' ')

       end
C
C
*+ plt_docont

       subroutine plt_docont(map_array,work_array,status)
C      --------------------------------------------------
C
C Plot the contours themselves on the plot
C
*-
$include (anmap)anmap-plot-sys:incl

C data arrays
       real*4    map_array(*), work_array(*)
C error flag and pointers
       integer   status, ip_redt, l, ic, ip_int, iint, test_size
C transformation array
       real*4    trans(6)
C contour levels max/min on map etc.
       real*4    work_max, work_min, cont_val, val_map
C UV-range of plot
       integer   uv_full(4), ius, ivs, length
C debug control function
       logical   cmd_dblev
C logical variable pspecifying that interpolation is actually used
       logical   use_interp

       if (status.ne.0) return

C check for frame
       if (.not.frame_init) then
         call plt_frinit(.true.,status)
       end if

C open frame
       call plt_frset(status)

C turn off immediate update buffering
       call pgbbuf

C if required interpolate the map data using the standard work array
C then define the matrix to transform to map coordinates appropriately
       call iuv_load(uv_full,status)
       test_size = abs( (uv_range(2)-uv_range(1)+1)
     *                 *(uv_range(3)-uv_range(4)+1) )

       use_interp = int_opt .and. test_size.le.(256*256)
       if ( use_interp ) then
C do interpolation to work_array
         call intmap(map_array,uv_range,work_array,ius,ivs,status)

C .. define transformation
         trans(1) = uv_range(1) - 0.5
         trans(2) = 0.5
         trans(3) = 0.0
         trans(4) = uv_range(3) + 0.5
         trans(5) = 0.0
         trans(6) = -0.5

       else
C .. define transformation for non-interpolated data
         trans(1) = uv_full(1) - 1
         trans(2) = 1.0
         trans(3) = 0.0
         trans(4) = uv_full(3) + 1
         trans(5) = 0.0
         trans(6) = -1.0
         ius = iu_size
         ivs = iv_size

       end if

C scan data array plotted for MAX and MIN
       work_min = 1.0e+30
       work_max = -1.0e+30
       length = ius*ivs
       if ( use_interp ) then
         print *,'** Doing interpolation size = ',length
         do l = 1,length
           val_map = work_array(l)
           if (val_map.gt.work_max) work_max = val_map
           if (val_map.lt.work_min) work_min = val_map
         end do

       else
         print *,'** Scanning map size = ',length
         do l = 1,length
           val_map = map_array(l)
           if (val_map.gt.work_max) work_max = val_map
           if (val_map.lt.work_min) work_min = val_map
         end do

       end if

C record plotting of specified levels
       do ic = cont_plot_d+1,cont_total_d
         cont_val = dashed_cont(ic)
         if (cont_val.ge.work_min .and. cont_val.le.work_max) then
           dashed_done(ic) = 1
         else
           dashed_done(ic) = -1
         end if
       end do
       do ic = cont_plot_s+1,cont_total_s
         cont_val = solid_cont(ic)
         if (cont_val.ge.work_min .and. cont_val.le.work_max) then
           solid_done(ic) = 1
         else
           solid_done(ic) = -1
         end if
       end do

C plot solid contours
       call pgsls(solid_style)
       call pgsci(solid_colour)
       call pgslw(solid_width)
       if ( use_interp ) then
         call pgconr(work_array,ius,ivs,1,ius,
     *               1,ivs,solid_cont(cont_plot_s+1),
     *               -(cont_total_s-cont_plot_s),trans)

       else
         call pgconr(map_array,ius,ivs,ipl1,ipl2,
     *               jpl1,jpl2,solid_cont(cont_plot_s+1),
     *               -(cont_total_s-cont_plot_s),trans)

       end if
       cont_plot_s = cont_total_s

C plot dash contours
       call pgsls(dash_style)
       call pgsci(dash_colour)
       call pgslw(dash_width)
       if ( use_interp ) then
         call pgconr(work_array,ius,ivs,1,ius,
     *               1,ivs,dashed_cont(cont_plot_d+1),
     *               -(cont_total_d-cont_plot_d),trans)

       else
         call pgconr(map_array,ius,ivs,ipl1,ipl2,
     *               jpl1,jpl2,dashed_cont(cont_plot_d+1),
     *               -(cont_total_d-cont_plot_d),trans)

       end if
       cont_plot_d = cont_total_d

C reset default style
       call pgsls(1)
       call pgsci(1)
       call pgslw(1)

C return to immediate update state
       call pgebuf

C close frame
       call plt_frend(status)

C report error messages
       call cmd_err(status,'PLT_DOCONT',' ')

       end
C
C
*+ plt_dogrey

       subroutine plt_dogrey(map_array,status)
C      ---------------------------------------
C
C Produce a grey scale plot (on most devices overwriting the screen)
C
*-
$include (anmap)anmap-plot-sys:incl

C data arrays
       real*4    map_array(*)
C error flag and pointers
       integer   status, ip_redt
C transformation array
       real*4    trans(6), grey_scale_buffer(2,256)
C device dependent scaling
       real*4    black, white
       character current_device_type*80
       integer   len_cdt
       logical   cmatch
C UV-range and size of plot
       integer   uv_full(4), ius, ivs
C counters etc.
       integer   i, j
C windows and view ports
       real*4    xv1, xv2, yv1, yv2, xw1, xw2, yw1, yw2
C character attributes
       real*4    old_ch_size
       integer   old_font
C string for label and length of the string
       character string*20
       integer   length

C check status on entry
       if (status.ne.0) return

C check for grey-scale option
       if (.not.grey_opt) return

C check for frame
       if (.not.frame_init) then
         call plt_frinit(.true.,status)
       end if
       call plt_frset(status)

C turn off immediate update buffering
       call pgbbuf

C define matrix to transform to map coordinates
       call iuv_load(uv_full,status)
       trans(1) = uv_full(1) - 1
       trans(2) = 1.0
       trans(3) = 0.0
       trans(4) = uv_full(3) + 1
       trans(5) = 0.0
       trans(6) = -1.0
       ius = iu_size
       ivs = iv_size

C do the grey-scale
       status = 0
       call pgqinf('TYPE',current_device_type,len_cdt)
       if (cmatch(current_device_type(1:len_cdt),'LEXIDATA')) then
         black = min(black_level,white_level)
         white = max(black_level,white_level)
       else
         black = max(black_level,white_level)
         white = min(black_level,white_level)
       end if
       call pggray(map_array,ius,ivs,ipl1,ipl2,jpl1,jpl2,
     *             black,white,trans)
       status = 0
C plot scale bar if required
       if (scale_bar_opt) then
C .. initialise scaled array for display
         do i=1,256
           do j=1,2
             grey_scale_buffer(j,i) = black +
     *                        (white-black)*float(i-1)/255.0
           end do
         end do
C .. reset the range of the plot -- save current defaults
         call pgqvp( 0, xv1, xv2, yv1, yv2 )
         call pgqwin( xw1, xw2, yw1, yw2 )
         call pgvport( min(xv1,xv1+(xv2-xv1)*scale_bar_width),
     *                 max(xv1,xv1+(xv2-xv1)*scale_bar_width),
     *                 yv1, yv2 )
         call pgwindow( 0.0, 2.0, 0.0, 256.0 )
         trans(1) = -0.5
         trans(2) = 1.0
         trans(3) = 0.0
         trans(4) = -0.5
         trans(5) = 0.0
         trans(6) = 1.0
         call pggray(grey_scale_buffer,2,256,1,2,1,256,
     *               black,white,trans)
         status = 0
         if (scale_bar_text) then
           call pgmove(0.0,0.0)
           call pgdraw(2.0,0.0)
           call pgdraw(2.0,256.0)
           call pgdraw(0.0,256.0)
           call pgdraw(0.0,0.0)
           call pgqch(old_ch_size)
           call pgsch(mark_size)
           call pgqcf(old_font)
           call pgscf(mark_font)
           call chrtoc(black,string,length)
           call pgmtext('L',0.1,0.0,0.0,string(1:length))
           call chrtoc(white,string,length)
           call pgmtext('L',0.1,1.0,1.0,string(1:length))
           call pgsch(old_ch_size)
           call pgscf(old_font)
         end if
         call pgvport( xv1, xv2, yv1, yv2 )
         call pgwindow( xw1, xw2, yw1, yw2 )
       end if

C return to immediate update state
       call pgebuf

C close the frame
       call plt_frend(status)

       call cmd_err(status,'PLT_DOGREY',' ')

       end
C
C
*+ plt_dosymb

       subroutine plt_dosymb(map_array,status)
C      ---------------------------------------
C
C Produce a symbol plot specifying certain data types
C
*-
$include (anmap)anmap-plot-sys:incl

C data arrays
       real*4    map_array(*)
C error flag and pointers
       integer   status
C counters
       integer   iu, iv
C data value
       real*4    x
C null data value
       real*4    blank_value
C character attributes
       real*4    size, old_size

C check status on entry
       if (status.ne.0) return

C check for grey-scale option
       if (.not.symbol_opt) return

C check for frame
       if (.not.frame_init) then
         call plt_frinit(.true.,status)
       end if
       call plt_frset(status)

C turn off immediate update buffering
       call pgbbuf

C do the plot
       call ennull(blank_value,status)
       call pgqch( old_size )
       size = 35.0/float(uv_range(3)-uv_range(4))
       call pgsch( size )
       if (status.ne.0) goto 999
       do iv=uv_range(3),uv_range(4),-1
         do iu=uv_range(1),uv_range(2)
           call iuvval(map_array,iu,iv,x,status)
           if (status.ne.0) goto 999
           if (x.eq.blank_value .and. symb_blank.gt.0) then
             call pgpoint(1,float(iu),float(iv),symb_blank)
           else if (x.ge.val_symb_max .and. symb_max.gt.0) then
             call pgpoint(1,float(iu),float(iv),symb_max)
           else if (x.le.val_symb_min .and. symb_min.gt.0) then
             call pgpoint(1,float(iu),float(iv),symb_min)
           end if
         end do
       end do
       call pgsch( old_size )

C return to immediate update state
999    call pgebuf

C close the frame
       call plt_frend(status)

       call cmd_err(status,'PLT_DOSYMB',' ')

       end
C
C
*+ plt_dovecs

       subroutine plt_dovecs(chi_map,int_map,status)
C      ---------------------------------------------
C
C Produce a grey scale plot (on most devices overwriting the screen)
C
*-
       $include (anmap)anmap-plot-sys:incl
       $include (library)constants:incl

C data arrays
       real*4    chi_map(*), int_map(*)
C error flag and pointers
       integer   status
C transformation array
       real*4    trans(6), length
C UV-range and size of plot
       integer   uv_full(4), ius, ivs
C blank value on CHI map
       real*4    blank_value


C check status on entry
       if (status.ne.0) return

C check for vectors option
       if (.not.vectors_opt) return

C check for frame
       if (.not.frame_init) then
         call plt_frinit(.true.,status)
       end if
       call plt_frset(status)

C turn off immediate update buffering
       call pgbbuf

C define matrix to transform to map coordinates
       call iuv_load(uv_full,status)
       trans(1) = uv_full(1) - 1
       trans(2) = 1.0
       trans(3) = 0.0
       trans(4) = uv_full(3) + 1
       trans(5) = 0.0
       trans(6) = -1.0
       ius = iu_size
       ivs = iv_size

C do the plotting
       status = 0
       length = vec_scale
       if (vec_type.ne.0) length = vec_length
       call ennull( blank_value, status )
       call pgvectors(ius,ivs,ipl1,ipl2,jpl1,jpl2,chi_map,int_map,
     *                vec_type,length,vec_u_samp,vec_v_samp,
     *                trans, vec_rotate*const_d2r,
     *                vec_gate, blank_value)
       status = 0

C return to immediate update state
       call pgebuf

C close the frame
       call plt_frend(status)

       call cmd_err(status,'PLT_DOVECS',' ')

       end
C
C
*+ plt_pips

       subroutine plt_pips(status)
C      ---------------------------
C
C Annotate frame with pips
C
C 1  --   define plot regions to have sides 100x(100xaspect_ratio)
C 2  --   setup map projection parameters (preces/stproj)
C 3  --   calculate map corners/pip positions
C 4  --   plot pips
C 5  --   restore PGPLOT environment
C 6  --   restore map-projection environment
C
C (PA/EMW  17 Oct 87)
C (DJT     12 Apr 88)
C
*-
$include (library)constants:incl
$include (library)maplib-redtape:incl
$include (anmap)anmap-plot-sys:incl

       character  label*16
       real*8     pipval(32)
       real       pdist(32), pipinr, pipind
       integer    piptyp, prsec
       integer    status
       real*4     vpx1, vpx2, vpy1, vpy2, ratio_x, ratio_y, ratio_ch,
     *            ratio_u
       logical    cmd_dblev

       if (status.ne.0) return

C define region to plot
       call pgqwin(x1,x2,y1,y2)
       call pgqvp(0,vpx1,vpx2,vpy1,vpy2)
       ratio_x = (vpx2-vpx1)/0.9
       ratio_y = (vpy2-vpy1)/0.9
       ratio_u = max(ratio_y,ratio_x)
       size_x = x2-x1
       size_y = y2-y1
       size_m = max(size_x,size_y)
       size_x = 100.0*size_x/size_m
       size_y = 100.0*size_y/size_m
       call pgwindow(0.0,size_x,0.0,size_y)
       call pgsch(mark_size)
       call pgscf(mark_font)
       ratio_ch = mark_size/1.0

C setup map projection parameters
       call stproj(iproj,0,usamp,skew,ramap,decmap,refdat,epoch,status)

C find pip intervals in sec/arcsec
       pipinr=grid_ra*60.0
       pipind=grid_dec*60.0

C SIDE 1
       call pipsc(uv_range,1,1,pipinr,pdist,pipval,npip,status)
       if (status.eq.0) then
         do i=1,npip
           xpos=pdist(i)*size_x
           ypos=size_y
           piptyp=nint(pipval(i)/pipinr)
           piptyp=iabs(mod(piptyp,2))+1
           if (npip.lt.4) piptyp=1
           pipsiz=2.0*pip_size/float(piptyp)
           call pgmove(xpos,ypos)
           call pgdraw(xpos,ypos-pipsiz)
         enddo
       endif

C SIDE 2
       prsec=0
       if (pipind.lt.1.0) prsec=2
       call pipsc(uv_range,2,2,pipind,pdist,pipval,npip,status)
       if (status.eq.0) then
         do i=1,npip
           xpos=size_x
           ypos=(1.0-pdist(i))*size_y
           piptyp=nint(pipval(i)/pipind)
           piptyp=iabs(mod(piptyp,2))+1
           if (npip.lt.4) piptyp=1
           pipsiz=2.0*pip_size/float(piptyp)
           call pgmove(xpos,ypos)
           call pgdraw(xpos-pipsiz,ypos)
           if (piptyp.eq.1) then
             call chdtos(pipval(i)/3600.d0,prsec,label,ll)
             call pgptext(xpos+0.5*ratio_ch/ratio_u,
     *                    ypos-1.2*ratio_ch/ratio_u,
     *                    0.0,0.0,label(1:ll))
           endif
         enddo
       endif

C SIDE 3
       prsec=0
       if (pipinr.lt.1.0) prsec=2
       call pipsc(uv_range,3,1,pipinr,pdist,pipval,npip,status)
       if (status.eq.0) then
         do i=1,npip
           xpos=pdist(i)*size_x
           ypos=0.0
           piptyp=nint(pipval(i)/pipinr)
           piptyp=iabs(mod(piptyp,2))+1
           if (npip.lt.4) piptyp=1
           pipsiz=2.0*pip_size/float(piptyp)
           call pgmove(xpos,ypos)
           call pgdraw(xpos,ypos+pipsiz)
           if (piptyp.eq.1) then
             call chdtos(pipval(i)/3600.d0,prsec,label,ll)
             call pgptext(xpos,-4.0*ratio_ch/ratio_u,
     *                    0.0,0.5,label,ll)
           endif
         enddo
       endif

C SIDE 4
       call pipsc(uv_range,4,2,pipind,pdist,pipval,npip,status)
       if (status.eq.0) then
         do i=1,npip
           xpos=0.0
           ypos=(1.0-pdist(i))*size_y
           piptyp=nint(pipval(i)/pipind)
           piptyp=iabs(mod(piptyp,2))+1
           if (npip.lt.4) piptyp=1
           pipsiz=2.0*pip_size/float(piptyp)
           call pgmove(xpos,ypos)
           call pgdraw(xpos+pipsiz,ypos)
         enddo
       endif

C reset map projection parameters for accurate position calculations
       call stproj(iproj,1,usamp,skew,ramap,decmap,refdat,epoch,status)

C restore PGPLOT environment
       call pgwindow(x1,x2,y1,y2)
       call pgsch(1.0)
       call pgscf(1)

       end
C
C
*+ plt_grid

       subroutine plt_grid(status)
C      ---------------------------
C
C Add grid to frame
C
C 1  --   save PGPLOT environment
C 2  --   setup map projection parameters (preces/stproj)
C 3  --   set up grid intervals
C 4  --   calculate RA,Dec on a regular grid
C 5  --   plot Dec lines using contours
C 6  --   plot RA lines using polylines
C 7  --   restore map-projection environment
C 8  --   restore PGPLOT environment
C
C To cater for maps including the North-Pole, Dec grid lines are
C displayed by contouring a regular grid of Dec values, and RA grid
C lines by tracing polylines from minimum Dec to maximum Dec.
C
C (DJT, 17 March 88)
C
*-
       integer    nstep
       parameter (nstep=51)
       real*4     xc(nstep),yc(nstep)
       real*4     dec_grid(nstep,nstep)

       real*4     trans(6)
       data       trans / 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 /

$include (anmap)anmap-plot-sys:incl
$include (library)constants:incl
$include (library)maplib-redtape:incl

       REAL*8     RA,DEC,RACP,DECCP
       real*8     rac(4),decc(4),ramnx(2),decmnx(2)
       real*8     ra_max,ra_min,dec_max,dec_min,dec_pole
       real*8     ra_step,ra_zero,dec_step,dec_zero
       real*8     u,v,u1,v1,ustep,vstep

       integer    status

       logical    style_set, colour_set

       if (status.ne.0) return

       if (grid_style.lt.0) then
         style_set = .true.
         grid_style=4
         if (index(output_device,'BENSON').ne.0) grid_style=1
       else
         style_set = .false.
       end if
       if (grid_colour.lt.0) then
         colour_set = .true.
         grid_colour=1
       else
         if (index(output_device,'BENSON').ne.0) grid_colour=3
         if (index(output_device,'LEXIDATA').ne.0) grid_colour=200
         colour_set = .false.
       end if

       call pgqwin(x1,x2,y1,y2)
       call pgqci(icol)
       call pgqls(isty)

C set up map projection parameters
       call stproj(iproj,0,usamp,skew,ramap,decmap,refdat,epoch,status)

C define region to plot
       size_x = x2-x1
       size_y = y2-y1
       size_m = max(size_x,size_y)
       size_x = 100.0*size_x/size_m
       size_y = 100.0*size_y/size_m
C
C find pip intervals, in secs/arcsecs
       pipinr=grid_ra*60.0
       pipind=grid_dec*60.0

C calculate RA, Dec on a regular grid.  DEC_GRID holds reduced
C values to improve precision.

       ra_min=1.e64
       ra_max=-ra_min
       dec_min=ra_min
       dec_max=ra_max
       ra_step=pipinr*const_st2r
       dec_step=pipind*const_sa2r

C allow for fractional map centre
       u1=uv_range(1)-xmc-iumap1+1
       v1=uv_range(4)+ymc-ivmap1-1
       ustep=dfloat(uv_range(2)-uv_range(1))/(nstep-1)
       vstep=dfloat(uv_range(3)-uv_range(4))/(nstep-1)

       do iu=1,nstep
         u=u1+(iu-1)*ustep
         do iv=1,nstep
           v=v1+(iv-1)*vstep
           call uvtord(u,v,racp,deccp,status)
           if (status.ne.0) return
           call preces(racp,deccp,ra,dec,1950.0d+0-epoch)
           if (iu.eq.1 .and. iv.eq.1) then
             ra_zero=dint(ra/ra_step)
             dec_zero=dint(dec/dec_step)
           endif
           ra_min=min(ra,ra_min)
           ra_max=max(ra,ra_max)
           dec_min=min(dec,dec_min)
           dec_max=max(dec,dec_max)
           dec_grid(iu,iv)=dec/dec_step-dec_zero
         enddo
       enddo

C set up contour level array for Dec grid lines
       idec1=dec_min/dec_step-dec_zero
       idec2=dec_max/dec_step-dec_zero
       if (dec_min.lt.0.d0) idec1=idec1-1
       if (dec_max.lt.0.d0) idec2=idec2-1
       nlev=min(idec2-idec1+1,nstep)
       do i=1,nlev
         xc(i)=idec1+i-1
       enddo

C set up window for contouring and draw Dec grid lines
       call pgbbuf
       call pgsci(grid_colour)
       call pgsls(grid_style)
       size_x = float(nstep)
       size_y = float(nstep)
       call pgwindow(1.0,size_x,1.0,size_y)
       call pgconr(dec_grid,nstep,nstep,1,nstep,1,nstep,xc,-nlev,trans)

C restore U,V window
       call pgwindow(x1,x2,y1,y2)

C set up polylines and draw RA grid lines
       ira1=ra_min/ra_step
       ira2=ra_max/ra_step
       dec_pole=90.d0*const_d2r
       dec_min=max(-dec_pole,dec_min)
       dec_max=min(dec_pole,dec_max)
       dec_step=(dec_max-dec_min)/(nstep-1)
       do i=ira1,ira2
         ra=i*ra_step
         do j=1,nstep
           dec=dec_min+(j-1)*dec_step
           call preces(ra,dec,racp,deccp,epoch-1950.0d+0)
           call rdtouv(racp,deccp,u,v,status)
           xc(j)=u
           yc(j)=v
         enddo
         call pgline(nstep,xc,yc)
       enddo

C reset map projection parameters for accurate position calculations
       call stproj(iproj,1,usamp,skew,ramap,decmap,refdat,epoch,status)

C restore colour index
       call pgsci(icol)
       call pgsls(isty)
       if (style_set)  grid_style  = -1
       if (colour_set) grid_colour = -1
       call pgebuf

       end
C
C
*$ 3) Cursor Input Routines
C
C
*+ plt_getuv

       subroutine plt_getuv (prompt, default, iuv, status)
C      ---------------------------------------------------
C
C  Prompts for a map U,V coordinate window.
C
C  Given:
C      PROMPT    char*(*)    prompt string
C      DEFAULT   char*(*)    default input string
C
C  Returned:
C      IUV       integer(4)  U,V coordinate window
C      STATUS    integer     status
C
C  Prompts on the input device for a map area, specified as four integers,
C  (IU1,IU2,IV1,IV2).  If only one integer is entered then the square area
C  (-IU1,IU1,IU1,-IU1) is returned.  Note that the ordering convention
C   IU1<IU2 and IV1>IV2  is enforced if necessary.  If the redtape common
C  blocks contain parameters for a current map, then checks are also made
C  that the area lies within the map bounds.  If the input string is null,
C  the DEFAULT string is used as input.  If the default string is null or
C  '*', the current values of IUV are used as default.
C
C  The user may optionally specify "CURSOR" in response to the prompt and
C  if then a map is plotted on the current plot device (and open) then
C  the UV-range may be specified by positioning the cursor on the graphics
C  screen in the usual manner.
C
C  The STATUS value should be zero on entry:  the returned status value
C  is zero if successful.  Other possible values are:
C
C      - invalid u,v window (ILL_UVWIND)
C      - u,v window outside map (UV_OUTMAP)
C      - unexpected SINTRAN I/O error, or user break
C
C  (DJT, 14 October 86)
C  (PA,  22 January 88)
C
*-
       character*(*)  prompt, default
       integer        iuv(4), status, minirt(8)
       call enminirt( minirt, status )
       call plt_getuv2(minirt,prompt,default,iuv,status)
       end
C
C
*+ plt_getuv2

       subroutine plt_getuv2 (minirt, prompt, default, iuv, status)
C      ------------------------------------------------------------
C
C  Prompts for a map U,V coordinate window.
C
C  Given:
C      MINIRT    integer(8)  map mini-redtape
C      PROMPT    char*(*)    prompt string
C      DEFAULT   char*(*)    default input string
C
C  Returned:
C      IUV       integer(4)  U,V coordinate window
C      STATUS    integer     status
C
C  Prompts on the input device for a map area, specified as four integers,
C  (IU1,IU2,IV1,IV2).  If only one integer is entered then the square area
C  (-IU1,IU1,IU1,-IU1) is returned.  Note that the ordering convention
C   IU1<IU2 and IV1>IV2  is enforced if necessary.  If the redtape common
C  blocks contain parameters for a current map, then checks are also made
C  that the area lies within the map bounds.  If the input string is null,
C  the DEFAULT string is used as input.  If the default string is null or
C  '*', the current values of IUV are used as default.
C
C  The user may optionally specify "CURSOR" in response to the prompt and
C  if then a map is plotted on the current plot device (and open) then
C  the UV-range may be specified by positioning the cursor on the graphics
C  screen in the usual manner.
C
C  The STATUS value should be zero on entry:  the returned status value
C  is zero if successful.  Other possible values are:
C
C      - invalid u,v window (ILL_UVWIND)
C      - u,v window outside map (UV_OUTMAP)
C      - unexpected SINTRAN I/O error, or user break
C
C  (DJT, 14 October 86)
C  (PA,  22 January 88)
C  (PA,  17 December 91)
*-
       character*(*)  prompt, default
       character*20   string
       integer        iuv(4), iuv_n(4), iu1, iu2, iv1, iv2, status
       integer        iu, iv, ls, errdev, minirt(8)
       logical        batch, cmatch, verify_value, cursor_available

$include (library)sintran-errors:incl
$include (library)maplib-errors:incl
$include (anmap)anmap-plot-sys:incl

        equivalence (iuv_n(1),iu1),(iuv_n(2),iu2),(iuv_n(3),iv1),
     *              (iuv_n(4),iv2)

    1  if (status.eq.0) then

         write(string,'(3(i4,'',''),i4)')iuv

C    Read U,V window from command line

         call getwrd(prompt,default,string,ls,status)
         if (status.ne.0) then
           call cmd_err(status,'GETUV',' ')
           return
         end if

C Look for "CURSOR" response
         call chucas(string)
         if (cmatch(string(1:ls),'CURSOR')) then

           call pgqcur(cursor_available)
           if (frame_init .and. cursor_available) then

C ... look for cursor input
             verify_value = .true.
             call wrout('.. mark the UV-range with the graphics cursor')
             call wrout('   mark opposite corners of the window')
             call wrout('   type any character to enter or a Q to quit')
             call plcu_uvwin(iuv_n,status)

          else

C ... no plot on output device therefore do not use cursor input
             call cmd_wrerr('UV-POSITION',
     *                       'NO plot frame or NO cursor available')
             status = ill_uvwind

           end if

         else if (ls.gt.0) then

C .. normal terminal prompt
           verify_value = .true.
           call chctoi(string(1:ls),iu1,status)
           call nxtwrd(string,ls,status)
           if (ls.gt.0) then
             call chctoi(string(1:ls),iu2,status)
             call nxtwrd(string,ls,status)
             call chctoi(string(1:ls),iv1,status)
             call nxtwrd(string,ls,status)
             call chctoi(string(1:ls),iv2,status)
           else
             iu1=-iu1
             iu2=-iu1
             iv1=iu2
             iv2=iu1
             ls=1
           endif
         end if
C
C    Check for validity
C
         if (verify_value) then

           if (ls.eq.0 .or. status.eq.bad_char) then
             call wrout('*** bad syntax,  U1,U2,V1,V2  wanted')
             status=ill_uvwind
           else
C
C    Enforce IU1 < IU2 and IV1 > IV2
C
             if (iu1.gt.iu2) then
               iu=iu1
               iu1=iu2
               iu2=iu
             endif
             if (iv1.lt.iv2) then
               iv=iv1
               iv1=iv2
               iv2=iv
             endif
C
C    Check against map bounds
C
               if (iu1.lt.minirt(1) .or. iu2.gt.minirt(2) .or.
     :             iv1.gt.minirt(3) .or. iv2.lt.minirt(4) ) then
                 call enqerr( errdev )
                 write(errdev,'(x,a,4i6)')
     :             '*** area outside map, restrict to :',
     :             minirt(1),minirt(2),minirt(3),minirt(4)
                 status=uv_outmap
               endif
           endif

           if (status.eq.ill_uvwind .or. status.eq.uv_outmap) then
             call enqbch( batch )
             if (.not.batch) status=0
             call setcli(' ')
             goto 1
           endif

           iuv(1)=iu1
           iuv(2)=iu2
           iuv(3)=iv1
           iuv(4)=iv2

         endif
       endif

       end
C
C
*+ plt_getpos2

       subroutine plt_getpos2(minirt,prompt,default,uv_pos,status)
C      -----------------------------------------------------------
C
C Read a pixel position - optionally using the cursor
C
C Input
C     MINIRT       -     I4(8)      -     map mini redtape
C     Prompt       -     C*(*)      -     prompt string
C     Default      -     C*(*)      -     default string
C
C Returned
C     UV_POS       -     R4(2)      -     uv-position (real)
C     STATUS       -     I4         -     error return code
C
C The user is prompted for a UV position. If the answer supplied is
C CURSOR then the UV position is read from the creen by positioning the
C graphics CURSOR.
C
C [PA, January 1988]
*-
$include (anmap)error-file-full:incl
$include (library)chrlib-functions:incl

       character*(*)   prompt, default
       character*80    string, char*1
       integer         status, pgcurse, minirt(8)
       logical         cursor_available
       real*4          x, y, uv_pos(2)

C read the command line
       if (status.ne.0) return
       string = ' '
       write(string,'(F8.1,F8.1)') uv_pos
       call getstr(prompt,default,string,status)
       if (status.ne.0) then
         call cmd_err(status,'UV-POSITION',' ')
         return
       end if

C test for 'cursor' input mode
       call chucas(string)
       if (cmatch(string(1:lenb(string)),'CURSOR')) then
         cursor_available = .false.
         call pgqcur(cursor_available)
         if (.not.cursor_available) then
           call cmd_wrerr('UV-POSITION',
     *                     'Device does not support cursor input')
           status = ill_curse
           return
         end if
         call plt_frset(status)
         status = pgcurse(x,y,char) - 1
         call plt_frend(status)
         call chucas(char)
         if (char.eq.'Q') then
           call cmd_wrerr('UV-POSITION','Input abandoned')
           status = ill_inter
           return
         else if (status.ne.0) then
           call cmd_wrerr('UV-POSITION','Error reading cursor')
           status = ill_curse
           return
         end if

       else

C .. normal input
         call setcli(string(1:lenb(string)))
         call getr(' ',' ',x,status)
         call getr(' ',' ',y,status)
         if (status.ne.0) then
           call cmd_err(status,'UV-POSITION','Error in input position')
           status = ill_pos
           return
         end if
       end if
       if (x.lt.float(minirt(1)) .or. x.gt.float(minirt(2)) .or.
     *     y.lt.float(minirt(4)) .or. y.gt.float(minirt(3))) then
         status = ill_pos
         call cmd_wrerr('UV-POSITION','Position outside map')
         return
       end if
       uv_pos(1) = x
       uv_pos(2) = y

       end
C
C
*+ plt_rdpos

       subroutine plt_rdpos(prompt,uv_pos,status)
C      ------------------------------------------
C Read a map position optionally using the cursor
C
       character*(*)   prompt
       real*4          uv_pos(2)
       integer         status, minirt(8)
       call enminirt(minirt,status)
       call plt_getpos2(minirt,prompt,'cursor',uv_pos,status)
       end
C
*+ plcu_uvwin

       subroutine plcu_uvwin(uv_win,status)
C      ------------------------------------
C
C Read a UV-window from the plot frame
*-
       integer   status, uv_win(4), uv_pos1(2), uv_pos2(2), i_area

       if (status.ne.0) return

       uv_pos1(1) = uv_win(1)
       uv_pos1(2) = uv_win(3)
       call plcu_uvpos(uv_pos1,status)
       uv_pos2(1) = uv_pos1(1)
       uv_pos2(2) = uv_pos1(2)
       call plcu_uvpos(uv_pos2,status)
       if (istat.eq.0) then
         uv_win(1) = min(uv_pos1(1),uv_pos2(1))
         uv_win(2) = max(uv_pos1(1),uv_pos2(1))
         uv_win(3) = max(uv_pos1(2),uv_pos2(2))
         uv_win(4) = min(uv_pos1(2),uv_pos2(2))
         i_area = (uv_win(1)-uv_win(2))*(uv_win(3)-uv_win(4))
         if (i_area.eq.0) status = ill_onpic
       end if
       end
C
C
*+ plcu_pos

       subroutine plcu_pos(pos,status)
C      -------------------------------
C
C Read a valid position from the screen
C
C Returned
C   POS          -       2R4        -      cursor-position (world
C                                          coordinates).
C   STATUS       -        I4        -      error return code
C
C An attempt is made to look for an ATTN interupt from the user.
C Possible error codes:
C
C    USR_BREAK
C    any PGPLOT error code on reading the cursor position.
*-
$INCLUDE (anmap)anmap-plot-sys:INCL

       integer       pgcurse, pos(2), status
       character     char*1
       logical       attn

       if (status.ne.0) return

       xpos = pos(1)
       ypos = pos(2)

       if (attn(status)) return
       status = pgcurse(xpos,ypos,char)
       status = status - 1
       if (status.eq.0) then
         if (attn(status)) return
         pos(1) = xpos
         pos(2) = ypos
       end if
       end
C
C
*+ plcu_uvpos

       subroutine plcu_uvpos(uv_pos,status)
C      ------------------------------------
C
C Read a valid UV-position off the plot frame
C
C Returned
C   UV_POS       -       4R4        -      cursor position
C   STATUS       -        I4        -      error return code
C
C An attempt is made to look for an ATTN interup from the user.
C Possible error codes:
C
C    ILL_ONPIC
C    USR_BREAK
C    any PGPLOT error code on reading the cursor position.
*-
$include (anmap)anmap-plot-sys:incl

       integer       pgcurse, uv_pos(2), status
       character     char*1
       logical       attn

       if (status.ne.0) return

       xpos = uv_pos(1)
       ypos = uv_pos(2)

       if (attn(status)) return
       call plt_frset(status)
       status = pgcurse(xpos,ypos,char)
       status = status - 1
       call plt_frend(status)
       if (status.eq.0) then
         if (attn(status)) return
         uv_pos(1) = nint(xpos)
         uv_pos(2) = nint(ypos)
         if   (uv_pos(1).ge.uv_range(1) .and. uv_pos(1).le.uv_range(2)
     *   .and. uv_pos(2).ge.uv_range(4) .and. uv_pos(2).le.uv_range(3))
     *     then
           status = 0
         else
           status = ill_onpic
         end if
       end if
       end
C
C
*+ plcu_read

       subroutine plcu_read(map_array,status)
C      --------------------------------------
C
C Read cursor position and return UV, RA/DEC and value
C
C This is a high level routine and may be called at any time. The routine
C checks that a valid plot is open. The output is sent to the terminal
C and records the nearest UV pixel and value, the interpolated position
C and map value at the cursor location and finally the RA and DEC of this
C location using the redtape of the displayed map.
C
*-
$include (anmap)anmap-plot-sys:incl
$include (library)maplib-redtape:incl
$include (library)constants:incl

       character    ch_typed*1, string_int*14, string_pix*14,
     *              string_pb*14
       integer      status
       integer      len_val_int, len_val_pix, len_val_pb
       real*8       du, dv, ra, dec, ra0, dec0, dpos_u, dpos_v
       real*4       value_int, value_pix, pb_value, pos_u, pos_v
       real*4       wx1, wx2, wy1, wy2
       real*4       map_array(*)
C
       integer      pgcurse
       logical      cursor_available, cmd_dblev

       if (status.ne.0) return

       call pgqcur(cursor_available)
       if (.not.cursor_available) then
         call cmd_wrerr('CURSOR-POSITION',
     *                   'Device does not support input')
         return
       end if
       if (.not.plot_open) then
         call cmd_wrerr('CURSOR-POSITION',
     *                   'Plot not open - unable to access cursor')
         return
       end if

C .. find initial position for cursor
       call wrout('.. move cursor: type any character to end')
       call plt_frset(status)
       call pgqwin(wx1,wx2,wy1,wy2)
       pos_u = (wx1+wx2)/2.0
       pos_v = (wy1+wy2)/2.0

C .. read cursor position
       status = pgcurse(pos_u,pos_v,ch_typed) - 1
       if (status.ne.0) then
         call cmd_wrerr('CURSOR-POSITION','Error reading CURSOR')
         return
       end if

C determine map value at cursor position and validity
       call enqout(iout)
       dpos_u = pos_u
       dpos_v = pos_v
       call ruvval(map_array,dpos_u,dpos_v,2,value_int,status)
       call iuvval(map_array,nint(pos_u),nint(pos_v),
     *             value_pix,status)
       call cmd_err(status,'cursor-position','outside map')

C return if point not on map
       if (status.ne.0) return

C output information to the terminal
       call chrtoc(value_int,string_int,len_val_int)
       call chrtoc(value_pix,string_pix,len_val_pix)
       len_uni = lenb(exunit)
       du = pos_u
       dv = pos_v
       if (cmd_dblev(3)) then
         call prproj(1,status)
       end if
       call uvtord(du,dv,ra0,dec0,status)
       call precrd(epoch,ra0,dec0,1950.0d0,ra,dec)
       call pbcorr(ra0,dec0,rapnt,decpnt,rteles,
     *             pb_value,status)
       if (pb_value.gt.0.0) then
         call chrtoc(value_int/pb_value,string_pb,len_val_pb)
       else
         call chrtoc(0.0,string_pb,len_val_pb)
       end if
       call frrads('HMS',ra,i_hour,i_min_time,sec_time)
       call frrads('DMS',dec,i_deg,i_min_arc,sec_arc)
       write(iout,100)string_int(1:len_val_int),
     >                exunit(1:len_uni),pos_u,pos_v,
     >                string_pix(1:len_val_pix),
     >                exunit(1:len_uni),nint(pos_u),nint(pos_v),
     >                i_hour,i_min_time,sec_time,
     >                i_deg,i_min_arc,sec_arc,
     >                string_pb(1:len_val_pb),exunit(1:len_uni),
     >                pb_value

 100   format(1x/
     >           1X,'.. Interpolated value = ',A,' ',A,' at',2F8.1/
     >           1X,'.. Pixel value        = ',A,' ',A,' at',2I6/
     >           1X,'.. Position (1950.0)  = ',
     >           I3,1X,I2,1X,F7.3,I5,1X,I2,1X,F7.2/
     >           1X,'.. PB corrected value = ',A,' ',A,'  PB = ',F6.2/)

       call plt_frend(status)

       end
C
C
C
C
*$ 4) Support Routines for the Plotting Package
C
*+ ctr_getlev

       subroutine ctr_getlev(region_max, region_min,
     *                       c_levs, n_levs, status)
C      ---------------------------------------------
C
C  Input contour levels, in one of several formats.
C
C    - a number NC of equispaced levels between data min and max
C    - starting level, contour interval and last level in ext.units
C    - starting level, mult.factor and last level in ext.units
C    - contour levels specified explicitly in external units
C    - contour levels specified explicitly as % of maximum value
C
C
C A list of contour levels is retuend in C_LEVS up to a maximum
C N_LEVS. REGION_MAX is simply the maximum data values in the chosen
C UV_range and should be passed to the routine as should REGION_MIN
C
C C_LEVS should have dimension at least 100 in the calling routine.
C
C This routine provides contour input as found in the system contours
C program.
C-
       integer    status
       integer    ncont
       parameter (ncont=50)
       real       eps, facint
       parameter (eps=1.e-6)
       parameter (facint=1.3)
C
       logical    yesno
C
       real       zc(ncont), z(3), zrange, ztop, dz, fz
       integer    i, ic, ipc, m, mc, nc, lenb
       character  string*60
C
       real*4     c_levs(1), region_max, region_min
       integer    n_levs
C
       save       mc
C
$include (library)maplib-redtape:incl
C
       if (status.ne.0) return
C
       nc=0
       ipc=0
C
C  FACINT allows for max.value up to 1.3*ZMAX after interpolation
C
       ztop=region_max*facint
C
C  Equispaced contour levels
C
       call geti(
     :' Number of equispaced contour levels, or press return : ',
     :                                                    ' ',nc,status)
C
       if (nc.gt.0) then
         nc=min0(nc,ncont)
         dz=(region_max-region_min)/(2.*nc+1)
         zc(1)=region_min+dz
         do ic=2,nc
           zc(ic)=zc(ic-1)+2.*dz
         enddo
         ipc=1
       else
C
C  Linear contour levels
C
         write(string,'(3a)')
     :     ' First level, increment, last level in ',
     :      exunit(1:lenb(exunit)),' : '
         call ctr_getnr(string,' ',z,3,m,status)
         if (m.gt.0) then
           nc=1
           dz=0.
           zc(nc)=z(1)
           if (m.gt.1) dz=z(2)
           if (m.gt.2) ztop=amin1(ztop,z(3))
           if (dz.gt.0.0) then
    1        if (nc.lt.ncont .and. zc(nc)+dz.lt.ztop+eps) then
               zc(nc+1)=zc(nc)+dz
               nc=nc+1
               goto 1
             endif
           endif
           ipc=1
         else
C
C  Logarithmic contour levels
C
           call ctr_getnr(
     :      'First level, factor, last level :',' ',z,3,m,status)
           if (m.gt.0) then
             nc=1
             fz=1.0
             zc(nc)=z(1)
             if (m.gt.1) fz=z(2)
             if (m.gt.2) ztop=amin1(ztop,z(3))
             if (fz.gt.1.0) then
    2          if (nc.lt.ncont .and. zc(nc)*fz.lt.ztop+eps) then
                 zc(nc+1)=zc(nc)*fz
                 nc=nc+1
                 goto 2
               endif
             endif
           else
C
C  Explicit contour levels
C
             write(string,'(3a)')
     :         ' Contour levels in ',EXUNIT(1:LENB(EXUNIT)),' : '
             call ctr_getnr(string,' ',zc,ncont,nc,status)
             if (nc.eq.0 .and. status.eq.0) then
C
C  Contour levels as percentages of maximum
C
               call ctr_getnr('Contour levels as % : ',' ',zc,ncont,nc,
     :                                                           status)
               if (nc.gt.0) then
                 do ic=1,nc
                   zc(ic)=zc(ic)*(zmax-zerol)*scalef/100.
                 enddo
               elseif (status.eq.0) then
                 nc=mc
               endif
C
             endif
           endif
         endif
       endif
C
       mc=nc
C
       if (nc.eq.0 .or. status.ne.0) return
C
C
C  Sort the contour levels and remove redundancies
C
       call qsortr(zc,nc)
       zrange=zc(nc)-zc(1)
C
       i=0
       ic=0
    3  i=i+1
       if (i.gt.nc) goto 4
       if (zc(i).lt.region_min) goto 3
       if (zc(i).gt.region_max*facint) goto 4
       if (i.gt.1 .and. abs(zc(i)-zc(i-1)).lt.eps*zrange) goto 3
       if (ipc.eq.1 .and. abs(zc(i)).lt.eps*zrange) then
         if (.not.yesno(
     :     '... Do you want to include the zero level? ',' ',STATUS))
     :                                                           goto 3
         zc(i)=0.0
       endif
       zc(ic+1)=zc(i)
       ic=ic+1
       goto 3
C
C  Report the chosen contour levels
C
    4  nc=ic
       if (nc.eq.0) then
         write(*,'(/A)')'No contour levels specified'
       elseif (nc.eq.1) then
         write(*,'(/i3,a,1pe12.4,x,a)')
     :                     nc,' contour at',zc(1),exunit(1:lenb(exunit))
       else
         write(*,'(/i3,a,/(1p5e12.4))')
     :                     nc,' contours at',(zc(ic),ic=1,nc)
       endif
       write(*,*)
C
C  Set up the output arrays
C
C
       n_levs = 0
       do ic=nc,1,-1
         n_levs = n_levs+1
         c_levs(n_levs) = zc(ic)
       enddo
C
       end
C
C
*+ ctr_getnr

       subroutine ctr_getnr (prompt, default, values, n, m, status)
C      ------------------------------------------------------------
C
C  Reads a set of real numbers from the command line.
C
C  Given:
C      PROMPT    char*(*)    prompt string
C      DEFAULT   char*(*)    default input string
C      N         integer     max.no of values to read
C
C  Returned:
C      VALUES    real*(*)    real array
C      M         integer     number of values actually read
C      STATUS    integer     status value
C
C  Reads up to N words from the command line, prompting for more input
C  if necessary.  The input is decoded as real constants, and these values
C  are returned in the VALUES array.  If the input string is null, the
C  DEFAULT string is taken as input.  If the default string is null or
C  '*', the current contents of the value array remain unchanged by
C  default.
C
C  The STATUS value should be zero on input: the returned status value
C  is zero, unless an I/O error or user break has been detected, or the
C  input string does not contain valid real constants.
C
*-
       character*(*) prompt, default
       character*32  string
       integer       n, m, status
       real          values(n), value
       integer       ls
C
$INCLUDE (LIBRARY)IOLIB-ERRORS:INCL
C
       character*18  errmsg
       parameter   ( errmsg = 'real number wanted' )
C
       m=0
    1  if (n.gt.0 .and. status.eq.0) then
C
         call getwrd(prompt,default,string,ls,status)
C
         do while (ls.gt.0 .and. status.eq.0)
           call chucas(string(1:ls))
           call chctor(string(1:ls),value,status)
           call nxtwrd(string,ls,status)
           if (m.lt.n .and. status.eq.0) then
             values(m+1)=value
             m=m+1
           endif
         enddo
C
C      Check for syntax error
C
         if (status.gt.0) then
           status=bad_syntax
           call wrmsg(status,errmsg)
           call setcli(' ')
           m=0
           goto 1
         endif
C
       endif
C
       end
C
C
*+ char_setlev

       subroutine char_setlev(value,string,len_str)
C      --------------------------------------------
C
C choose a character representation for the contour level
*-
       character*(*)   string
       integer         len_str
       integer         ival_log, ival_num
       real*4          value

C prepare number
       if (abs(value).lt.1.0e-25) then
         ival_log = 3
         ival_num = 0
       else
         ival_log = nint(log10(abs(value)))
         ival_num = nint(value*(10.0**(3-ival_log)))
       end if

C convert to character string
       call pgnumb(ival_num,ival_log-3,2,string,len_str)

       end
C
C
