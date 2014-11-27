*$ Map-Display Routine Library
*  ===========================
C
C P. Alexander MRAO, Cambridge.
C
*+ plot_sys

       subroutine plot_sys(i_command,map_array,status)
C      -----------------------------------------------
C
C Interactive contouring and map plotting
C
C A command line driven map plotting sub system.  The calling program
C must use the MAP-CATALOGUE and stack system.  The work array is
C passed by argument to this routine.  All plotting uses the PGPLOT library.
C
C Update Log.
C -----------
C
C 17/9/87:  Defined basic commands etc. and simple (non-plotting) routines.
C 18/9/87:  More routines added, some commands changed.
C 19/9/87:  Corrections to Contour input (new method) and scaling plot.
C 20/9/87:  Corrections to frame scaling and contouring + general.
C 21/9/87:  Addition of text plotting - corrected dashed-contours;
C           cursor input and linear contours implemented
C 22/9/87:  changes to command names (some removed). UV-box and grid
C           options added, change to character representation of levels.
C 23/9/87:  Corrections to some routines. Addition of "add-cross; 'C' in
C           W-O-P; SET-CROSS-STYLE; NEXT-FRAME", correction to framing
C           and character size definition.
C 24/9/87:  Preparing for release; help written, bugs cleared up - only
C           pips are unavailable.
C 27/9/87:  Correction to CROSSes option, optionally fast contour algorithm
C           installed. Line width option added.
C 30/9/87:  Correction to map size handling and checking of "R" option.
C 14/10/87: Pips added and tested; change to UVTORD calls throughout.
C           (This will need looking at again).
C           READ-MAP command added to this sub-system.
C 17/10/87: Corrections to pips (setup up STPROJ parameters consistent
C           with the "quick" precession routine PRECES).
C           Correction to printing of contour levels and the handling of
C           the zero contour.
C 17/11/87: Installation of OVERLAY-MAP and FAST-CONTOUR commands. Change
C           to the way contours are labelled (stop at bottom of plot and
C           only those plotted are recorded). OUTPUT-DEVICE now handled
C           by GETPLT; DATE and program name plotting installed.
C 11/12/87: EXPLAIN now uses updated HELP_UTIL program. Set-output-device
C           now does an end-plot.
C 21/12/87: LINEAR-CONTOURS and LOGARITHMIC-CONTOURS now have proper error
C           checking of user inputs before setting up the contour levels.
C 12/01/88: Modification to allow calling of BASIC-COMMANDS.
C           Change in style of wording on output plot.
C 21/01/88: Update state is correctly handled during output to the
C           Pericom -- this should stop screen flicker.
C 24/03/88: The number of commands has been reduced (by adding multiple
C           option commands) and some bugs have been fixed. All routines
C           requiring 'UV-range' input can now read from the graphics
C           screen if required (and appropriate).
C 14/09/88: All routines now forced to access STATUS using the routine
C           argument. All variables defined so that $inplicit off may be
C           used.
C 21/09/88: Proper interpolation option installed for contouring which
C           by default will be off. INTMAP is now used to interpolate the
C           required UV region into the work array. This must occur each
C           time PLT_DOCONT is called so as to be able to correctly
C           release space in the work array for other utilities.
C 20/1/89:  More corrections to linear-contours and log-contours
C 13/02/89: New DISPLAY command replaces show commands
C 23/03/89: Substantial changes to plot library in the way
C           overlay maps are handled. New include file format.
C           Grey-scales improved. Colours for LEXI enhanced.
C           Drawing-sys allows redefinition of frame.
C 22/08/89: plot_TVmod and internal functions for setting colour
C           table moved to PLOT-COL-TABLE and enhanced colour
C           option added.
C 23/10/89: bug fixed in interpolation option for large maps
C 14/12/89: added support for vector plotting
C 05/03/90: added plotting of symbols representing special data
C 01/05/90: new option to allow full use of plotting area
C 20/06/90: cursor-position command added to display sub-system
C 11/03/91: standard get-type command added to plot system
C 20/11/91: additional options to frames and graphics overlay
C 16/12/91: new surface plot option to do a surface plot in a graphics
C           overlay window.
C
*-
       $include (anmap)anmap-plot-sys:incl
       $include (anmap)anmap-sys-pars:incl
       $include (library)chrlib-functions:incl

C map work space
       real*4          map_array(*)

C define variables to access commands
       integer         number_commands, i_command, i_com_done, icomm
       parameter      (number_commands = 33)
       character*70    liscom(number_commands)

C parameters holds information on the command line
       character*80    parameters
       integer         len_cli, len_p

C logical flag set if the sub-system is called in the form:
C   map-display command-name options
C from another sub-system
       logical         exit_at_end
       integer         status

C define commands
       data liscom(1)
     *  / 'set-map ................. define map to use for plot'/
       data liscom(2)
     *  / 'set-pips ................ turn pips on/off [on]'/
       data liscom(3)
     *  / 'set-grid ................ turn grid on/off [off,off]'/
       data liscom(4)
     *  / 'set-uv-range ............ define uv-range to plot [full]'/
       data liscom(5)
     *  / 'set-interpolation ....... interpolation option [off]'/
       data liscom(6)
     *  / 'set-style ............... set various style options'/
       data liscom(7)
     *  / 'grey-scale .............. define (overlay) of grey scale'/
       data liscom(8)
     *  / 'vector-plot ............. define (overlay) of vectors'/
       data liscom(9)
     *  / 'symbol-plot ............. define (overlay) of symbols'/
       data liscom(10)
     *  / 'solid-contours .......... define solid contour levels'/
       data liscom(11)
     *  / 'dash-contours ........... define dash contour levels'/
       data liscom(12)
     *  / 'linear-contours ......... define linear contour levels'/
       data liscom(13)
     *  / 'logarithmic-contours .... define logarithmic contour levels'/
       data liscom(14)
     *  / 'reset-contour-levels .... reset all contour levels'/
       data liscom(15)
     *  / 'plot .................... plot portions of the plot'/
       data liscom(16)
     *  / 'go-plot ................. do complete plot'/
       data liscom(17)
     *  / 'segment-screen .......... define more than one plot'/
       data liscom(18)
     *  / 'next-frame .............. move to next frame/screen'/
       data liscom(19)
     *  / 'overlay-map ............. set map for overlay'/
       data liscom(20)
     *  / 'clear-screen ............ clear all segments'/
       data liscom(21)
     *  / 'display ................. display options and levels'/
       data liscom(22)
     *  / 'add-title ............... add title to plot'/
       data liscom(23)
     *  / 'add-crosses ............. add crosses to plot'/
       data liscom(24)
     *  / 'add-text ................ add text to plot'/
       data liscom(25)
     *  / 'end-plot ................ end current plot'/
       data liscom(26)
     *  / 'set-output-device ....... define the output device'/
       data liscom(27)
     *  / 'graphics-overlay ........ graphics overlay mode on/off'/
       data liscom(28)
     *  / 'slice ................... plot slice through map'/
       data liscom(29)
     *  / 'surface-plot ............ plot relief surface view of map'/
       data liscom(30)
     *  / 'draw-on-plot ............ enter interactive draw mode'/
       data liscom(31)
     *  / 'TV-modify ............... modify TV lookup table'/
       data liscom(32)
     *  / 'cursor-position ......... return map position and value'/
       data liscom(33)
     *  / 'get ..................... read options into parameters'/

C check whether to exit on completion of command
       call enqcli(parameters,len_cli)
       exit_at_end = len_cli.ne.0
       i_com_done = 0

1000   continue
       status = 0

C return if command is finished and call was of the form
C           "map-display  command-name"
C
       if (exit_at_end .and. i_com_done.ne.0) then
         i_command = 100
         return
       end if

C decode commmands
       call cmd_getcmd(prompt_plot,liscom,
     *                  number_commands,icomm,status)

C check for error
       if (status.ne.0) then
         call cmd_err(status,'DISPLAY-MAP',' ')
         goto 1000
       end if

C exit for basic command
       if (icomm.le.0) then
         i_command = icomm
         return
       end if

C increment command counter
       i_com_done = i_com_done + 1

C decode command
       goto (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
     *       22,23,24,25,26,27,28,29,30,31,32,33)
     *       icomm

C-----------------------------------------------------------------------
C
C Options
C -------
1      call plot_setmap(.false.,map_array,status)
       GOTO 1000
2      call plot_setpip(status)
       goto 1000
3      call plot_setgrd(status)
       goto 1000
4      call plot_setuv(map_array,status)
       goto 1000
5      call plot_setint(status)
       goto 1000
6      call plot_style(status)
       goto 1000
7      call plot_grey(status)
       goto 1000
8      call plot_vectors(status)
       goto 1000
9      call plot_symbols(status)
       goto 1000
10     call plot_ctsol(status)
       goto 1000
11     call plot_ctdash(status)
       goto 1000
12     call plot_ctlin(status)
       goto 1000
13     call plot_ctlog(status)
       goto 1000
14     call plot_ctclr(status)
       goto 1000
15     call plot_pltopt(map_array,status)
       goto 1000
16     call plot_go(map_array,status)
       goto 1000
17     call plot_setseg(status)
       goto 1000
18     frame_done = .false.
       frame_init = .false.
       cont_done  = .false.
       title_done = .false.
       date_done  = .false.
       goto 1000
19     call plot_setmap(.true.,map_array,status)
       goto 1000
20     if (plot_open) then
         call plt_frinit(.false.,status)
       else
         call pgbegin(0,output_device,1,1)
         call pgadvance
         call pgend
       end if
       goto 1000
21     call plot_display(status)
       goto 1000
22     call plot_addtit(status)
       goto 1000
23     call plot_addcrs(status)
       goto 1000
24     call plot_addtxt(.true.,status)
       goto 1000
25     call plt_end(status)
       goto 1000
26     call plot_setout(status)
       goto 1000
27     call plot_grov(status)
       goto 1000
28     call plot_slice(map_array,status)
       goto 1000
29     call plot_surface(map_array,status)
       goto 1000
30     call plot_draw(status)
       goto 1000
31     call plot_TVmod(status)
       goto 1000
32     call map_alloc_in( imap, 'DIRECT', map_array, ip_map, status )
       call redt_load( imap, status )
       call plcu_read( map_array(ip_map), status )
       call map_end_alloc( imap, map_array, status )
       goto 1000
33     call plot_get( status )
       goto 1000
       end
C
C
*+ plot_setmap

       subroutine plot_setmap(overlay,map_array,status)
C      ------------------------------------------------
C
C define map to use for the plot
C
C Input
C    OVERLAY      -      Logical      -    overlay control option
C
C Returned
C    STATUS       -      I4           -    error return code
C
C If OVERLAY is set to false on entry to the routine then the user
C defines a new map to plot. If FRAME_DONE is set to TRUE then a call
C to NEXT-FRAME is effectively made to move onto the next plot. The
C UV_RANGE etc. are then redefined before plotting.
C
C If OVERLAY is set to true then the map is checked for the UV_REGION
C as defined being on the map. If it is, then the map is setup as an
C overlayed map, otherwise the command is faulted and no change is made.
*-
$include (anmap)anmap-plot-sys:incl
$include (library)maplib-redtape:incl

       integer      status
       logical      overlay,    uv_within
       integer      uv_test(4), izmnx(5)
       real*4       map_array(*), zmnx(5)
       integer      imap_old, ip_map_old

C check status value on entry
       if (status.ne.0) return

C check that overlay mode must follow an ordinary "set-map" command
       if (overlay .and. (.not.map_read .or. .not.plot_open)) then
         call cmd_wrerr('SET-MAP','No existing frame/map')
         return
       end if

C read input map from user
       imap_old = imap
       ip_map_old = ip_map
       call mapcat_getmap('Map : ','Default-Map','Read',imap,status)

       if (status.eq.0 .and. overlay) then

C .. test for overlap of defined uv_range on overlay map
         call iuv_load(uv_test,status)
         if (.not.uv_within(uv_range,uv_test)) then
           call cmd_wrerr('SET-MAP',
     >      '"OVERLAY" Map does not contain current UV-range')
           call cmd_wrerr('SET-MAP','Map not defined as overlay')
           imap = imap_old
           ip_map = ip_map_old
           return
         end if

C read map into core  and find max/min
         call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
         call scnmap(map_array(ip_map),uv_range,zmnx,izmnx,status)
         region_max = zmnx(1)
         region_min = zmnx(2)

       else if (status.eq.0) then

C .. read data into core
         call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)

C .. define new plot
         map_read   = .true.
         frame_init = .false.
         frame_done = .false.
         cont_done  = .false.
         title_done = .false.
         date_done  = .false.

         cont_total_s = 0
         cont_total_d = 0
         cont_plot_s  = 0
         cont_plot_d  = 0

C .. define default UV_range (whole map)
         call iuv_load(uv_range,status)
         iu_size = ixmax
         iv_size = iymax
         ipl1 = 1
         ipl2 = ixmax
         jpl1 = 1
         jpl2 = iymax

C .. find max/min on map and store this information
         call scnmap(map_array(ip_map),uv_range,zmnx,izmnx,status)
         region_max = zmnx(1)
         region_min = zmnx(2)

C .. define pips for default uv_range
         call plot_getpip(.false.,status)

       end if

       call map_end_alloc(imap,map_array,status)
       call cmd_err(status,'SET-MAP',' ')
       end
C
C
*+ plot_setseg

       subroutine plot_setseg(status)
C      ------------------------------
C
C Define segments for plot
C
C The screen is segmented into NxN regions and each may be used for a
C separate plot. They are numbered consecutively from top left to bottom
C right as if reading a page.
C This routine should only be called when no plot is opn.
C
*-
$include (anmap)anmap-plot-sys:incl

       integer   status, n_old_seg

C check status value on entry
       if (status.ne.0) return

       if (plot_open) then
         call cmd_wrerr('SEGMENT-SCREEN','Plot currently active')
         return
       end if
       n_old_seg = segments
       call geti('Horizontal Segments : ','*',segments,status)
       if (segments.gt.3) then
         call cmd_wrerr('SEGMENT-SCREEN','Too many segments >3')
         segments = n_old_seg
       end if
       call cmd_err(status,'SEGMENT-SCREEN',' ')
       end
C
C
*+ plot_grov

       subroutine plot_grov(status)
C      ----------------------------
C
C Control the plotting of additional plots using the overlay facility
C
*-
$include (anmap)anmap-plot-sys:incl

       integer    status
       logical    onoff

C check status on entry
       if (status.ne.0) return

       graphics_overlay =
     *          onoff('Graphics overlay (on/off) : ','OFF',status)
       if (graphics_overlay) then
         call getnr('Overlay-region (Normalized-coords.) : ',
     *              '0.7,1.0,0.7,1.0',overlay_vp,4,status)
       end if
       if (.not.graphics_overlay) then
         view_port(1) = 0.0
         view_port(2) = 1.0
         view_port(3) = 0.0
         view_port(4) = 1.0
       end if
       call cmd_err(status,'GRAPHICS-OVERLAY',' ')

       end
C
C
*+ plot_style

       subroutine plot_style(status)
C      -----------------------------
C
C Set the various style options
*-
       $include (anmap)anmap-plot-sys:incl
       $include (anmap)anmap-sys-pars:incl
       $include (library)chrlib-functions:incl
       $include (library)iolib-functions:incl

C options list
       character*50  option_list(11), option
       integer       lo
C error return
       integer       status
C output unit
       integer       iout
C saving style options
       character     parameter_name*40, save_string*80
       integer       len_param, len_save
C variables used to hold current option settings
       integer       old_font
C save-state
       logical       save_not_state, save_state
       common       /local_set_style_save/ save_not_state

       data option_list /'TEXT ........ set text output',
     *                   'FRAME ....... set plot frame and text',
     *                   'LINES ....... set lines on contour plots',
     *                   'CROSS ....... set crosses and text',
     *                   'GRID ........ set grid type',
     *                   'CONTOURS .... turn on/off list of contours',
     *                   'DATE ........ turn on/off writing date etc.',
     *                   'SCALE-BAR ... set scale-bar options',
     *                   'OVERLAY ..... set graphics-overlay options',
     *                   'RESET ....... reset default options',
     *                   'SAVE ........ turn save option on/off'       /

C check status on entry
       if (status.ne.0) return
       save_state = .not.save_not_state

C prompt for option
       parameter_name = ' '
       save_string    = ' '
       call getopt('Option (?=help) : ',
     *             'RESET',option_list,11,option,status)
       call chucas(option)
       if (status.ne.0) then
         call cmd_err(status,'SET-STYLE',' ')
         return
       end if
       lo = lenb(option)

C act
       if (cmatch(option(1:lo),'TEXT')) then
C      --------------------------------
         call getr('Text-size : ','*',text_size,status)
         old_font = text_font
         call geti('Text-font : ','*',text_font,status)
         if (text_font.lt.0 .or. text_font.gt.4) then
           call cmd_wrerr('SET-STYLE','"TEXT" Invalid font: reset')
           text_font = old_font
         end if

         parameter_name = '%set-style-text'
         write(save_string,'(a,f10.2,i3)')
     *    'map-display set-style text ',text_size, text_font


       else if (cmatch(option(1:lo),'FRAME')) then
C      --------------------------------------
         call getr('Mark-size : ','*',mark_size,status)
         call getr('Pip-size : ','*',pip_size,status)

         old_font = mark_font
         call geti('Font : ','*',mark_font,status)
         if (mark_font.lt.0 .or. mark_font.gt.4) then
           call cmd_wrerr('SET-STYLE','"FRAME" Invalid font: reset')
           mark_font = old_font
         end if
         call geti('Frame-colour (0-4) : ','*',frame_colour,status)
         if (frame_colour.lt.0 .or. frame_colour.gt.4) then
           frame_colour = 1
           call cmd_wrerr('SET-STYLE:','"FRAME" Invalid colour: reset')
         end if
         frame_space = -1.0
         call getr('Frame-space (0.0-1.0) [default] : ',' ',
     *              frame_space,status)

         parameter_name = '%set-style-frame'
         write(save_string,'(a,f10.2,f10.2,i3,i4,f7.2)')
     *    'map-display set-style frame ',mark_size,pip_size,mark_font,
     *     frame_colour,frame_space


       else if (cmatch(option(1:lo),'GRID')) then
C      --------------------------------------
         call geti('Grid-style (1-5; -1=default for device) : ',
     *             '*',grid_style,status)
         if (grid_style.lt.-1 .or. grid_style.gt.5 .or.
     *       grid_style.eq.0                           ) then
           call cmd_wrerr('SET-STYLE','"GRID" Invalid style: reset')
           grid_style = -1
         end if
         call geti('Grid-colour (0-4; -1=default for device) : ',
     *             '*',grid_colour,status)
         if (grid_colour.lt.-1 .or. grid_colour.gt.4) then
           call cmd_wrerr('SET-STYLE','"GRID" Invalid colour: reset ')
           grid_colour = -1
         end if

         parameter_name = '%set-style-grid'
         write(save_string,'(a,i4,i4)')
     *    'map-display set-style grid ',grid_style,grid_colour

       else if (cmatch(option(1:lo),'LINES')) then
C      --------------------------------------
         call geti('Solid-style (1-5) : ','*',solid_style,status)
         if (solid_style.lt.1 .or. solid_style.gt.5) then
           call cmd_wrerr('SET-STYLE','"LINE" Invalid style 1-5 only')
           solid_style = 1
         end if
         call geti('Solid-colour (0-4) : ','*',solid_colour,status)
         if (solid_colour.lt.0 .or. solid_colour.gt.4) then
           call cmd_wrerr('SET-STYLE','"LINE" Invalid colour 0-4 only')
           solid_colour = 1
         end if
         call geti('Solid-width (1-20) : ','*',solid_width,status)
         if (solid_width.lt.1 .or. solid_width.gt.20) then
           call cmd_wrerr('SET-TYLE','"LINE" Invalid line width')
           solid_width = 1
         end if
         call geti('Dash-style (1-5) : ','*',dash_style,status)
           if (dash_style.lt.1 .or. dash_style.gt.5) then
           call cmd_wrerr('SET-STYLE','"LINE" Invalid style 1-5 only')
           dash_style = 2
         end if
         call geti('Dash-colour (0-4) : ','*',dash_colour,status)
         if (dash_colour.lt.0 .or. dash_colour.gt.4) then
           call cmd_wrerr('SET-STYLE','"LINE" Invalid colour 0-4 only')
           dash_colour = 1
         end if
         call geti('Dash-width (1-20) : ','*',dash_width,status)
         if (dash_width.lt.1 .or. dash_width.gt.20) then
           call cmd_wrerr('SET-STYLE','"LINE" Invalid line width')
           dash_width = 1
         end if

         parameter_name = '%set-style-lines'
         write(save_string,'(a,6i6)')
     *    'map-display set-style lines ',
     *    solid_style, solid_colour, solid_width,
     *    dash_style, dash_colour, dash_width


       else if (cmatch(option(1:lo),'CROSS')) then
C      --------------------------------------
         call getr('Cross-size : ','*',cross_size,status)
         write(save_string,'(a,f10.2)')
     *    'map-display set-style CROSS ',cross_size
         parameter_name = '%set-style-cross'

       else if (cmatch(option(1:lo),'CONTOURS')) then
C      -----------------------------------------
          cont_opt = onoff('Plot-levels (on/off) : ','on',status)
          if (cont_opt) then
            save_string = 'map-display set-style contours on'
          else
            save_string = 'map-display set-style contours off'
          end if
          parameter_name = '%set-style-contours'

       else if (cmatch(option(1:lo),'DATE')) then
C      -------------------------------------
          date_opt = onoff('Plot-date (on/off) : ','on',status)
          if (date_opt) then
            save_string = 'map-display set-style date on'
          else
            save_string = 'map-display set-style date off'
          end if
          parameter_name = '%set-style-date'

       else if (cmatch(option(1:lo),'SCALE-BAR')) then
C      ------------------------------------------
          scale_bar_opt = onoff('Display scale-bar (on/off) : ',
     *                          'on',status)
          if (scale_bar_opt) then
            call getr('Width of scale-bar : ','*',
     *                 scale_bar_width,status)
            save_string = ' '
            write(save_string,'(A,F12.3)')
     *            'map-display set-style scale-bar on ',
     *            scale_bar_width
            scale_bar_text = onoff('Display text (on/off) : ',
     *                       'on',status)
          else
            save_string = 'map-display set-style scale-bar off'
          end if
          parameter_name = '%set-style-scale-bar'

       else if (cmatch(option(1:lo),'OVERLAY')) then
C      ------------------------------------------
          screen_clear = onoff('Clear-screen on overlay (on/off) : ',
     *                         'on',status)
          call getr('Frame-offset-X : ','*',frame_offset(1),status)
          call getr('Frame-offset-Y : ','*',frame_offset(2),status)
          if (screen_clear) then
            write(save_string,'(A,F8.2,F8.2)')
     *       'map-display set-style overlay on ',
     *       frame_offset(1),frame_offset(2)
          else
            write(save_string,'(A,F8.2,F8.2)')
     *       'map-display set-style overlay off ',
     *       frame_offset(1),frame_offset(2)
          end if
          parameter_name = '%set-style-overlay'

       else if (cmatch(option(1:lo),'RESET')) then
C      --------------------------------------
         call plt_reset(status)
         call cmd_unsetparam('%set-style-text',status)
         call cmd_unsetparam('%set-style-frame',status)
         call cmd_unsetparam('%set-style-grid',status)
         call cmd_unsetparam('%set-style-lines',status)
         call cmd_unsetparam('%set-style-cross',status)
         call cmd_unsetparam('%set-style-contours',status)
         call cmd_unsetparam('%set-style-scale-bar',status)
         call cmd_unsetparam('%set-style-overlay',status)
         call cmd_unsetparam('%set-style-date',status)

       else if (cmatch(option(1:lo),'SAVE')) then
C      -------------------------------------
          save_not_state = .not.onoff('Save-date (on/off) : ',
     *                                'on',status)

       end if

C prime system parameter if required
       len_param = lenb(parameter_name)
       if (save_state .and. len_param.gt.0) then
         len_save = lenb(save_string)
         call cmd_setparam(parameter_name(1:len_param),
     *                     save_string(1:len_save),
     *                     status                      )
       end if

C handle errors
       call cmd_err(status,'SET-STYLE',' ')

       end
C
C
*+ plot_setpip

       subroutine plot_setpip(status)
C      ------------------------------
C
C Turn pips option on/off
*-
$include (anmap)anmap-plot-sys:incl

       integer    status
       logical    onoff

       if (status.ne.0) return

       pips_opt = onoff('Pips on/off : ','on',status)
       if (pips_opt) call plot_getpip(.true.,status)
       uvpip_opt = onoff('UV-pips on/off : ','on',status)
       call cmd_err(status,'set-pips',' ')

       end
C
C
*+ plot_getpip

       subroutine plot_getpip(ask,status)
C      ----------------------------------
C
C  Input pip intervals, in RA and Dec.
C
C-

       integer    ic, status
       real*8     rac(4), decc(4), ra_range, dec_range
       real       facr, facd, pipinr, pipind
       character  prompt*40
       logical    ask, cmd_dblev

$include (library)constant:incl
$include (library)maplib-redtape:incl
$include (anmap)anmap-plot-sys:incl

       if (status.ne.0) return

C check for writing out information from the routines
       if (cmd_dblev(1)) ic=1

C  Set up map projection parameters
       call stproj(iproj,0,usamp,skew,ramap,decmap,refdat,epoch,status)

C  Find suitable pip intervals
       call pipsa(uv_range,rac,decc,ra_range,dec_range,ic,status)
       call pipsb(uv_range,5,ra_range,dec_range,pipinr,pipind,0,status)

C  Offer these intervals
       facr=1.0
       if (pipinr.gt.500.) facr=60.0
       pipinr=pipinr/facr
       write(prompt,1)pipinr
       if (facr.eq.60.) prompt(33:35)='min'
       if (ask) call getr(prompt,' ',pipinr,status)
       grid_ra=pipinr*facr/60.0
C
       facd=1.0
       if (pipind.gt.500.) facd=60.0
       pipind=pipind/facd
       write(prompt,2)pipind
       if (facd.eq.60.) prompt(36:38)='min'
       if (ask) call getr(prompt,' ',pipind,status)
       grid_dec=pipind*facd/60.0
C
C  Reset map projection parameters
       call stproj(iproj,1,usamp,skew,ramap,decmap,refdat,epoch,status)
C
    1  format('Pip/grid interval in RA [',F5.1,'] secs: ')
    2  format(20X,'Dec [',F5.1,'] arcsecs: ')
C
       end
C
C
*+ plot_setgrd

       subroutine plot_setgrd(status)
C      ------------------------------
C
C Set the grid option on/off
*-
$include (anmap)anmap-plot-sys:incl

       integer   status
       logical   onoff

       if (status.ne.0) return

       grid_opt = onoff('RA/DEC grid on/off : ','off',status)
       uvgrid_opt = onoff('UV grid on/off : ','off',status)
       if (uvgrid_opt) then
         call getr('Spacing in U (0=default) : ','0',grid_u,status)
         call getr('Spacing in V (0=default) : ','0',grid_v,status)
       end if
       call cmd_err(status,'SET-GRID',' ')
       end
C
C
*+ plot_setint

       subroutine plot_setint(status)
C      ------------------------------
C
C Set the interpolation option on/off
*-
$include (anmap)anmap-plot-sys:incl

       integer   status
       logical   onoff

       if (status.ne.0) return

       int_opt = onoff('Interpolation on/off : ','off',status)
       call cmd_err(status,'SET-INTERPOLATION',' ')
       end
C
C
*+ plot_setout

       subroutine plot_setout(status)
C      ------------------------------
C
C Define the output device
*-
$include (anmap)anmap-plot-sys:incl

       integer    status

       if (status.ne.0) return

       call enqplt(0,output_device)
       call getplt('Output-device (?=list) : ',
     *             '*',output_device,status)
       if (status.eq.0) then
         call plt_end(status)
       end if
       call cmd_err(status,'SET-OUTPUT-DEVICE',' ')
       end
C
C
*+ plot_setuv

       subroutine plot_setuv(map_array,status)
C      ---------------------------------------
C
C set uv-range to contour
C
C The UV-range on the map is prompted for: the max and min for the
C specified region are updated.
*-
$include (anmap)anmap-plot-sys:incl

C data array
       real*4    map_array(*)
C error flag
       integer   status
C max min in map range, UV-range and redtape pointer
       real*4    zmnx(10)
       integer   izmnx(10), uv_full(4), ip_redt

C check status on entry
       if (status.ne.0) return

       if (.not.map_read) then
         call cmd_wrerr('SET-UV-RANGE','no map set')

       else
         call plt_getuv('UV-range : ','*',uv_range,status)
         call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
         call scnmap(map_array(ip_map),uv_range,zmnx,izmnx,status)
         call map_end_alloc(imap,map_array,status)
         call iuv_load(uv_full,status)
         region_max = zmnx(1)
         region_min = zmnx(2)
         ipl1 = uv_range(1) - uv_full(1) + 1
         ipl2 = uv_range(2) - uv_full(1) + 1
         jpl1 = - uv_range(3) + uv_full(3) + 1
         jpl2 = - uv_range(4) + uv_full(3) + 1
         call plot_getpip(.false.,status)
         call cmd_err(status,'SET-UV-RANGE',' ')
       end if

       end
C
C
*+ plot_addtit

       subroutine plot_addtit(status)
C      ------------------------------
C
C define title for the plot
*-
$include (anmap)anmap-plot-sys:incl

       integer     status, lenb

C check status on entry
       if (status.ne.0) return

       call getstr('Plot-title : ',title_plot,title_plot,status)
       title_opt = lenb(title_plot).gt.0
       if (title_opt) call plt_dotext(status)
       call cmd_err(status,'ADD-TITLE',' ')
       end
C
C
*+ plot_addtxt

       subroutine plot_addtxt(check_position,status)
C      ---------------------------------------------
C
C Add text to plot as specified location
C
C Input
C    CHECK_POSITION      -      L      -    internal position check flag
C
C Returned
C    STATUS              -      I4     -    error return code
C
C If check_position is set to true the input UV position for the text
C is checked to ensure that it is within the plotted frame. This check
C is not performed if check_position is set false.
*-
$include (anmap)anmap-plot-sys:incl

C error flag
       integer        status

C text position variables
       integer        ipos(2)
       real*4         xpos, ypos

C integer function
       integer        lenb, pgtext

C current style settings
       integer        old_font
       real*4         old_size

       logical        check_position
       character*80   text
       integer        len_text

C check status on entry
       if (status.ne.0) return

       ipos(1) = 0
       ipos(2) = 0
       if (.not.frame_done) then
         call cmd_wrerr('ADD-TEXT','No frame plotted')
         return
       end if
       call getni('UV-position for text : ',' ',ipos,2,status)
       if (check_position) then
         if (ipos(1).lt.uv_range(1).or.ipos(1).gt.uv_range(2) .or.
     *       ipos(2).lt.uv_range(4).or.ipos(2).gt.uv_range(3)) then
           call cmd_wrerr('ADD-TEXT','UV position not on frame')
           return
         end if
       end if

C read in text
       call getstr('Text : ',' ',text,status)
       if (status.ne.0) then
         call cmd_err(status,'ADD-TEXT',' ')

       else
         len_text = lenb(text)

C .. plot at position
         xpos = ipos(1)
         ypos = ipos(2)
C .. retain current style attributes
         call pgqcf(old_font)
         call pgqch(old_size)
         call pgscf(text_font)
         call pgsch(text_size)
C .. plot text
         status = pgtext(xpos,ypos,text(1:len_text))
C .. reset style attributes
         call pgscf(old_font)
         call pgsch(old_size)

       end if
       end
C
C
*+ plot_addcrs

       subroutine plot_addcrs(status)
C      ------------------------------
C
C draw cross at specified RA and DEC location
*-
$include (anmap)anmap-sys-pars:incl
$include (anmap)anmap-plot-sys:incl
$include (library)maplib-redtape:incl
$include (library)constants:incl
$include (library)io-constants:incl

C error flag
       integer      status
C position of cross
       real*8       ra, dec, du, dv, ra0, dec0
       integer      iup, ivp
       real*4       up, vp
C number of crosses, input units
       integer      number_crosses, i_cross, iin
C text for cross and crosses file
       character    text*80, crosses_file*(iolen_file)
C loop variable
       logical      loop
C variables to hold current attributes
       integer      old_font
       real*4       old_size
C logical flag specifying file input and no text plotting
       logical      file_input, mark_cross_with_text
C logical function requesting user response
       logical      yesno, cmd_dblev, cmatch
C text string length
       integer      len_text, lenb, intlc

C check status on entry
       if (status.ne.0) return

C check for plot frame
       if (.not. frame_done) then
         call cmd_wrerr('ADD-CROSS','No plot frame')
         return
       end if

C do user I/O
       file_input = (yesno('Use Crosses file : ','no',status))

       if (file_input) then
C .. open crosses file and set this file for input
         call getfil('Crosses-file : ',default_crosses_file,
     *               crosses_file,status)
         call opefil(i_cross,crosses_file,'read',0,status)
         call setin(i_cross)
       else
         call enqin(i_cross)
       end if

       loop = .true.

C .. loop reading from input unit -- either terminal or the
C    crosses file. When the routine comes to the end of the
C    crosses file he file is automatically closed and the
C    input unit reset to unit 1, the Do loop will then terminate.
       do while (loop)

         call enqin(iin)
         if (iin.ne.i_cross) goto 100
         call getra('RA : ',' ',ra,status)
         call getdec('DEC : ',' ',dec,status)
         call getstr('TEXT : ',' ',text,status)
         mark_cross_with_text = .true.
         if (cmatch('#',text(1:lenb(text)))) then
           mark_cross_with_text = .false.
         end if
C .. check STATUS before doing plotting
         if (status.eq.0) then

           call precrd(1950.0d0,ra,dec,epoch,ra0,dec0)
           call rdtouv(ra0,dec0,du,dv,status)
           len_text = lenb(text)

C .. draw the cross if U and V on the plot
           iup = nint(du)
           ivp = nint(dv)
           if (cmd_dblev(3)) then
             print *,'position = ',du,dv,iup,ivp
             print *,'text = ',text(1:lenb(text))
             print *,'marking cross = ',mark_cross_with_text
           end if
           if (iup.ge.uv_range(1) .and. iup.le.uv_range(2) .and.
     *       ivp.ge.uv_range(4) .and. ivp.le.uv_range(3)) then
             up = du
             vp = dv
             if (cmd_dblev(3)) then
               print *,'.. plotting up,vp = ',up,vp
             end if
             call plt_frset(status)
             call pgbbuf
             call pgmove(up-cross_size,vp)
             call pgdraw(up+cross_size,vp)
             call pgmove(up,vp-cross_size)
             call pgdraw(up,vp+cross_size)
             if (mark_cross_with_text) then
               call pgqch(old_size)
               call pgsch(text_size)
               call pgqcf(old_font)
               call pgscf(text_font)
               call pgtext(up+cross_size,vp+cross_size,text(1:len_text))
               call pgscf(old_font)
               call pgsch(old_size)
             end if
             call pgebuf
             call plt_frend
           else
             call cmd_wrerr('ADD-CROSS','Position not on plot')
           end if
         end if
100      call enqin(iin)
         loop = iin.ne.terminal_in .and. file_input
       end do
       call cmd_err(status,'ADD-CROSS',' ')

       end
C
C
*+ plot_grey

       subroutine plot_grey(status)
c      ----------------------------
C
C Set grey-scale option and range
*-
$include (anmap)anmap-plot-sys:incl

       integer    status
       logical    onoff

C check status on entry
       if (status.ne.0) return

C read in option
       grey_opt = onoff('Grey-scaling on/off : ','off',status)

       if (grey_opt) then
         white_level = region_max
         black_level = region_min
         call getr('Minimum-Level : ','*',black_level,status)
         call getr('Maximum-Level : ','*',white_level,status)
       end if
       call cmd_err(status,'GREY-SCALE',' ')
       end
C
C
*+ plot_ctclr

       subroutine plot_ctclr(status)
C      -----------------------------
C
C Clear currently defined contour levels
*-
$include (anmap)anmap-plot-sys:incl

       integer    status, l

C check status on entry
       if (status.ne.0) return

C clear counters of contours defined and plotted and the contour tables
       cont_total_s = 0
       cont_total_d = 0
       cont_plot_s = 0
       cont_plot_d = 0
       do l = 1,50
         dashed_done(l) = 0
         solid_done(l) = 0
       end do

C reset the grey scale
       grey_opt = .false.
       black_level = 0.0
       white_level = 0.0

       end
C
C
*+ plot_ctsol

       subroutine plot_ctsol(status)
C      -----------------------------
C
C Specified solid contour levels
*-
$include (anmap)anmap-plot-sys:incl

C error flag
       integer   status

C number of contours read
       integer   nread

C current contour level
       real*4    level

C input loop control
       logical   input

       if (status.ne.0) return

C .. read in a list of levels
       input = .true.
       nread = 0
       do while (input)
         level = -1.0e+40
         call getr('Contours-solid : ',' ',level,status)
         input = level.gt.-1.0e+30 .and. status.eq.0

C .. store this level
         if (input) then
           nread = nread + 1
           cont_total_s = cont_total_s + 1
           if (cont_total_s.le.50) then
             solid_cont(cont_total_s) = level
           else
             input = .false.
             cont_total_s = 50
             call cmd_wrerr('SOLID-CONTOURS','No. levels > 50')
           end if
         end if
       end do

       call cmd_err(status,'SOLID-CONTOURS',' ')

       end
C
C
*+ plot_ctdash

       subroutine plot_ctdash(status)
C      ------------------------------
C
C Specified dashed contour levels
*-
$include (anmap)anmap-plot-sys:incl

C error flag
       integer   status

C number of contours read
       integer   nread

C current contour level
       real*4    level

C input loop control
       logical   input

       if (status.ne.0) return

C .. read in a list of levels
       input = .true.
       nread = 0
       do while (input)
         level = -1.0e+40
         call getr('Contours-dashed : ',' ',level,status)
         input = level.gt.-1.0e+30 .and. status.eq.0

C .. store these levels away
         if (input) then
           nread = nread + 1
           cont_total_d = cont_total_d + 1
           if (cont_total_d.le.50) then
             dashed_cont(cont_total_d) = level
           else
             input = .false.
             cont_total_d = 50
             call cmd_wrerr('DASHED-CONTOURS','No. levels > 50')
           end if
         end if
       end do

       call cmd_err(status,'DASHED-CONTOURS',' ')

       end
C
C
*+ plot_ctlin

       subroutine plot_ctlin(status)
C      -----------------------------
C
C Define linear contours
*-
$include (anmap)anmap-plot-sys:incl

C error flag
       integer    status
C contour increment variables
       real*4     c_start, c_incre, c_level, c_max, c_max0
C test variables
       real*4     test_1, test_2
C logical function requesting user response
       logical    yesno

C check status on entry
       if (status.ne.0) return

C read start, increment and end
       if (.not.map_read) then
         call cmd_wrerr('LINEAR-CONTOURS','no map set')
         return
       end if
       c_start = -1.0e+30
       c_incre = -1.0e+30
       call getr('Start-level : ',' ',c_start,status)
       if (c_start.lt.-1.0e+25) return
       call getr('Increment : ',' ',c_incre,status)
       if (c_incre.lt.-1.0e+25) return
       if (abs(c_incre).lt.1.0e-10) return
       c_max = region_max
       c_max0 = c_max
       call getr('Maximum : ','*',c_max,status)
       if (abs(c_max-c_max0).lt.1.0e-4) then

C .. user has pressed return to the request for contour levels
C .. x 1.3 allows for possible interpolation onto finer grid.
         c_max = c_max*1.3

       end if

       test_1 = c_max-c_start
       test_2 = test_1*c_incre
       if (test_2.lt.0.0) then

C .. user has typed an inconsistent set of max/min and increment -- tell
C .. the user about their error:
         call cmd_wrerr('LINEAR-CONTOURS',
     >                   'Invalid MAX/MIN and increment')
         return

       end if

       if (status.eq.0) then

C .. define contour levels
         c_level = c_start
         do while (c_level.le.c_max   .and.
     *             cont_total_s.le.50 .and.
     *             cont_total_d.le.50      )

           if (abs(c_level) .lt. 1.0e-7) then

C ... note zero contour
             if (yesno('Include zero-contour : ','no',status)) then
               cont_total_d = cont_total_d + 1
               if (cont_total_d.le.50)
     *             dashed_cont(cont_total_d) = 0.0
             end if
           else if (c_level.lt.0.0) then
             cont_total_d = cont_total_d + 1
             if (cont_total_d.le.50)
     *           dashed_cont(cont_total_d) = c_level
           else
             cont_total_s = cont_total_s + 1
             if (cont_total_s.le.50)
     *           solid_cont(cont_total_s) = c_level
           end if
           c_level = c_level + c_incre
         end do
       end if
       if (cont_total_s .gt. 50) then
         call cmd_wrerr('LINEAR-CONTOURS','No. levels > 50; reset')
         cont_total_s = 50
       end if
       if (cont_total_d .gt. 50) then
         call cmd_wrerr('LINEAR-CONTOURS','No. levels > 50; reset')
         cont_total_d = 50
       end if
       call cmd_err(status,'LINEAR-CONTOURS',' ')
       end
C
C
*+ plot_ctlog

       subroutine plot_ctlog(status)
C      -----------------------------
C
C Define logarithmic contours
*-
$include (anmap)anmap-plot-sys:incl

C error flag
       integer  status
C contour increment variables
       real*4   c_start, c_fact, c_max, c_max_o, c_level

C check status on entry
       if (status.ne.0) return

C read start, factor and end
       if (.not.map_read) then
         call cmd_wrerr('LOGARITHMIC-CONTOURS','No map set')
         return
       end if
       c_start = -1.0e+30
       c_fact = -1.0e+30
       call getr('Start-level : ',' ',c_start,status)
       if (c_start.lt.-1.0e+25) return
       call getr(' Factor : ',' ',c_fact,status)
       if (c_fact.lt.-1.0e+25) return
       if (abs(c_fact).lt.1.0e-10) return
       c_max = region_max
       c_max_o = c_max
       call getr('Maximum : ','*',c_max,status)

C check for consistency in user input
       if (c_max.lt.c_start) then

         call cmd_wrerr('LOGARITHMIC-CONTOURS',
     >                   'Max-level < Min-level')
         return
       else if (c_fact.lt.1.0) then

         call cmd_wrerr('LOGARITHMIC-CONTOURS',
     >        'Factor < 1.0 (decreasing or oscillating values')
         return
       end if

       if (abs(c_max-c_max_o).lt.1.0e-4) then

C .. allow for possible interpolation of map in default case
         c_max = c_max*1.3
       end if

       if (status.eq.0) then

C .. define contour levels
         c_level = abs(c_start)
         do while (c_level.le.c_max   .and.
     *             cont_total_s.le.50 .and.
     *             cont_total_d.le.50      )

C ... add in a negative level
           cont_total_d = cont_total_d + 1
           if (cont_total_d.le.50)
     *         dashed_cont(cont_total_d) = -c_level

C ... add in a positive level
           cont_total_s = cont_total_s + 1
           if (cont_total_s.le.50)
     *         solid_cont(cont_total_s) = c_level

           c_level = abs(c_level*c_fact)

         end do
       end if
       if (cont_total_s .gt. 50) then
         call cmd_wrerr('LOG-CONTOURS','No. levels > 50; reset')
         cont_total_s = 50
       end if
       if (cont_total_d .gt. 50) then
         call cmd_wrerr('LOG-CONTOURS','No. levels > 50; reset')
         cont_total_d = 50
       end if
       call cmd_err(status,'LOGARITHMIC-CONTOURS',' ')
       end
C
C
*+ plot_go

       subroutine plot_go(map_array,status)
C      ------------------------------------
C
C Do the plot
*-
       $include (anmap)anmap-plot-sys:incl
C
       real*4     map_array(*)
       integer    status,  iscr, ip_scr, ip_vec, ip_chi

C check status on entry
       if (status.ne.0) return

       call map_alloc_scr( 512, 512, 'DIRECT', iscr, ip_scr, status)
       call map_alloc_in( imap, 'DIRECT', map_array, ip_map, status)
       print *,'** Allocated : ',ip_map,ip_scr
       call plt_dogrey(map_array(ip_map),status)
       frame_done = .false.
       call plt_doframe(status)
       call plt_dosymb(map_array(ip_map),status)
       call plt_docont(map_array(ip_map),map_array(ip_scr),status)
       if (vectors_opt .and. vec_chi_map.ne.0) then
         if (vec_int_map.ne.imap .and. vec_type.ne.2) then
           call map_alloc_in( vec_int_map, 'DIRECT',
     *                        map_array, ip_map, status )
         end if
         call map_alloc_in( vec_chi_map, 'DIRECT',
     *                      map_array, ip_chi, status)
         call plt_dovecs( map_array(ip_chi), map_array(ip_map), status)
         if (vec_int_map.ne.imap .and. vec_type.ne.2) then
           call map_end_alloc( vec_int_map, map_array, status )
         end if
         call map_end_alloc( vec_chi_map, map_array, status )
       end if
       call plt_dotext(status)

       call map_end_alloc( imap, map_array, status )
       call map_end_alloc( iscr, map_array, status )

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
       $include (anmap)anmap-plot-sys:incl

C local variables
       integer     points_max
       parameter  (points_max = 2000)
       integer     n_points, n, i, iuv_1(2), iuv_2(2)
       real*4      slice(points_max), x_axis(points_max),
     *             uv_1(2), uv_2(2), u1, u2, v1, v2
       real*4      u, v, slice_max,  slice_min
       real*8      du, dv
       character   text*80
       integer     len_t
       logical     cmd_dblev

       if (.not.map_read) then
         call cmd_wrerr('SLICE','No map defined: use SET-MAP')
         return
       end if

C read positions on map
       call plt_rdpos('Left-End-of-slice  : ',uv_1,status)
       call plt_rdpos('Right-End-of-slice : ',uv_2,status)
       if (status.ne.0) then
         call cmd_err(status,'SLICE',' ')
         return
       end if
       do i = 1,2
         iuv_1(i) = nint(uv_1(i))
         iuv_2(i) = nint(uv_2(i))
       end do

C read map
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)

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
          return
       end if

C open the plot device
       call plt_grinit(status)

C define plot size
       call pgrnge(slice_min,slice_max,v1,v2)
       call pgrnge(x_axis(1),x_axis(n_points),u1,u2)
       call pgwindow(u1,u2,v1,v2)

C plot
       call pgbbuf
       call pgbox('BNT',0.0,0,'BNT',0.0,0)
       call pgline(n_points,x_axis(1),slice(1))
       write(text,'(''Slice from '',2i5,'' to '',2i5)')iuv_1,iuv_2
       call pglabel('Relative UV','Intensity (Map-Units)',text(1:len_t))

C end the plot
       call pgebuf
       call plt_grend(status)
       end
C
C
*+ plot_pltopt

       subroutine plot_pltopt(map_array,status)
C      ----------------------------------------
C
C Plot - use one of various options
*-
       $include (anmap)anmap-plot-sys:incl
       $include (anmap)anmap-sys-pars:incl
       $include (library)chrlib-functions:incl

C data array
       real*4        map_array(*)
C error flag output unit and pointers to scratch map
       integer       status, iout, iscr, ip_scr, ip_chi
C definition of options
       character*50  option_list(8), option
       integer       lo

       data option_list /'TEXT ....... add text to existing plot',
     *                   'FRAME ...... plot a frame',
     *                   'GREY ....... plot a grey-scale',
     *                   'SYMBOLS .... plot symbol-image',
     *                   'CONTOURS ... plot contour map',
     *                   'VECTORS .... plot vector field',
     *                   'ALL ........ do all plotting',
     *                   'METAFILE ... plot a saved METAFILE' /

C prompt for option
       call getopt('Plot-option (?=help) : ','ALL',
     *             option_list,8,option,status)
       call chucas(option)
       if (status.ne.0) then
         call cmd_err(status,'PLOT',' ')
         return
       end if
       lo = lenb(option)

C act
       if (cmatch(option(1:lo),'TEXT')) then
         call redt_load( imap, status )
         call plt_dotext(status)

       else if (cmatch(option(1:lo),'FRAME')) then
         call redt_load( imap, status )
         frame_done = .false.
         call plt_doframe(status)

       else if (cmatch(option(1:lo),'CONTOURS')) then
         call redt_load( imap, status )
         call map_alloc_scr( 512, 512, 'DIRECT', iscr, ip_scr, status )
         call map_alloc_in( imap, 'DIRECT', map_array, ip_map, status )
         call plt_docont(map_array(ip_map),map_array(ip_scr),status)
         call map_end_alloc( imap, map_array, status )
         call map_end_alloc( iscr, map_array, status )

       else if (cmatch(option(1:lo),'GREY')) then
         call redt_load( imap, status )
         call map_alloc_in( imap, 'DIRECT', map_array, ip_map, status )
         call plt_dogrey(map_array(ip_map),status)
         call map_end_alloc( imap, map_array, status )

       else if (cmatch(option(1:lo),'SYMBOLS')) then
         call redt_load( imap, status )
         call map_alloc_in( imap, 'DIRECT', map_array, ip_map, status )
         call plt_dosymb(map_array(ip_map),status)
         call map_end_alloc( imap, map_array, status )

       else if (cmatch(option(1:lo),'VECTORS')) then
         call redt_load( imap, status )
         if (vec_chi_map.ne.0 .and. vectors_opt) then
           if (vec_int_map.ne.0 .and. vec_type.ne.2) then
             call map_alloc_in( vec_int_map, 'DIRECT',
     *                          map_array, ip_map, status )
           end if
           call map_alloc_in( vec_chi_map, 'DIRECT',
     *                        map_array, ip_chi, status)
           call plt_dovecs(map_array(ip_chi),map_array(ip_map),status)
           if (vec_int_map.ne.0 .and. vec_type.ne.2) then
             call map_end_alloc( vec_int_map, map_array, status )
           end if
           call map_end_alloc( vec_chi_map, map_array, status )
         end if

       else if (cmatch(option(1:lo),'METAFILE')) then
         call plot_metafile( status )

       else if (cmatch(option(1:lo),'ALL')) then
         call redt_load( imap, status )
         call plot_go(map_array,status)

       end if
       end
C
C
*+plot_metafile

       subroutine plot_metafile( s )
C
C Plot a metafile on an output device
C
C Returned:
C   status
       integer       s
C
C-
       $include (library)io-constants:incl
       $include (anmap)anmap-sys-pars:incl

       character*(iolen_file)  file, device
c
       if (s.ne.0) return

       file=default_metafile
       call enqplt(0,device)
       call getfil('metafile name : ','*',file,s)
       call getplt('graphics device/type : ','*',device,s)
       if (s.eq.0) then
          call gmfplot(file,device)
       endif
       if (s.ne.0) call wrerr( s, 'in plot_metafile' )
       end

C
C
*$ Information Routines
*  --------------------
C
*+ plot_display(status)

       subroutine plot_display(status)
C      -------------------------------
C
C Display options and contour levels
*-
       integer       status

       integer       number_options
       parameter    (number_options = 4)

       character*60  display_options(number_options)
       character*60  option

       integer       len_opt, lenb
       logical       cmatch

       if (status.ne.0) return

       display_options(1) = 'options ....... parameter settings'
       display_options(2) = 'levels ........ contour levels'
       display_options(3) = 'vectors ....... vector plot options'
       display_options(4) = 'all ........... all information'

       call getopt('Option (?=list) : ','options',display_options,
     *             number_options,option,status)
       if (status.eq.0) then
         len_opt = lenb(option)
         if (cmatch(option(1:len_opt),'OPTIONS')) then
           call plot_shopt(status)

         else if (cmatch(option(1:len_opt),'LEVELS')) then
           call plot_shcnt(status)

         else if (cmatch(option(1:len_opt),'VECTORS')) then
           call plot_shvec(status)

         else if (cmatch(option(1:len_opt),'SYMBOLS')) then
           call plot_shsymb(status)

         else if (cmatch(option(1:len_opt),'ALL')) then
           call plot_shopt(status)
           call plot_shvec(status)
           call plot_shsymb(status)
           call plot_shcnt(status)

         end if

       end if

       end


*+ plot_shcnt

       subroutine plot_shcnt(status)
C      -----------------------------
C
C Display settting of contour levels
*-
$include (anmap)anmap-plot-sys:incl

C error flag and counters
       integer    status, i
C output unit
       integer    iout

C check status on entry
       if (status.ne.0) return

       call enqout(iout)
       write(iout,100)cont_total_s,cont_plot_s
       write(iout,110)solid_style,solid_colour
       if (cont_total_s .gt.0) then
         write(iout,120)(solid_cont(i), i=1,cont_total_s)
       end if
100    format(1X/1X,'Contour Levels (SOLID)'/
     >           1X,'----------------------'/
     >           1X,'  Total = ',I2,'  Plotted = ',I2)
110    format(   1X,'  Style = ',I2,'  Colour  = ',I2)
120    format(   1X,'  Levels...'/10(6X,1P5E12.2/))
       write(iout,200)cont_total_d,cont_plot_d
       write(iout,110)dash_style,dash_colour
       if (cont_total_d.gt.0) then
         write(iout,120)(dashed_cont(i), i=1,cont_total_d)
       end if
200    format(1X/1X,'Contour Levels (DASHED)'/
     >           1X,'-----------------------'/
     >           1X,'  Total = ',I2,'  Plotted = ',I2)
       write(iout,'(1x)')
       if (grey_opt) then
         write(iout,300)black_level,white_level
       end if
300    format(1X/1X,'Grey Scale Defined'/
     >           1X,'------------------'/
     >           1X,'  Black = ',1PE12.3/
     >           1X,'  White = ',1PE12.3)

       end
C
C
*+ plot_shopt

       subroutine plot_shopt(status)
C      -----------------------------
C
C Display options as setup
*-
$include (anmap)anmap-plot-sys:incl
$include (library)maplib-redtape:incl

C character option
       character  char_onoff*3
C error flag
       integer    status
C output unit
       integer    iout

C check status on entry
       if (status.ne.0) return

C get output unit
       call enqout(iout)
       write(iout,10)
10     format(1X/1X,'Options and Parameters for MAP-DISPLAY'/
     *           1X,'--------------------------------------'/1X)

C map read in
       if (map_read) then
         write(iout,100)rtitle(1),uv_range
 100     format(1X,'  Map read : Title = ',A/1X/
     >          1X,'  UV-range         = ',4I6)
         write(iout,105)region_min,region_max
 105     format(1X,'  Min/Max in range = ',1P2E12.3/1X)
       end if
       write(iout,110)char_onoff(int_opt),char_onoff(pips_opt),
     *                char_onoff(uvpip_opt),char_onoff(grid_opt),
     *                char_onoff(uvgrid_opt)
       write(iout,120)char_onoff(cont_opt),char_onoff(cont_opt)
 110   format(1X,'  Interpolation    = ',A/
     >        1X,'  Draw pips        = ',A,'  Draw UV-pips  = ',A/
     >        1X,'  Draw grid        = ',A,'  Draw UV-grid  = ',A)
 120   format(1X,'  Plot date        = ',A/
     >        1X,'  Plot levels      = ',A)
       if (scale_bar_opt) then
         write(iout,135) scale_bar_width
 135     format(1X,'  Scale-bar width  = ',F12.3)
       end if
       if (title_opt) then
         write(IOUT,130)TITLE_PLOT
 130     format(1X/1X,'  Plot-title       = ',A)
       end if
       write(iout,'(1x)')
       write(iout,150)'Solid',solid_style,solid_colour,solid_width
       write(iout,150)'Dash ',dash_style,dash_colour,dash_width
 150   format(1X,'  Contours ',A,':  Style = ',I1,'  Colour = ',I1,
     >           '  Width = ',I2)
       write(iout,160)mark_font,mark_size,text_font,text_size
 160   format(1X/1X,'  Markings:   font = ',I1,'  text-size = ',F6.2/
     >           1X,'  Text    :   font = ',I1,'  text-size = ',F6.2)
       write(iout,'(1x)')

       if (grey_opt) then
         write(iout,300)black_level,white_level
       end if
300    format(1X/1X,'Grey Scale Defined'/
     >           1X,'------------------'/
     >           1X,'  Max.  = ',1PE12.3/
     >           1X,'  Min.  = ',1PE12.3)

       end
C
C
*+ plot_shvec

       subroutine plot_shvec(status)
C      -----------------------------
C
C Display vector plot options
*-
       $include (anmap)anmap-plot-sys:incl
       $include (library)maplib-redtape:incl
       $include (library)chrlib-functions:incl

C error flag
       integer    status
C output unit
       integer    iout
C character strings
       character  source*30, program*8

C check status on entry
       if (status.ne.0) return

C get output unit
       call enqout(iout)
       write(iout,10)
10     format(1X/1X,'Vector Plot Option for MAP-DISPLAY'/
     *           1X,'----------------------------------'/1X)

C map read in
       if (vectors_opt) then
         write(iout,'(1X,A)') '  Plotting of Vectors Enabled'
       else
         write(iout,'(1X,A)') '  Plotting of Vectors NOT Enabled'
       end if
       if (vec_chi_map.ne.0) then
         call mapcat_enqsr(vec_chi_map,source,program,status)
         write(iout,100)vec_chi_map,source(1:lenb(source)),'-',program
 100     format(1X,'  Position-Angle-Map [',i3,'] = ',A,A1,A)
       end if
       if (vec_int_map.ne.0) then
         call mapcat_enqsr(vec_int_map,source,program,status)
         write(iout,110)vec_int_map,source(1:lenb(source)),'-',program
 110     format(1X,'  Intensity-Map      [',i3,'] = ',A,A1,A)
       end if

       if (vec_type.eq.0) then
         write(iout,120) 'Vectors scaled by intensity map',vec_scale,
     *                   'scale (grid-points/map-units)'
       else if (vec_type.eq.1) then
         write(iout,120) 'Vectors scaled by intensity map',vec_length,
     *                   'maximum length (grid-points)'
       else if (vec_type.eq.1) then
         write(iout,120) 'Constant length vectors',vec_length,
     *                   'length (grid-points)'
       end if
120    format(1x/1x,'  Plot-Type   : ',A/
     *           1x,'  Length-Type : ',F10.4,' ',A)
       write(iout,130)vec_gate,vec_rotate,vec_u_samp,vec_v_samp
130    format(1x,   '  Gate        : ',1PE10.2/
     *        1x,   '  Rotation    : ',1PE10.2/
     *        1x,   '  Sampling-UV : ',2i5/1x)
       end
C
C
*+ plot_shsymb

       subroutine plot_shsymb(status)
C      ------------------------------
C
C Display symbol plot options
*-
       $include (anmap)anmap-plot-sys:incl
       $include (library)chrlib-functions:incl

C error flag
       integer    status
C output unit
       integer    iout
C character conversion
       integer*2  i2

C check status on entry
       if (status.ne.0) return

C get output unit
       call enqout(iout)
       write(iout,10)
10     format(1X/1X,'Symbol Plot Option for MAP-DISPLAY'/
     *           1X,'----------------------------------'/1X)

C map read in
       if (symbol_opt) then
         write(iout,'(1X,A)') '  Plotting of Symbols Enabled'
       else
         write(iout,'(1X,A)') '  Plotting of Symbols NOT Enabled'
       end if
       if (symb_blank.gt.0) then
         i2 = symb_blank
         write(iout,100) char(i2)
 100     format(1X,'  Marking blank pixels character     : ',A1)
       end if
       if (symb_min.gt.0) then
         i2=symb_min
         write(iout,110) char(i2),val_symb_min
 110     format(1X,'  Marking minimum pixels character   : ',A1/
     *          1X,'  Lower gate for pixels to be marked : ',1PE12.3)
       end if
       if (symb_max.gt.0) then
         i2=symb_max
         write(iout,120) char(i2),val_symb_max
 120     format(1X,'  Marking maximum pixels character   : ',A1/
     *          1X,'  Upper gate for pixels to be marked : ',1PE12.3)
       end if
       write(iout,*) ' '
       end
C
C
       CHARACTER*3 FUNCTION CHAR_ONOFF(OPTION)
C      ---------------------------------------
C
C output character representation for logical option
*-
       logical option

       if (option) then
         char_onoff = 'on '
       else
         char_onoff = 'off'
       end if

       end
C
C
*+ plot_get

       subroutine plot_get(status)
C      ---------------------------
C
C Enquire the value of the named quantity and return as a parameter
C
C Returned:
C   error status return
       integer          status
C
C The value of the requested quantity is returned in the named command
C langauge parameter.  The options provided by this routine are concerned
C with access to information specific to the current plot.
C
C The following quantities may be enquired:
C   MAP-POSITION      -      read position in map optionally using cursor
C   UV-RANGE          -      prompt for a UV-range optionally using cursor
*-
       $include (library)chrlib-functions:incl
       $include (library)io-constants:incl
C
C variables used locally
       character*30   quantity, parameter
       character*80   value
       character*4    number
       integer        len_q, len_p, len_v
       integer        uv_pos(2), uv_range(4), data_type
       integer        n, i1, i2, imap, iin
C define the quantites to be enquired
       integer        number_quantities
       parameter     (number_quantities = 2)
       character*20   quantities(number_quantities)
       data           quantities(1)/'MAP-POSITION       '/
       data           quantities(2)/'UV-RANGE           '/

C check status on entry
       if (status.ne.0) return

C find quantity to enquire and parameter name for result
       call getwrd('Quantity : ','UV-RANGE',quantity,len_q,status)
       parameter = '%'//quantity(1:len_q)
       call getwrd('Parameter-name : ','*',parameter,len_p,status)
       if (status.ne.0) goto 999

C search quantity list and take action
       value = ' '
C .. MAP-POSITION
       if (cmatch(quantity(1:len_q),quantities(1))) then
         call enqin(iin)
         call setin(terminal_in)
         call plt_rdpos('Map-position (UV) : ',uv_pos,status)
         call setin(iin)
         if (status.eq.0) then
           value = ' '
           do n=1,2
             call chitoc(uv_pos(n),number,i2)
             i1 = intlc(number)
             len_v = lenb(value)
             if (n.eq.1) then
               value = number(i1:i2)
             else
               value = value(1:len_v)//','//number(i1:i2)
             end if
           end do
         end if

C .. UV-RANGE
       else if (cmatch(quantity(1:len_q),quantities(2))) then
         call enqin(iin)
         call mapcat_getmap('Map-entry : ','Default-Map','READ',
     *                      imap,status)
         call redt_load(imap,status)
         call enredt(uv_range,data_type,status)
         call setin(terminal_io)
         call plt_getuv('UV-range : ','*',uv_range,status)
         call setin(iin)
         if (status.eq.0) then
           value = ' '
           do n=1,4
             call chitoc(uv_range(n),number,i2)
             i1 = intlc(number)
             len_v = lenb(value)
             if (n.eq.1) then
               value = number(i1:i2)
             else
               value = value(1:len_v)//','//number(i1:i2)
             end if
           end do
         end if

C .. unknown quantity
       else
         call wrout('*** Unknown quantity in MAP-DISPLAY GET')
         goto 999
       end if
       len_v = lenb(value)
       call cmd_setparam(parameter(1:len_p),value(1:len_v),status)
999    call cmd_err(status,'plot_get',' ')

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
       $include (anmap)anmap-plot-sys:incl

C local variables
       integer     max_size
       parameter  (max_size = 512)
       real*4      x_axis(max_size), work(max_size)
       integer     minirt(10), ixr(4)
       real*4      bias, zmnx(4), x1, x2, y1, y2
       integer     ioff, iscr, ip_scr, i, j, izmnx(4)
       logical     cmd_dblev

       if (.not.map_read) then
         call cmd_wrerr('SURFACE','No map defined: use SET-MAP')
         return
       end if

C read map
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call enminirt( minirt, status )
       call map_end_alloc( imap, map_array, status )

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
       call getr('Offset between rows (map-units) : ','*',bias,status)
       ioff = 0
       call geti('Offset between columns (pixels) : ','*',ioff,status)
C flip image
       call map_alloc_scr( 512, 512, 'DIRECT', iscr, ip_scr, status)
       do j=1,minirt(6)
         do i=1,minirt(5)
           map_array(ip_scr+i-1+(j-1)*minirt(5)) =
     *              map_array(ip_map+i-1+(minirt(6)-j)*minirt(5))
         end do
       end do
       call map_end_alloc( iscr, map_array, status )

C check errors
       if (status.ne.0) then
          call cmd_err(status,'SURFACE',' ')
          return
       end if

C open the plot device
       do i=uv_range(1),uv_range(2)
         x_axis(i-uv_range(1)+1) = i
       end do
       if (offset.ge.0) then
         x1 = x_axis(1)
         x2 = x_axis(ixr(2)-ixr(1)+1)+offset
       else
         x1 = x_axis(1)+offset
         x2 = x_axis(ixr(2)-ixr(1)+1)
       end if
       y1 = zmnx(2)
       y2 = zmnx(2)+bias*float(ixr(4)-ixr(3))
       call getr('X-low : ','*',x1,status)
       call getr('X-high : ','*',x2,status)
       call getr('Y-low : ','*',y1,status)
       call getr('Y-high : ','*',y2,status)
       call plt_grinit(status)
       if (status.ne.0) goto 999

C do plot
       call pgbbuf
       call pgwindow( x1, x2, y1, y2 )
       call pghi2d(map_array(ip_scr),minirt(5),minirt(6),
     *             ixr(1),ixr(2),ixr(3),ixr(4),x_axis,ioff,bias,
     *             .true.,work)
       call pgebuf
       call plt_grend(status)
999    call cmd_err( status,'SURFACE-PLOT',' ')
       end
