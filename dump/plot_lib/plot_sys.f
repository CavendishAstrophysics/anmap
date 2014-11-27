*$ Map-Display Routine Library
*  ===========================
C
C P. Alexander MRAO, Cambridge.
C
*+ plot_sys

       subroutine plot_sys(interp,cdata,map_array,status)
C      --------------------------------------------------
C
C Interactive system to display images
C
C Updated:
C    command interpreter data structure
       integer       interp(*)
C    command language data structure
       integer       cdata(*)
C    map work space
       real*4          map_array(*)
C    error status
       integer         status
C
C A command line driven image plotting sub-system.  The calling program
C must use the MAP-CATALOGUE and stack system.  The work array is
C passed by argument to this routine.  All plotting uses the PGPLOT library.
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/cmd_lang_data.inc'

C define variables to access commands
       integer         number_commands, i_com_done, icomm,
     *                 number_options
       parameter      (number_commands = 26, number_options=10)
       character*70    liscom(number_commands),
     *                 option_list(number_options), option

C parameters holds information on the command line
       character*80    parameters
       integer         len_cli
C map allocation
       integer         ip_map
C logical flag set if the sub-system is called in the form:
C   map-display command-name options
C from another sub-system
       logical         exit_at_end
C counter
       integer         i

C define commands
       data liscom(1)
     *  / 'set-map .................. define map to display'/
       data liscom(2)
     *  / 'overlay-map .............. define second map to display'/
       data liscom(3)
     *  / 'set-pips ................. turn pips on/off [on]'/
       data liscom(4)
     *  / 'set-grid ................. turn grid on/off [off,off]'/
       data liscom(5)
     *  / 'set-uv-range ............. define uv-range to plot [full]'/
       data liscom(6)
     *  / 'set-interpolation ........ interpolation option [off]'/
       data liscom(7)
     *  / 'set-style ................ set various style options'/
       data liscom(8)
     *  / 'grey-scale ............... define (overlay) of grey scale'/
       data liscom(9)
     *  / 'vector-plot .............. define (overlay) of vectors'/
       data liscom(10)
     *  / 'symbol-plot .............. define (overlay) of symbols'/
       data liscom(11)
     *  / 'contours ................. specify a list of contours'/
       data liscom(12)
     *  / 'linear-contours .......... define linear contour levels'/
       data liscom(13)
     *  / 'logarithmic-contours ..... define log. contour levels'/
       data liscom(14)
     *  / 'reset-contour-levels ..... reset all contour levels'/
       data liscom(15)
     *  / 'clear-contour-levels ..... clear all contour levels'/
       data liscom(16)
     *  / 'plot ..................... plot portions of the plot'/
       data liscom(17)
     *  / 'initialise ............... initialise aspects of plot'/
       data liscom(18)
     *  / 'display .................. display options and levels'/
       data liscom(19)
     *  / 'title-plot ............... add title to plot'/
       data liscom(20)
     *  / 'modify-lookup-table ...... modify LUT for non-TV device'/
       data liscom(21)
     *  / 'cursor-position .......... return map position and value'/
       data liscom(22)
     *  / 'get ...................... read options into parameters'/
       data liscom(23)
     *  / 'surface-plot ............. plot a map as a surface relief'/
       data liscom(24)
     *  / 'isometric-plot ........... plot map as isometric surface'/
       data liscom(25)
     *  / 'annotate ................. annotate the plot'/
       data liscom(26)
     *  / 'crosses-file ............. specify a crosses file'/

C check whether to exit on completion of command
       call io_enqcli(parameters,len_cli)
       exit_at_end = len_cli.ne.0
       i_com_done = 0
       do i=1,cmd_data_len
         cmd_data(i) = cdata(i)
       enddo

C initialise graphic structure
       call graphic_init( graphic_image, image_defn, status)
       annot_data(1) = len_annot_data

1000   continue
       status = 0
       if (exit_at_end .and. i_com_done.ne.0) then
         do i=1,cmd_data_len
           cdata(i) = cmd_data(i)
         enddo
         cdata(1) = 100
         call graphic_end( graphic_image, image_defn, status)
         return
       end if

C decode commmands
       call plot_frset(status)
       call cmd_getcmd('Map-Display> ',liscom,
     *                  number_commands,icomm,status)

C check for error
       if (status.ne.0) then
         call cmd_err(status,'MAP-DISPLAY',' ')
         goto 1000
       end if

C exit for basic command
       if (icomm.le.0) then
         do i=1,cmd_data_len
           cdata(i) = cmd_data(i)
         enddo
         cdata(1) = icomm
         call graphic_end( graphic_image, image_defn, status)
         return
       end if

C increment command counter
       i_com_done = i_com_done + 1

C decode command
       if (chr_cmatch('set-map',liscom(icomm))) then
         call plot_setmap(.false.,map_array,status)
         cmd_results = plotsys_setmap_command
       elseif (chr_cmatch('overlay-map',liscom(icomm))) then
         option_list(1) = 'set ........... specify an overlay-map'
         option_list(2) = 'off ........... turn overlay-map off'
         option_list(3) = 'on ............ turn overlay-map on'
         option_list(4) = 'current-main .. select main map as current'
         option_list(5) = 'current-overlay select overlay as current'
         call io_getopt('Overlay-option (?=help) : ','ALL',
     *                   option_list,5,option,status)
         if (chr_cmatch('set',option)) then
           call plot_setmap(.true.,map_array,status)
           overlay_defined = .true.
           overlay_map = .true.
         elseif (chr_cmatch('off',option)) then
           overlay_map = .false.
         elseif (chr_cmatch('on',option)) then
           overlay_map = .true.
         elseif (chr_cmatch('current-main',option)) then
           imap_current = 1
         elseif (chr_cmatch('current-overlay',option)) then
           imap_current = 2
         endif

       elseif (chr_cmatch('set-pips',liscom(icomm))) then
         pips_opt = io_onoff('Pips on/off : ','on',status)
         if (pips_opt) then
            call plot_getpip(.true.,status)
         endif
         if (pips_opt) then
           uvpip_opt = io_onoff('UV-pips on/off : ','off',status)
         else
           uvpip_opt = io_onoff('UV-pips on/off : ','on',status)
         endif

       elseif (chr_cmatch('set-grid',liscom(icomm))) then
         grid_opt = io_onoff('RA/DEC grid on/off : ','off',status)
         uvgrid_opt = io_onoff('UV grid on/off : ','off',status)
         if (uvgrid_opt) then
           call io_getr('Spacing in U (0=default) : ','0',grid_u,status)
           call io_getr('Spacing in V (0=default) : ','0',grid_v,status)
         end if

       elseif (chr_cmatch('set-uv-range',liscom(icomm))) then
         call plot_getuv('UV-range : ','*',uv_range,status)
         call plot_getpip(.false.,status)
         call cmd_exec(plotsys_setuv_command,status)
         cmd_results = plotsys_setuv_command

       elseif (chr_cmatch('set-interpolation',liscom(icomm))) then
         interpolate_opt =
     *      io_onoff('Interpolation on/off : ','off',status)

       elseif (chr_cmatch('set-style',liscom(icomm))) then
         call plot_style(status)

       elseif (chr_cmatch('grey-scale',liscom(icomm))) then
         image_opt = io_onoff('Grey-scaling on/off : ','off',status)
         if (image_opt) then
           image_min = data_range(1,imap_current)
           image_max = data_range(2,imap_current)
           call io_getr('Image-min : ','*',image_min,status)
           call io_getr('Image-max : ','*',image_max,status)
         end if

       elseif (chr_cmatch('vector-plot',liscom(icomm))) then
         call plot_vectors(status)

       elseif (chr_cmatch('symbol-plot',liscom(icomm))) then
         call plot_symbols(status)

       elseif (chr_cmatch('contours',liscom(icomm))) then
         call plot_ctdefine(status)

       elseif (chr_cmatch('linear-contours',liscom(icomm))) then
         call plot_ctlin(status)

       elseif (chr_cmatch('logarithmic-contours',liscom(icomm))) then
         call plot_ctlog(status)

       elseif (chr_cmatch('clear-contour-levels',liscom(icomm))) then
         option_list(1) = 'all ........... clear all levels'
         option_list(2) = 'type .......... clear specific type'
         call io_getopt('Clear-option (?=help) : ','ALL',
     *                   option_list,2,option,status)
         if (chr_cmatch('all',option)) then
           i = 0
         else
           call io_geti('Contour-type (0=all) : ','0',i,status)
         endif
         call plot_ctclear(i,status)

       elseif (chr_cmatch('reset-contour-levels',liscom(icomm))) then
         option_list(1) = 'all ........... clear all levels'
         option_list(2) = 'type .......... clear specific type'
         call io_getopt('Reset-option (?=help) : ','ALL',
     *                   option_list,2,option,status)
         if (chr_cmatch('all',option)) then
           i = 0
         else
           call io_geti('Contour-type (0=all) : ','0',i,status)
         endif
         call plot_ctreset(i,status)

       elseif (chr_cmatch('plot',liscom(icomm))) then
         option_list(1) = 'all .......... plot everything'
         option_list(2) = 'refresh ...... refresh current plot'
         option_list(3) = 'grey ......... plot grey scale'
         option_list(4) = 'frame ........ plot frame'
         option_list(5) = 'contours ..... plot contours'
         option_list(6) = 'symbol ....... plot symbols'
         option_list(7) = 'vectors ...... plot vectors'
         option_list(8) = 'text ......... plot text and titles'
         option_list(9) = 'annotations .. plot annotations'
         option_list(10)= 'crosses ...... plot crosses file'
         call io_getopt('Plot-option (?=help) : ','ALL',
     *                   option_list,10,option,status)
         call plot_doplot(option,map_array,status)
         call cmd_err(status,'PLOT',' ')

       elseif (chr_cmatch('initialise',liscom(icomm))) then
         option_list(1) = 'all .......... initialise everything'
         option_list(2) = 'plot ......... initialise for (re-)plotting'
         option_list(3) = 'options ...... options for the plot'
         option_list(4) = 'setup ........ definition of maps etc.'
         option_list(5) = 'annotations .. initailise annotations'
         option_list(6) = 'title ........ title for plot'
         call io_getopt('Initialise-option (?=help) : ','ALL',
     *                   option_list,6,option,status)

         if ( chr_cmatch(option,'plot') .or.
     *        chr_cmatch(option,'all')    ) then
           call plot_init_plot( status )
           call annotate_init( annot_data, option, status )
         endif
         if ( chr_cmatch(option,'options') .or.
     *        chr_cmatch(option,'all')    ) then
           call plot_init_opt( status )
           call annotate_init( annot_data, option, status )
         endif
         if ( chr_cmatch(option,'setup') .or.
     *        chr_cmatch(option,'all')    ) then
           call plot_init_setup( status )
           call annotate_init( annot_data, option, status )
         endif
         if ( chr_cmatch(option,'annotations') .or.
     *        chr_cmatch(option,'all')    ) then
           call annotate_init( annot_data, 'all', status )
         endif
         if ( chr_cmatch(option,'title') .or.
     *        chr_cmatch(option,'all')    ) then
           title_plot = ' '
           title_opt = .false.
           title_done = .false.
         endif

       elseif (chr_cmatch('display',liscom(icomm))) then
         call plot_display(status)

       elseif (chr_cmatch('title-plot',liscom(icomm))) then
         call io_getstr('Plot-title : ',' ',title_plot,status)
         title_opt = chr_lenb(title_plot).gt.0
         title_done = .false.

       elseif (chr_cmatch('modify-lookup-table',liscom(icomm))) then
         call plot_TVmod(status)

       elseif (chr_cmatch('cursor-position',liscom(icomm))) then
         call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
         call redt_load( imap, status )
         call plot_cursor_read( map_array(ip_map), .true., status )
         call map_end_alloc( imap, map_array, status )

       elseif (chr_cmatch('get',liscom(icomm))) then
         call plot_get( map_array, status )

       elseif (chr_cmatch('surface-plot',liscom(icomm))) then
         call plot_surface( map_array, status )

       elseif (chr_cmatch('isometric-plot',liscom(icomm))) then
         call plot_isometric( map_array, status )

       elseif (chr_cmatch('annotate',liscom(icomm))) then
         call graphic_open( image_defn, status )
         call plot_frinit( .true., status )
         call annotate( annot_data, status )

       elseif (chr_cmatch('crosses-file',liscom(icomm))) then
         option_list(1) = 'on ........... crosses file plotting on'
         option_list(2) = 'off .......... crosses file plotting off'
         option_list(3) = 'file ......... set name of crosses file'
         option_list(4) = 'size ......... set size of crosses'
         option_list(5) = 'extended ..... file contains style info.'
         call io_getopt('Crosses-option (?=help) : ','off',
     *                   option_list,5,option,status)

         if ( chr_cmatch(option,'on') ) then
           crosses_opt = 1
           if (chr_lenb(crosses_file).eq.0) then
             call io_getfil('Crosses-file : ',' ',crosses_file,status)
           endif
         elseif ( chr_cmatch(option,'extended') ) then
           crosses_opt = 2
           if (chr_lenb(crosses_file).eq.0) then
             call io_getfil('Crosses-file : ',' ',crosses_file,status)
           endif
         elseif ( chr_cmatch(option,'off') ) then
           crosses_opt = 0
         elseif ( chr_cmatch(option,'file') ) then
           call io_getfil('Crosses-file : ',' ',crosses_file,status)
         elseif ( chr_cmatch(option,'size') ) then
           call io_getr('Cross-size : ','*',crosses_size,status)
         endif

       endif

       goto 1000
       end





