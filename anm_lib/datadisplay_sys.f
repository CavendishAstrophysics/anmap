*+ datadisplay_sys

       subroutine datadisplay_sys( interp, cdata, map_array, status )
C      --------------------------------------------------------------
C
C Sub-system to display data "files"
C
C
C Updated:
C    interpreter data structure
       integer       interp(*)
C    command language data structure
       integer       cdata(*)
C    map data array
       real*4        map_array(*)
C Returned:
C    error status
       integer       status
C
C This sub-system uses the object-oriented Anmap graphics model to display
C data files.  A data structure of type graph is used to store
C the information concerning the display.  The files to be displayed must
C be internal files (not implemented yet) or external spectral-type files.
C
C P. Alexander, MRAO, Cambridge
C Version 1.0. March 1993
C-

C include standard include definitions
       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C sub-system specific includes
       include '../include/plt_buffer_defn.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'
       include '../include/plt_graph_defn.inc'

C Local variables
C ---------------
C  command line, length and initial check
       character*120      command_line
       integer            len_com, i_comm_done
       logical            exit_on_completion
C  command line interpretation
       integer            number_commands, icom
       parameter         (number_commands = 15)
       character*80       liscom(number_commands),
     *                    opt, opts(6)

C define commands
       data liscom(1) /
     * 'set-frame-style .......... set style options for frame'
     *                /
       data liscom(2) /
     * 'set-text-style ........... set text options for titles'
     *                /
       data liscom(3) /
     * 'set-title-options ........ set options for title positions'
     *                /
       data liscom(4) /
     * 'set-key-options .......... set options for key'
     *                /
       data liscom(5) /
     * 'line-options ............. specify options for each line'
     *                /
       data liscom(6) /
     * 'title .................... specify title for the plot'
     *                /
       data liscom(7) /
     * 'x-title .................. specify x-title for the plot'
     *                /
       data liscom(8) /
     * 'y-title .................. specify y-title for the plot'
     *                /
       data liscom(9) /
     * 'x-range .................. range for X-axis values'
     *                /
       data liscom(10)/
     * 'y-range .................. range for Y-axis values'
     *                /
       data liscom(11)/
     * 'view-port ................ (sub) view-port for the graph'
     *                /
       data liscom(12)/
     * 'plot ..................... plot or update the current graph'
     *                /
       data liscom(13)/
     * 'initialise ............... initialise options for the graph'
     *                /
       data liscom(14)/
     * 'annotate ................. annotate the graph'
     *                /
       data liscom(15)/
     * 'get ...................... get information for the graph'
     *                /

C counters and pointers
       integer    n, l, l1
       character  string*80
C buffers to data sructure
       integer    gopt(len_grline), gsave(len_grline)

C check status on entry
       if (status.ne.0) return
       call io_enqcli( command_line, len_com )
       exit_on_completion = len_com.ne.0
       i_comm_done = 0

C initialise the text drawing graphic
       call graphic_init(graphic_graph, graph, status)
       call graphic_copy_graphic(graph, graphic, status)
       call graph_setup_coords( .false., status )
       annot_data(1) = len_annot_data

C command loop-back
100    continue
       if (exit_on_completion .and. i_comm_done.gt.0) then
           cdata(1) = 100
           call graphic_end(graphic_graph, graph, status)
           return
       end if
       status = 0
       call cmd_getcmd( 'Data-display> ',liscom,number_commands,
     *                  icom,status)

       if (status.ne.0) then
         call cmd_err(status,'Data-display',' ')
         goto 100
       else
         i_comm_done = i_comm_done + 1
       end if

       if (icom.le.0) then
         status = 0
         cdata(1) = icom
         call graphic_end(graphic_graph, graph, status)
         return
       end if

C decode command
       if (chr_cmatch('set-frame-style',liscom(icom))) then
         call graphic_get_frame_opt( graph_frame, status)

       elseif (chr_cmatch('set-text-style',liscom(icom))) then
         call graphic_get_text_opt( graph_title_opt, status)

       elseif (chr_cmatch('set-title-options',liscom(icom))) then
         opts(1)='title ......... options for title'
         opts(2)='x-title ....... options for x-title'
         opts(3)='y-title ....... options for y-title'
         call io_getopt( 'Title-option (?=list) : ','title',
     *                   opts, 3, opt, status )
         if (chr_cmatch('title',opt)) then
           call io_geti('Title-position (1=Top, 2=Bottom) : ','*',
     *                  graph_opt_t,status)
           if (graph_opt_t.le.1) then
             graph_opt_t = 1
           else
             graph_opt_t = 2
           endif
           call io_getr('Outwards displacement (characters) : ','*',
     *                  graph_posn_t(1),status)
           call io_getr(
     *             'Title location on frame (0.0=left 1.0=right) : ',
     *             '*',graph_posn_t(2),status)
           call io_getr(
     *             'Justification  (0.0=left 0.5=centre 1.0=right): ',
     *             '*',graph_posn_t(3),status)

         elseif (chr_cmatch('x-title',opt)) then
           call io_geti('X-title-position (1=Top, 2=Bottom) : ','*',
     *                  graph_opt_xt,status)
           if (graph_opt_xt.le.1) then
             graph_opt_xt = 1
           else
             graph_opt_xt = 2
           endif
           call io_getr('Outwards displacement (characters) : ','*',
     *                  graph_posn_xt(1),status)
           call io_getr(
     *             'X-title location on frame (0.0=left 1.0=right) : ',
     *             '*',graph_posn_xt(2),status)
           call io_getr(
     *             'Justification  (0.0=left 0.5=centre 1.0=right): ',
     *             '*',graph_posn_xt(3),status)

         elseif (chr_cmatch('y-title',opt)) then
           call io_geti(
     *      'Y-title-position (1=left, 2=right 3=left/V 4=right/V ) : ',
     *      '*',graph_opt_yt,status)
           if (graph_opt_yt.le.1) then
             graph_opt_yt = 1
           elseif (graph_opt_yt.gt.4) then
             graph_opt_yt = 4
           endif
           call io_getr('Outwards displacement (characters) : ','*',
     *                  graph_posn_yt(1),status)
           call io_getr(
     *             'Y-title location on frame (0.0=left 1.0=right) : ',
     *             '*',graph_posn_yt(2),status)
           call io_getr(
     *             'Justification  (0.0=left 0.5=centre 1.0=right): ',
     *             '*',graph_posn_yt(3),status)
         endif

       elseif (chr_cmatch('set-key-options',liscom(icom))) then
         opts(1)='on ............ turn key on'
         opts(2)='off ........... turn key off'
         opts(3)='position ...... set key position'
         opts(4)='frame ......... frame around key'
         opts(5)='text-style .... text-style for key'
         call io_getopt( 'Key-option (?=list) : ','off',
     *                   opts, 5, opt, status )
         if (chr_cmatch(opt,'on')) then
           if (graph_key_status.eq.0) graph_key_status = 1
         elseif (chr_cmatch(opt,'off')) then
           graph_key_status = 0
         elseif (chr_cmatch(opt,'position')) then
           call io_getnr('Position in frame (0-1;0-1) : ',
     *                   '*',graph_key_opts,2,status)
         elseif (chr_cmatch(opt,'frame')) then
           if (io_onoff('Frame around key (on/off) : ',
     *         'off',status)) then
             graph_key_opts(3) = 1.0
           else
             graph_key_opts(3) = 0.0
           endif
         elseif (chr_cmatch(opt,'text-style')) then
           call graphic_get_text_opt(graph_key_text,status)
           call io_getr('Text drop : ','*',graph_key_opts(4),status)
         endif

       elseif (chr_cmatch('line-options',liscom(icom))) then
C .. prompt for line to modify
         if (l1.eq.0) l1 = 1
         call io_geti('Line-number (0=all) : ','*',l1,status)
         if (l1.le.0) l1 = 0
         if (l1.gt.graph_max_lines) l1 = graph_max_lines
C .. setup default
         if (l1.eq.0) then
           call graphic_default_grline( gopt, 3, status )
         else
           call graphic_copy_grline( graph_grline(1,l1), gopt, status )
         endif
C .. get options
         call graphic_copy_grline( gopt, gsave, status )
         call graphic_get_grline( gopt, graph_ntb, graph_itext,
     *                            graph_text, status )
C .. update options
         if (l1.ne.0) then
           call graphic_copy_grline( gopt, graph_grline(1,l1), status )
         else
           do n=1,graph_max_lines
              do l=1,len_grline
                if (gopt(l).ne.gsave(l)) then
                  graph_grline(l,n) = gopt(l)
                endif
              enddo
           enddo
         endif

       elseif (chr_cmatch('title',liscom(icom))) then
         string = ' '
         call io_getstr('Title : ',' ',string,status)
         call graphic_delete_text( graph_ntb, graph_itext,
     *                             graph_text, graph_title, status )
         call graphic_put_text( graph_ntb, graph_itext,
     *                          graph_text, string,
     *                          graph_title, status )

       elseif (chr_cmatch('x-title',liscom(icom))) then
         string = ' '
         call io_getstr('X-title : ',' ',string,status)
         call graphic_delete_text( graph_ntb, graph_itext,
     *                             graph_text, graph_x_title, status )
         call graphic_put_text( graph_ntb, graph_itext,
     *                          graph_text, string,
     *                          graph_x_title, status )

       elseif (chr_cmatch('y-title',liscom(icom))) then
         string = ' '
         call io_getstr('Y-title : ',' ',string,status)
         call graphic_delete_text( graph_ntb, graph_itext,
     *                             graph_text, graph_y_title, status )
         call graphic_put_text( graph_ntb, graph_itext,
     *                          graph_text, string,
     *                          graph_y_title, status )

       elseif (chr_cmatch('view-port',liscom(icom))) then
         call io_getnr( 'View-port : ','*',
     *                  graph_view_port,4,status)
         if (graph_view_port(1).lt.0.0 .or.
     *       graph_view_port(1).gt.1.0) then
           graph_view_port(1) = 0.1
         endif
         if (graph_view_port(2).lt.0.0 .or.
     *       graph_view_port(2).gt.1.0) then
           graph_view_port(2) = 0.9
         endif
         if (graph_view_port(3).lt.0.0 .or.
     *       graph_view_port(3).gt.1.0) then
           graph_view_port(3) = 0.1
         endif
         if (graph_view_port(4).lt.0.0 .or.
     *       graph_view_port(4).gt.1.0) then
           graph_view_port(4) = 0.1
         endif
         graph_view_port_opt = 1
         call graph_setup_coords( .false., status )

       elseif (chr_cmatch('x-range',liscom(icom))) then
         call io_getnr( 'X-range (0,0 = default) : ','*',
     *                   graph_xrange,2,status)
         if (graph_xrange(1).ne.graph_xrange(2)) then
           graph_xcoord_opt =  1
         else
           graph_xcoord_opt = 0
         endif
         call graph_setup_coords( .false., status )

       elseif (chr_cmatch('y-range',liscom(icom))) then
         call io_getnr( 'Y-range (0,0 = default) : ','*',
     *                   graph_yrange,2,status)
         if (graph_yrange(1).ne.graph_yrange(2)) then
           graph_ycoord_opt =  1
         else
           graph_ycoord_opt = 0
         endif
         call graph_setup_coords( .false., status )

       elseif (chr_cmatch('plot',liscom(icom))) then
         opts(1)='all ........... plot everyting'
         opts(2)='line .......... plot specified line'
         opts(3)='annotations ... plot all annotations'
         opts(4)='frame ......... plot the frame'
         opts(5)='refresh ....... refresh the whole graph'
         call io_getopt( 'Plot-option (?=list) : ','all',
     *                   opts, 5, opt, status )
         if (chr_cmatch(opt,'all')) then
           plot_refresh = .false.
           call graph_plot( interp, 1, 0, status )
           call annotate_plot( annot_data, 'all', status )
         elseif (chr_cmatch(opt,'refresh')) then
           plot_refresh = .true.
           call graph_plot( interp, 1, 0, status )
           call graph_setup_coords( .false., status )
           call annotate_plot( annot_data, 'all', status )
           plot_refresh = .false.
         elseif (chr_cmatch(opt,'line')) then
           call io_geti('Line (0=all; -1=outstanding) : ','-1',
     *                   n, status )
           plot_refresh = .false.
           if (n.eq.-1) then
             n = 0
           endif
           call graph_plot( interp, -1, n, status )
         elseif (chr_cmatch(opt,'frame')) then
           plot_refresh = .false.
           call graph_init( 'plot', status )
           call graph_plot( interp, 0, 0, status )
         elseif (chr_cmatch(opt,'annotations')) then
           call graph_setup_coords( .false., status )
           call annotate_plot( annot_data, 'all', status )
         endif

       elseif (chr_cmatch('initialise',liscom(icom))) then
         opts(1)='all ........... initialise everything for drawing'
         opts(2)='plot .......... initialise for (re-)plotting'
         opts(3)='options ....... initialise options'
         opts(4)='annotations ... initialise objects in drawing'
         opts(5)='title ......... initialise titles'
         opts(6)='setup ......... initialise setup'
         call io_getopt( 'Initialise-option (?=list) : ','all',
     *                   opts, 6, opt, status )
         if (.not.chr_cmatch('annotations',opt)) then
           call graph_init( opt, status )
           call annotate_init( annot_data, opt, status )
         else
           call annotate_init( annot_data, 'all', status )
         endif

       elseif (chr_cmatch('get',liscom(icom))) then
         call graph_setup_coords( .false., status )
         call graph_get( status )

       elseif (chr_cmatch('annotate',liscom(icom))) then
         call graph_setup_coords( .true., status )
         call annotate( annot_data, status )

       end if
       goto 100

       end

C
C
*+ graph_setup_coords

       subroutine graph_setup_coords( init_device, status )
C      ----------------------------------------------------
C
C Establish the graph coordinate system on the current device
C
C Given:
C   flag to request device is initialsed if required
       logical    init_device
C Updated:
C   error status
       integer    status
C
C-
       include '../include/plt_buffer_defn.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'
       include '../include/plt_graph_defn.inc'

C local variables
       real*4   xr, yr, x1, x2, y1, y2

       if (status.ne.0) return

C check for an open device
       if ((.not.plot_open .or. plot_next) .and. init_device) then
         call graphic_open( graph, status )
       endif

C initialise view-port
       call graphic_copy_graphic(graph,graphic,status)
       if (graph_view_port_opt.eq.1) then
         xr = (graphic_view_port(2)-graphic_view_port(1))
         yr = (graphic_view_port(4)-graphic_view_port(3))
         x1 = graphic_view_port(1) + graph_view_port(1)*xr
         x2 = graphic_view_port(1) + graph_view_port(2)*xr
         y1 = graphic_view_port(3) + graph_view_port(3)*yr
         y2 = graphic_view_port(3) + graph_view_port(4)*yr
         call pgsetvp( x1,x2,y1,y2 )
       else
         call pgsetvp( graphic_view_port(1), graphic_view_port(2),
     *                 graphic_view_port(3), graphic_view_port(4) )
       endif

C setup coordinates on the view-port
       if (graph_xcoord_opt.ne.0 .and. graph_ycoord_opt.ne.0) then
           call pgsetwin(graph_coords(1),graph_coords(2),
     *                   graph_coords(3),graph_coords(4))
       elseif (graph_xcoord_opt.ne.0) then
           call pgsetwin(graph_coords(1),graph_coords(2),0.0,1.0)
       elseif (graph_ycoord_opt.ne.0) then
           call pgsetwin(0.0,1.0,graph_coords(3),graph_coords(4))
       else
           call pgsetwin(0.0,1.0,0.0,1.0)
       endif

       call cmd_err(status,'graph_setup_coords',' ')
       end
C
C
*+ graph_plot

       subroutine graph_plot( interp, plot_frame, line, s )
C      ----------------------------------------------------
C
C Plot the specified line or all line
C
C Given:
C   pointer to TCL interpreter
      integer    interp(*)
C   flag to control frame plotting
C       =1  include frame
C       =-1 no frame
C       0=frame only
       logical    plot_frame
C   line to plot, or 0 == ALL lines
       integer    line
C
C Updated:
C   error status
       integer    s
C
C The specified line (or all lines) are plotted.  If a device has not
C been specified and the device is not open then the user will be prompted
C for an output device.
C-
       include '../include/plt_buffer_defn.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'
       include '../include/plt_graph_defn.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C local variables
       integer    n, nd, nn, n1, n2, m
C data arrays
       integer    id, ndata, max_ndata
       parameter (max_ndata = 4096)
       real*4     x(max_ndata), y(max_ndata),
     *            ex(max_ndata), ey(max_ndata),
     *            b1(max_ndata), b2(max_ndata)
       real*8     dx(max_ndata), dy(max_ndata)
       logical    graph_lines_init, do_auto
C character strings
       character*(iolen_file) file
       character              title*80, function*256
       character              xlist*40, ylist*40
       integer                l11, l12, l21, l22
C data scaling information held in common
       real*4                     graph_dtmax(4)
       common /graph_local_scale/ graph_dtmax, graph_lines_init

C check status on entry
       if (s.ne.0) return

C check line index
       if (line.le.0 .or. line.gt.graph_max_lines) then
         n1 = 1
         n2 = graph_max_lines
       else
         n1 = line
         n2 = line
       endif

C open graphics device / window
       call graphic_copy_graphic( graph, graphic, s )
       call graphic_open( graph, s )
       call graph_setup_coords( .false., s )
       if (graphic_status.eq.0 .or. plot_refresh) then
         graph_lines_init = .false.
         graph_frame_status = 0
       endif
       if (plot_frame.eq.0) then
         graph_frame_status = 0
       endif
       do_auto = graph_lines_init

C loop through the line and plot frame etc.
       do n=n1,n2
         call graphic_copy_grline(graph_grline(1,n),grline,s)
         if ( ((grline_status.gt.0.and. .not.plot_refresh).or.
     *         (grline_status.lt.0.and. plot_refresh)         ).and.
     *         (grline_x_file.ne.0 .or.
     *          grline_func_opt.eq.grline_func_defined)      ) then
C .. this line is defined, but not plotted
            if (grline_func_opt.eq.grline_func_defined) then
             call graphic_enq_text( graph_ntb, graph_itext, graph_text,
     *                             grline_func_defn, file, s )
             ndata = 101
             do m=1,ndata
               dx(m) = graph_coords(1) + 
     *                0.01*(graph_coords(2)-graph_coords(1))*(m-1)
               dy(m) = 0.0
             enddo
             function = file(1:chr_lenb(file))//char(0)
             call eval_eqn( function, ndata, dx, dy )
             do m=1,ndata
               x(m) = dx(m)
               y(m) = dy(m)
             enddo
            elseif (grline_list_opt.eq.grline_list_defined) then
             call graphic_enq_text( graph_ntb, graph_itext, graph_text,
     *                             grline_list_defn, file, s )
             l11 = chr_intlc(file)
             l12 = chr_lenw(file)
             l22 = chr_lenb(file)
             l21 = chr_intlc(file(l12+2:l22)) + l12 + 1
             xlist = file(l11:l12)//char(0)
             ylist = file(l21:l22)//char(0)
             if (l12.ge.l11) then
               call eval_list( interp, xlist, ndata, x, s)
             else
               ndata = 0
             endif
             if (l22.ge.l21) then
               call eval_list( interp, ylist, ndata, y, s)
             else
               ndata = 0
             endif
             if (grline_ex_opt.ne.0 .and. grline_ex_file.ne.0) then
              call graphic_enq_text( graph_ntb, graph_itext, graph_text,
     *                               grline_ex_file, file, s )
              l11 = chr_intlc(file)
              l12 = chr_lenw(file)
              xlist = file(l11:l12)//char(0)
              if (l12.ge.l11) then
                if (grline_ex_opt.eq.2) then
                  call eval_list( interp, xlist, nd, ex, s)
                else
                  call eval_list( interp, xlist, ndata, ex, s)
                endif
              else
                ndata = 0
              endif
             endif
             if (grline_ey_opt.ne.0 .and. grline_ey_file.ne.0) then
              call graphic_enq_text( graph_ntb, graph_itext, graph_text,
     *                               grline_ey_file, file, s )
              l11 = chr_intlc(file)
              l12 = chr_lenw(file)
              ylist = file(l11:l12)//char(0)
              if (l12.ge.l11) then
                if (grline_ey_opt.eq.2) then
                  call eval_list( interp, ylist, nd, ey, s)
                else
                  call eval_list( interp, ylist, ndata, ey, s)
                endif
              else
                ndata = 0
              endif
             endif
            else
             call graphic_enq_text( graph_ntb, graph_itext, graph_text,
     *                              grline_x_file, file, s )
             call spec_allocate( file(1:chr_lenb(file)),'read',id,s)
             call spec_get_data( id, grline_x_col, x, ndata, s )
             call spec_deallocate(id, s )
             if (grline_y_file.eq.0) then
              grline_y_file = grline_x_file
             endif
             call graphic_enq_text( graph_ntb, graph_itext, graph_text,
     *                             grline_y_file, file, s )
             call spec_allocate( file(1:chr_lenb(file)),'read',id,s)
             call spec_get_data( id, grline_y_col, y, ndata, s )
             call spec_deallocate(id, s )
             if (grline_ex_opt.ne.0 .and. grline_ex_file.ne.0) then
              call graphic_enq_text( graph_ntb, graph_itext, graph_text,
     *                               grline_ex_file, file, s )
              call spec_allocate(file(1:chr_lenb(file)),'read',id,s)
              if (grline_ex_opt.eq.1) then
                call spec_get_data(id,grline_ex_col,ex,ndata,s)
              else
                call spec_get_data(id,grline_ex_col,b1,ndata,s)
                call spec_get_data(id,grline_ex_col+1,b2,ndata,s)
                nn = 0
                do nd=1,ndata
                  nn = nn + 1
                  ex(nn) = b1(nd)
                  nn = nn + 1
                  ex(nn) = b2(nd)
                enddo
              endif
              call spec_deallocate(id, s )
             endif
             if (grline_ey_opt.ne.0 .and. grline_ey_file.ne.0) then
              call graphic_enq_text( graph_ntb,graph_itext,graph_text,
     *                               grline_ey_file, file, s )
              call spec_allocate(file(1:chr_lenb(file)),'read',id,s)
              if (grline_ey_opt.eq.1) then
                call spec_get_data(id,grline_ey_col,ey,ndata,s)
              else
                call spec_get_data(id,grline_ey_col,b1,ndata,s)
                call spec_get_data(id,grline_ey_col+1,b2,ndata,s)
                nn = 0
                do nd=1,ndata
                  nn = nn + 1
                  ey(nn) = b1(nd)
                  nn = nn + 1
                  ey(nn) = b2(nd)
                enddo
              endif
              call spec_deallocate(id, s )
             endif
            endif
C .. if this is the first line to be plotted then do scaling
            if (.not.graph_lines_init) then
               if (graph_xcoord_opt.eq.-1) then
                  graph_coords(1) = 0.0
                  graph_coords(2) = 0.0
               endif
               if (graph_ycoord_opt.eq.-1) then
                  graph_coords(3) = 0.0
                  graph_coords(4) = 0.0
               endif
               call graphic_grrange(grline,ndata,x,y,
     *                              graph_coords,s)
               if (s.eq.0) then
                 if (graph_xcoord_opt.eq.0) graph_xcoord_opt = -1
                 if (graph_ycoord_opt.eq.0) graph_ycoord_opt = -1
               endif
               call graph_setup_coords( .false., s )
               graph_lines_init = .true.
C ... plot frame
            endif
            if (graph_frame_status.eq.0 .and.
     *          plot_frame.ge.0         ) then
                 call graphic_frame( graph_frame, s )
                 graph_frame_status = 1
                 call graphic_set_text_opt(graph_title_opt,s)
                 if (graph_title.ne.0) then
                   call graphic_enq_text(
     *                  graph_ntb,graph_itext,graph_text,
     *                            graph_title, title, s )
                   if (graph_opt_t.eq.1) then
                     call pgmtext('T',graph_posn_t(1),graph_posn_t(2),
     *                                graph_posn_t(3), title)
                   elseif (graph_opt_t.eq.2) then
                     call pgmtext('B',graph_posn_t(1),graph_posn_t(2),
     *                                graph_posn_t(3), title)
                   endif
                 endif
                 if (graph_x_title.ne.0) then
                   call graphic_enq_text(
     *                  graph_ntb,graph_itext,graph_text,
     *                         graph_x_title, title, s )
                   if (graph_opt_xt.eq.1) then
                     call pgmtext('B',graph_posn_xt(1),graph_posn_xt(2),
     *                                graph_posn_xt(3), title)
                   elseif (graph_opt_xt.eq.2) then
                     call pgmtext('T',graph_posn_xt(1),graph_posn_xt(2),
     *                                graph_posn_xt(3), title)
                   endif
                 endif
                 if (graph_y_title.ne.0) then
                   call graphic_enq_text(
     *                  graph_ntb,graph_itext,graph_text,
     *                         graph_y_title, title, s )
                   if (graph_opt_yt.eq.1) then
                     call pgmtext('L',graph_posn_yt(1),graph_posn_yt(2),
     *                                graph_posn_yt(3), title)
                   elseif (graph_opt_yt.eq.2) then
                     call pgmtext('R',graph_posn_yt(1),graph_posn_yt(2),
     *                                graph_posn_yt(3), title)
                   elseif (graph_opt_yt.eq.3) then
                     call pgmtext('LV',graph_posn_yt(1),
     *                                 graph_posn_yt(2),
     *                                 graph_posn_yt(3), title)
                   elseif (graph_opt_yt.eq.4) then
                     call pgmtext('RV',graph_posn_yt(1),
     *                                 graph_posn_yt(2),
     *                                 graph_posn_yt(3), title)
                   endif
                 endif

                 if (plot_frame.eq.0) goto 10
            endif
C .. plot line
            call graphic_grline( grline, ndata, x, y, ex, ey,
     *                           do_auto, graph_dtmax, s )
            do_auto = .true.
         endif
         call graphic_copy_grline(grline,graph_grline(1,n),s)
       enddo
10     continue
       if ( (graph_key_status.eq.1 .and. .not.plot_refresh) .or.
     *      (graph_key_status.eq.-1 .and. plot_refresh) ) then
          call graph_key( s )
       endif

       call cmd_err(s,'graph_plot',' ')

       end
C
C
*+ graph_init

       subroutine graph_init( opt, s )
C      -------------------------------
C
C Initialise graph-specific options
C
C Given:
C   option for initialisation
       character*(*)     opt
C Updated:
C   error status
       integer           s
C
C-
       include '../include/plt_buffer_defn.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'
       include '../include/plt_graph_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer     n, icode

       if (s.ne.0) return

       if (chr_cmatch('all',opt).or.chr_cmatch('plot',opt)) then
         graph_frame_status = 0
         call graphic_copy_graphic(graph,graphic,s)
         graphic_status = 0
         call graphic_copy_graphic(graphic,graph,s)
         graph_key_status = abs(graph_key_status)
       endif

       if (chr_cmatch('all',opt).or.chr_cmatch('title',opt)) then
         call graphic_delete_text(graph_ntb,graph_itext,graph_text,
     *                            graph_x_title,s)
         call graphic_delete_text(graph_ntb,graph_itext,graph_text,
     *                            graph_y_title,s)
         call graphic_delete_text(graph_ntb,graph_itext,graph_text,
     *                            graph_title,s)
         graph_x_title = 0
         graph_y_title = 0
         graph_title = 0
       endif

       if (chr_cmatch('all',opt)) then
         do n=1,graph_ntb
           graph_itext(n) = 0
         enddo
         graph_view_port_opt = 1
         graph_view_port(1) = 0.15
         graph_view_port(2) = 0.85
         graph_view_port(3) = 0.15
         graph_view_port(4) = 0.85
       endif

       if (chr_cmatch('all',opt).or.chr_cmatch('setup',opt)) then
         graph_xcoord_opt = 0
         graph_ycoord_opt = 0
         graph_xrange(1) = 0.0
         graph_xrange(2) = 0.0
         graph_yrange(1) = 0.0
         graph_yrange(2) = 0.0
       endif

       icode = -1
       if (chr_cmatch('all',opt)) then
         icode = 3
       elseif (chr_cmatch('options',opt)) then
         icode = 1
       elseif (chr_cmatch('setup',opt)) then
         icode = 2
       elseif (chr_cmatch('plot',opt)) then
         icode = 0
       endif
       if (icode.ge.0) then
         do n=1,graph_max_lines
           call graphic_default_grline(graph_grline(1,n),icode,s)
         enddo
       endif
       if (chr_cmatch('all',opt).or.chr_cmatch('options',opt)) then
           call graphic_default_frame_opt(graph_frame,s)
           call graphic_default_text_opt(graph_title_opt,s)
           graph_opt_xt = 1
           graph_opt_yt = 1
           graph_opt_t = 1
           graph_posn_xt(1) = 2.0
           graph_posn_xt(2) = 0.5
           graph_posn_xt(3) = 0.5
           graph_posn_yt(1) = 2.2
           graph_posn_yt(2) = 0.5
           graph_posn_yt(3) = 0.5
           graph_posn_t(1) = 2.0
           graph_posn_t(2) = 0.5
           graph_posn_t(3) = 0.5
           graph_key_status = 0
           call graphic_default_text_opt(graph_key_text,s)
           graph_key_opts(1) = 0.0
           graph_key_opts(2) = 1.0
           graph_key_opts(3) = 0.0
           graph_key_opts(4) = 1.0
       endif

       call cmd_err(s,'graph_init',' ')

       end
C
C
*+ graph_key

       subroutine graph_key( s )
C      -------------------------
C
C Plot a key
C
C Updated:
C  error status
       integer        s
C
C A key is added to the plot using the currently defined options
C-
       include '../include/plt_buffer_defn.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'
       include '../include/plt_graph_defn.inc'
       include '/mrao/include/chrlib_functions.inc'


C local variables
C   current viewport
       real*4         u1, u2, v1, v2
C   position of start of key box
       real*4         xpos, ypos
C   drop factor between successive lines
       real*4         drop
C   scale factor for key box
       real*4         scale
C   key to specify drawing a frame around the key
       integer        frame

C   counters and pointers
       integer        n
       real*4         x1, y1, x2, y2, x3, d
       real*4         x, y, xmax
C   text buffer
       character*256  string, str1, str2

C check status on entry
       if (s.ne.0) return

C check status of key plot
       if ( graph_key_status.eq.0 .or.
     *     (graph_key_status.eq.-1 .and. .not.plot_refresh) .or.
     *     (graph_key_status.eq.1  .and. plot_refresh) ) then
          return
       endif

C find current view-port/window, then reset
       call pgqwin( 0, u1, u2, v1, v2 )
       call pgsetwin( 0.0,1.0,0.0,1.0 )
       call graphic_scale_coord( s )

C sort out options
       xpos = graph_key_opts(1)
       ypos = graph_key_opts(2)
       drop = graph_key_opts(4)
       frame = nint(graph_key_opts(3))
       call graphic_set_text_opt(graph_key_text,s)
       call pgqch( scale )
       d = drop*scale*1.5/40.0

C loop for each item in the list
       y1 = ypos - 0.5*d - 0.025*scale
       y2 = ypos - d - 0.025*scale
       x1 = xpos + 0.025*scale
       x2 = xpos + 0.15*scale
       x3 = xpos + 0.175*scale
       do n = 1,graph_max_lines
         call graphic_copy_grline(graph_grline(1,n),grline,s)
         if ( grline_status.lt.0 ) then

C .. check for key entry for this line
           if (grline_key_opt.gt.0) then
             string = ' '
             if (grline_key_opt.eq.1) then
C ... use file and column
              if (grline_func_opt.eq.grline_func_defined) then
               call graphic_enq_text( graph_ntb, graph_itext,
     *                                graph_text,
     *                                grline_func_defn, str1, s )
               string = str1(1:chr_lenb(str1))
              else
               call graphic_enq_text( graph_ntb, graph_itext,
     *                                graph_text,
     *                                grline_x_file, str1, s )
               if (grline_y_file.ne.grline_x_file) then
                 call graphic_enq_text( graph_ntb, graph_itext,
     *                                  graph_text,
     *                                  grline_y_file, str2, s )
               else
                 str2 = ' '
               endif
               write(string,'(A,'' x ='',I2,A,'' y = '',I2)')
     *               str1(1:chr_lenb(str1)),grline_x_col,
     *               str2(1:chr_lenb(str2)),grline_y_col
              endif
             else
C ... use supplied text
               call graphic_enq_text( graph_ntb, graph_itext,
     *                                graph_text,
     *                                grline_key_text, string, s )
             endif
             if (grline_type.gt.0) then
               call graphic_set_line_opt(grline_line_opt,s)
               call pgmove(x1,y1)
               call pgdraw(x2,y1)
             endif
             if (grline_symbol.gt.0) then
               call graphic_set_text_opt(grline_text_opt,s)
               call pgpoint(1,0.5*(x1+x2),y1,grline_symbol)
             endif
             call graphic_set_text_opt(graph_key_text,s)
             call pgtext(x3,y2,string(1:chr_lenb(string)))
             call pgqpos(x,y)
             if (n.eq.1) then
               xmax = x
             else
               xmax = max(xmax,x)
             endif
             y1 = y1 - d
             y2 = y2 - d
           endif
         endif
       enddo

C if required draw a frame around the key
       if (frame.eq.1) then
         xmax = xmax + 0.025*scale
         call pgsls(1)
         call pgsci(1)
         call pgmove(xpos,ypos)
         call pgdraw(xpos,y1)
         call pgdraw(xmax,y1)
         call pgdraw(xmax,ypos)
         call pgdraw(xpos,ypos)
       endif

C update status
       graph_key_status = -1

C reset window and attributes
       call graphic_pop_coord( s )
       call pgqwin( 0, u1, u2, v1, v2 )
       call cmd_err( s, 'graph_key', ' ' )

       end
C
C
*+_
       subroutine graph_get(s)
C      -----------------------
C
C Provide "get" options for graph display systems
C
C Updated:
C   error status
       integer    s
C-
       include '../include/plt_buffer_defn.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'
       include '../include/plt_graph_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       character*70 opts(10), opt, string*120, number*20
       integer      n, l

       if (s.ne.0) return

C define options
       opts(1)='cursor-input .. cursor input from device'
       opts(2)='coordinates ... current coordinates'
       opts(3)='line .......... get line information'
       opts(4)='frame ......... get frame information'
       opts(5)='key ........... get key information'
       opts(6)='max-lines ..... maximum no. of lines'
       opts(7)='view-port ..... return graph view port'
       opts(8)='range ......... return data ranges'

       call io_getopt( 'Get-option (?=list) : ','cursor-input',
     *                  opts, 8, opt, s )

       string = ' '
       if (chr_cmatch(opt,'cursor-input')) then
          call graphic_cursor( s )

       elseif (chr_cmatch(opt,'coordinates')) then
         call cmd_defparam(.false.,'%coordinates','real',4,s)
         do n=1,4
           number = ' '
           string = ' '
           write(string,'(''%coordinates{'',i1,''}'')') n
           call chr_chrtoc( graph_coords(n),number,l)
           call cmd_setlocal(string(1:chr_lenb(string)),
     *                       number(1:l), s )
         enddo

       elseif (chr_cmatch(opt,'view-port')) then
         call cmd_defparam(.false.,'%graph-vp','real',4,s)
         do n=1,4
           number = ' '
           string = ' '
           write(string,'(''%graph-vp{'',i1,''}'')') n
           call chr_chrtoc( graph_view_port(n),number,l)
           call cmd_setlocal(string(1:chr_lenb(string)),
     *                       number(1:l), s )
         enddo

       elseif (chr_cmatch(opt,'range')) then
         call cmd_defparam(.false.,'%x-range','real',2,s)
         call cmd_defparam(.false.,'%y-range','real',2,s)
         do n=1,2
           number = ' '
           string = ' '
           write(string,'(''%x-range{'',i1,''}'')') n
           call chr_chrtoc( graph_xrange(n),number,l)
           call cmd_setlocal(string(1:chr_lenb(string)),
     *                       number(1:l), s )
           write(string,'(''%y-range{'',i1,''}'')') n
           call chr_chrtoc( graph_yrange(n),number,l)
           call cmd_setlocal(string(1:chr_lenb(string)),
     *                       number(1:l), s )
         enddo

       elseif (chr_cmatch(opt,'max-lines')) then
         call cmd_setlocal('%max-lines',number(1:n),s)

       elseif (chr_cmatch(opt,'frame')) then
         call graphic_pars_frame(graph_frame,s)
         call graphic_pars_text_opt(graph_title_opt,s)
         call chr_chitoc(graph_frame_status,number,l)
         call cmd_setlocal('%graph-frame-status',number(1:l),s)

       elseif (chr_cmatch(opt,'key')) then
         call chr_chitoc(graph_key_status,number,l)
         call graphic_pars_text_opt(graph_key_text,s)
         call cmd_setlocal('%graph-key-status',number(1:l),s)
         call cmd_defparam(.false.,'%graph-key-opts','real',4,s)
         do n=1,4
           number = ' '
           string = ' '
           write(string,'(''%graph-key-opts{'',i1,''}'')') n
           call chr_chrtoc( graph_key_opts(n),number,l)
           call cmd_setlocal(string(1:chr_lenb(string)),
     *                       number(1:l), s )
         enddo

       elseif (chr_cmatch(opt,'line')) then
          call io_geti('Line : ','1',n,s)
          call graphic_pars_grline(graph_grline(1,n),s)

       endif

       call cmd_err(s,'graph_get',' ')
       end










