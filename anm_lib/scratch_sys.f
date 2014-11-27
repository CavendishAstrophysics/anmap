*+ scratch_sys

       subroutine scratch_sys( interp, cdata, map_array, status )
C      ----------------------------------------------------------
C
C Sub-system to manipulate scratch graphic objects
C
C Updated:
C    command interpreter data structure
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
C scratch data produced in various routines within Anmap.
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
       include '../include/plt_scratch_defn.inc'

C Local variables
C ---------------
C  command line, length and initial check
       character*120      command_line
       integer            len_com, i_comm_done
       logical            exit_on_completion
C  command line interpretation
       integer            number_commands, icom
       parameter         (number_commands = 11)
       character*80       liscom(number_commands),
     *                    opt, opts(6)

C define commands
       data liscom(1) /
     * 'set-frame-style .......... set style options for frame'
     *                /
       data liscom(2) /
     * 'set-text-style ........... set style for text'
     *                /
       data liscom(3) /
     * 'set-line-style ........... set style for lines'
     *                /
       data liscom(4) /
     * 'title .................... specify title for the plot'
     *                /
       data liscom(5) /
     * 'x-title .................. specify x-title for the plot'
     *                /
       data liscom(6) /
     * 'y-title .................. specify y-title for the plot'
     *                /
       data liscom(7) /
     * 'view-port ................ (sub) view-port for scratch graphic'
     *                /
       data liscom(8) /
     * 'plot ..................... plot previous scratch graphic'
     *                /
       data liscom(9) /
     * 'initialise ............... initialise options for scratch'
     *                /
       data liscom(10)/
     * 'annotate ................. annotate the scratch graphic'
     *                /
       data liscom(11)/
     * 'get ...................... get information for the graphic'
     *                /


C counters and pointers
       integer    n, l
       character  string*80, number*20

C check status on entry
       if (status.ne.0) return
       call io_enqcli( command_line, len_com )
       exit_on_completion = len_com.ne.0
       i_comm_done = 0

C initialise the text drawing graphic
       call graphic_init(graphic_scratch, scratch_defn, status)
       call graphic_copy_graphic(scratch_defn, graphic, status)
       annot_data(1) = len_annot_data

C command loop-back
100    continue
       if (exit_on_completion .and. i_comm_done.gt.0) then
           cdata(1) = 100
           call graphic_end(graphic_scratch, scratch_defn, status)
           return
       end if
       status = 0
       call cmd_getcmd( 'Scratch> ',liscom,number_commands,
     *                  icom,status)

       if (status.ne.0) then
         call cmd_err(status,'Scratch',' ')
         goto 100
       else
         i_comm_done = i_comm_done + 1
       end if

       if (icom.le.0) then
         status = 0
         cdata(1) = icom
         call graphic_end(graphic_scratch, scratch_defn, status)
         return
       end if

C decode command
       if (chr_cmatch('set-frame-style',liscom(icom))) then
         call graphic_get_frame_opt( scratch_frame_opt, status)

       elseif (chr_cmatch('set-text-style',liscom(icom))) then
         call graphic_get_text_opt( scratch_text_opt, status)
         call io_geti('Symbol-type : ','*',scratch_symbol,status)

       elseif (chr_cmatch('set-line-style',liscom(icom))) then
         call graphic_get_line_opt( scratch_line_opt, status)

       elseif (chr_cmatch('title',liscom(icom))) then
         call io_getstr('Title : ',' ',main_title,status)

       elseif (chr_cmatch('x-title',liscom(icom))) then
         call io_getstr('X-title : ',' ',x_title,status)

       elseif (chr_cmatch('y-title',liscom(icom))) then
         call io_getstr('Y-title : ',' ',y_title,status)

       elseif (chr_cmatch('view-port',liscom(icom))) then
         call io_getnr( 'View-port : ','*',
     *                  scratch_view_port,4,status)
         if (scratch_view_port(1).lt.0.0 .or.
     *       scratch_view_port(1).gt.1.0) then
           scratch_view_port(1) = 0.1
         endif
         if (scratch_view_port(2).lt.0.0 .or.
     *       scratch_view_port(2).gt.1.0) then
           scratch_view_port(2) = 0.9
         endif
         if (scratch_view_port(3).lt.0.0 .or.
     *       scratch_view_port(3).gt.1.0) then
           scratch_view_port(3) = 0.1
         endif
         if (scratch_view_port(4).lt.0.0 .or.
     *       scratch_view_port(4).gt.1.0) then
           scratch_view_port(4) = 0.1
         endif
         scratch_view_port_opt = 1

       elseif (chr_cmatch('plot',liscom(icom))) then
         opts(1)='all ........... plot everyting'
         opts(2)='annotations ... plot all annotations'
         opts(3)='refresh ....... refresh the whole graph'
         call io_getopt( 'Plot-option (?=list) : ','all',
     *                   opts, 3, opt, status )
         if (chr_cmatch(opt,'all').or.chr_cmatch(opt,'refresh')) then
           plot_refresh = .true.
           call cmd_exec(scratch_command,status)
           call annotate_plot( annot_data, 'all', status )
           plot_refresh = .false.
         elseif (chr_cmatch(opt,'annotations')) then
           call scratch_setup_coords( status )
           call annotate_plot( annot_data, 'all', status )
         endif

       elseif (chr_cmatch('initialise',liscom(icom))) then
         opts(1)='all ........... initialise everything'
         opts(2)='options ....... initialise options'
         opts(3)='annotations ... initialise annotation'
         opts(4)='titles ........ initialise titles'
         call io_getopt( 'Initialise-option (?=list) : ','all',
     *                   opts, 4, opt, status )
         if (.not.chr_cmatch('annotations',opt)) then
           call scratch_init( opt, status )
           call annotate_init( annot_data, opt, status )
         else
           call annotate_init( annot_data, 'all', status )
         endif

       elseif (chr_cmatch('get',liscom(icom))) then
         call scratch_setup_coords( status )
         opts(1)='cursor-input .. cursor input from device'
         opts(2)='coordinates ... current coordinates'
         opts(3)='command ....... command associated with object'
         opts(4)='scratch ....... return information for definition'
         call io_getopt( 'Get-option (?=list) : ','cursor-input',
     *                   opts, 4, opt, status )

         string = ' '
         if (chr_cmatch(opt,'cursor-input')) then
           call graphic_cursor( status )
         elseif (chr_cmatch(opt,'coordinates')) then
           call cmd_defparam(.false.,'%coordinates','real',4,status)
           do n=1,4
             number = ' '
             string = ' '
             write(string,'(''%coordinates{'',i1,''}'')') n
             call chr_chrtoc( scratch_coords(n),number,l)
             call cmd_setlocal(string(1:chr_lenb(string)),
     *                         number(1:l), status )
           enddo
         elseif (chr_cmatch(opt,'command')) then
           call cmd_setlocal('%command',scratch_command,status)
         elseif (chr_cmatch(opt,'scratch')) then
           call graphic_pars_line_opt(scratch_line_opt,status)
           call graphic_pars_text_opt(scratch_text_opt,status)
           call graphic_pars_frame(scratch_frame_opt,status)
           call chr_chitoc(scratch_symbol,number,l)
           call cmd_setlocal('%scratch-symbol',number,status)
           call cmd_defparam(.false.,'%scratch-vp','real',4,status)
           do n=1,4
             number = ' '
             string = ' '
             write(string,'(''%scratch-vp{'',i1,''}'')') n
             call chr_chrtoc( scratch_view_port(n),number,l)
             call cmd_setlocal(string(1:chr_lenb(string)),
     *                         number(1:l), status )
           enddo
         endif

       elseif (chr_cmatch('annotate',liscom(icom))) then
         call scratch_setup_coords( status )
         call annotate( annot_data, status )

       end if
       goto 100

       end

C
C
*+ scratch_setup_coords

       subroutine scratch_setup_coords( status )
C      -----------------------------------------
C
C Establish the scratch coordinate system on the current device
C
C Updated:
C   error status
       integer    status
C
C-
       include '../include/plt_buffer_defn.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_scratch_defn.inc'

C local variables
       real*4   xr, yr, x1, x2, y1, y2

       if (status.ne.0) return

C initialise view-port
       call graphic_copy_graphic(scratch_defn,graphic,status)
       if (scratch_view_port_opt.eq.1) then
         xr = (graphic_view_port(2)-graphic_view_port(1))
         yr = (graphic_view_port(4)-graphic_view_port(3))
         x1 = graphic_view_port(1) + scratch_view_port(1)*xr
         x2 = graphic_view_port(1) + scratch_view_port(2)*xr
         y1 = graphic_view_port(3) + scratch_view_port(3)*yr
         y2 = graphic_view_port(3) + scratch_view_port(4)*yr
         call pgsetvp( x1,x2,y1,y2 )
       else
         call pgsetvp( graphic_view_port(1), graphic_view_port(2),
     *                 graphic_view_port(3), graphic_view_port(4) )
       endif

C setup coordinates on the view-port
       if (scratch_coord_opt.ne.0) then
           call pgsetwin(scratch_coords(1),scratch_coords(2),
     *                   scratch_coords(3),scratch_coords(4))
       else
           call pgsetwin(0.0,1.0,0.0,1.0)
       endif

       call cmd_err(status,'scratch_setup_coords',' ')
       end
C
C
*+ scratch_init

       subroutine scratch_init( opt, s )
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
       include '../include/plt_scratch_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (s.ne.0) return

       if (chr_cmatch('all',opt).or.chr_cmatch('titles',opt)) then
         x_title = ' '
         y_title = ' '
         main_title = ' '
       endif

       if (chr_cmatch('all',opt)) then
         scratch_command = ' '
         scratch_coord_opt = 0
         scratch_view_port_opt = 0
         scratch_view_port(1) = 0.15
         scratch_view_port(2) = 0.85
         scratch_view_port(3) = 0.15
         scratch_view_port(4) = 0.85
       endif

       if (chr_cmatch('all',opt).or.chr_cmatch('options',opt)) then
           call graphic_default_frame_opt(scratch_frame_opt,s)
           call graphic_default_text_opt(scratch_text_opt,s)
           call graphic_default_line_opt(scratch_line_opt,s)
           scratch_symbol = 1
       endif

       call cmd_err(s,'scratch_init',' ')

       end

