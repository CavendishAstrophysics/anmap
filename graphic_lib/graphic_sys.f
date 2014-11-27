*+ graphic_sys

       subroutine graphic_sys( interp, cdata, map_array, status )
C      ----------------------------------------------------------
C
C Sub-system to manage the graphic systems
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
C A sub-system to manage the display graphics of Anmap.
C All interaction with the graphics device itself is managed by this sub-system
C together with the initialisation, display and refreshing of all graphics
C elements.
C
C P. Alexander, MRAO, Cambridge
C Version 1.0. January 1993
C-

       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/cmd_lang_data.inc'

       include '../include/plt_basic_defn.inc'
       include '../include/plt_error_defn.inc'
       include '../include/plt_graphic_array.inc'

C Local variables
C ---------------
C  command line, length and initial check
       character*120      command_line
       integer            len_com, i_comm_done
       logical            exit_on_completion
C output unit and temporary error status
       integer            iout, istat
C  command line interpretation
       integer            number_commands, icom
       parameter         (number_commands = 24)
       character*80       liscom(number_commands)

C local counters and buffers
       integer            save_plot_segments, save_graphic_current,
     *                    save_graphic_current_type
       character*60       opts(10), opt, string, name
       integer            n, nn, i, buffer(len_struct), type, from, to,
     *                    list(max_graphic_structures), ls

C flag to indicate a new graphic definition
       logical            new_graphic

C file handling
       integer            iunit
       character          file*(iolen_file)
C functions
       integer            pgindev

C define commands
       data liscom(1) /
     * 'output-device ............ set the graphics device for output'
     *                /
       data liscom(2) /
     * 'open-device .............. open the current output device'
     *                /
       data liscom(3) /
     * 'close-device ............. close the current output device'
     *                /
       data liscom(4) /
     * 'clear-device ............. clear screen on the current device'
     *                /
       data liscom(5) /
     * 'next-page ................ request a new page for next plot'
     *                /
       data liscom(6) /
     * 'segment-screen ........... segment the screen'
     *                /
       data liscom(7) /
     * 'device-size .............. specify device size'
     *                /
       data liscom(8) /
     * 'device-select ............ select one of the defined devices'
     *                /
       data liscom(9) /
     * 'select-type .............. select graphic type and number'
     *                /
       data liscom(10)/
     * 'plot ..................... plot graphic (type and number)'
     *                /
       data liscom(11)/
     * 'refresh .................. refresh graphic (type and number)'
     *                /
       data liscom(12)/
     * 'initialise ............... initialise the current graphic'
     *                /
       data liscom(13)/
     * 'default-text-style ....... set default text style for graphic'
     *                /
       data liscom(14)/
     * 'default-line-style ....... set default line style for graphic'
     *                /
       data liscom(15)/
     * 'view-port ................ set view-port for graphic'
     *                /
       data liscom(16)/
     * 'transparent .............. set transparent mode for graphic'
     *                /
       data liscom(17)/
     * 'opaque ................... set opaque mode for graphic'
     *                /
       data liscom(18)/
     * 'depth .................... set depth of graphic in field'
     *                /
       data liscom(19)/
     * 'default-device ........... set the default device for graphic'
     *                /
       data liscom(20)/
     * 'list-graphics ............ list defined graphics'
     *                /
       data liscom(21)/
     * 'get ...................... get information into parameters'
     *                /
       data liscom(22)/
     * 'save ..................... save the specified graphic'
     *                /
       data liscom(23)/
     * 'recall ................... recall a saved graphic'
     *                /
       data liscom(24)/
     * 'copy-definition .......... copy definitions'
     *                /

C check status on entry
       if (status.ne.0) return
       call io_enqcli( command_line, len_com )
       exit_on_completion = len_com.ne.0
       i_comm_done = 0
       do i = 1,cmd_data_len
         cmd_data(i) = cdata(i)
       enddo

C check for selected device number being in accepted range
       if (plot_list_selected.le.0 .or.
     *     plot_list_selected.gt.plot_list_max) then
         plot_list_selected = 1
         plot_list_default = 1
       endif

C perform commands
100    continue
       if (exit_on_completion .and. i_comm_done.gt.0) then
           do i = 1,cmd_data_len
             cdata(i) = cmd_data(i)
           enddo
           cdata(1) = 100
           return
       end if
       status = 0
       call cmd_getcmd( 'Graphic> ',liscom,number_commands,
     *                  icom,status)

       if (status.ne.0) then
         call cmd_err(status,'Graphic',' ')
         goto 100
       else
         i_comm_done = i_comm_done + 1
       end if

       if (icom.le.0) then
         status = 0
          do i = 1,cmd_data_len
            cdata(i) = cmd_data(i)
          enddo
         cdata(1) = icom
         return
       end if

C decode command
       if (chr_cmatch('output-device',liscom(icom))) then
         call io_enqplt(0,plot_device)
         call io_getplt('Output-device (?=list) : ',
     *                  '*',plot_device,status)
         if (status.eq.0 .and. plot_open) then
           call pgclose(plot_list_id(plot_list_selected))
           plot_open = .false.
         end if
         plot_size = 0.0
         plot_aspect = 0.75
         call cmd_err(status,'OUTPUT-DEVICE',' ')

       else if (chr_cmatch('open-device',liscom(icom))) then
         if (.not.plot_open) then
           istat = pgindev( iout, plot_device,
     *                     plot_segments, plot_segments,
     *                     plot_list_id(plot_list_selected))
           call pgask(.false.)
           if (istat.ne.1) then
             status = ill_device
           else
             plot_open = .true.
             if (plot_aspect.lt.0.01) then
               plot_aspect = 0.75
             endif
             call pgpap(plot_size,plot_aspect)
             call pgadvance
             call pgpap(plot_size,plot_aspect)
           endif
         elseif (plot_next) then
           call pgpap(plot_size,plot_aspect)
           call pgadvance
           call pgpap(plot_size,plot_aspect)
           plot_next = .false.
         endif

       else if (chr_cmatch('close-device',liscom(icom))) then
         call pgclose(plot_list_id(plot_list_selected))
         plot_open = .false.

       else if (chr_cmatch('next-page',liscom(icom))) then
         plot_next = .true.

       else if (chr_cmatch('device-size',liscom(icom))) then
         call io_getr('Plot-size (cm) (0.0=default-size) : ','0.0',
     *                 plot_size, status )
         plot_size = plot_size / 2.5
         if (plot_size.lt.0.0) then
           plot_size = 0.0
         endif
         call io_getr('Aspect-ratio (height/width) : ','0.7',
     *                 plot_aspect,status)
         if (plot_aspect.lt.0.1) then
           plot_aspect = 0.75
         endif
*         if (status.eq.0 .and. plot_open) then
*           if (plot_aspect.lt.0.01) then
*             plot_aspect = 0.75
*           endif
*           call pgadvance
*           call pgpap(plot_size,plot_aspect)
*           call pgadvance
*         endif

       else if (chr_cmatch('device-select',liscom(icom))) then
         n = plot_list_selected
         call io_geti('Device-number : ','*',n,status)
         if (n.ge.1 .and. n.le.plot_list_max) then
           call graphic_device_select(n,status)
           plot_list_default = plot_list_selected
         else
           call io_wrout('*** Illegal device number: out of range')
         endif

       else if (chr_cmatch('clear-device',liscom(icom))) then
         call pgsetvp(0.0,1.0,0.0,1.0)
         call pgsetwin(0.0,1.0,0.0,1.0)
         call pgsfs( 1 )
         call pgsci( colour_background )
         call pgrect( 0.0,1.0,0.0,1.0 )
         call pgsfs( 2 )
         call pgsci( colour_foreground )

       else if (chr_cmatch('segment-screen',liscom(icom))) then
         if (plot_open) then
           call cmd_wrerr('SEGMENT-SCREEN','Plot currently active')
         else
           save_plot_segments = plot_segments
           call io_geti('Horizontal Segments : ','*',
     *                  plot_segments,status)
           if (plot_segments.gt.3) then
             call cmd_wrerr('SEGMENT-SCREEN','Too many segments >3')
             plot_segments = save_plot_segments
           end if
         endif
         call cmd_err(status,'SEGMENT-SCREEN',' ')

       else if (chr_cmatch('select-type',liscom(icom))) then
         new_graphic = .false.
         save_graphic_current_type = graphic_current_type
         save_graphic_current = graphic_current
         opts(graphic_image)   = 'image'
         opts(graphic_draw)    = 'drawing'
         opts(graphic_graph)   = 'graph'
         opts(graphic_scratch) = 'scratch'
         call io_getopt( 'Graphic-type (?=list) : ','image',
     *        opts,graphic_max,opt,status)
         if (status.eq.0) then
           do n=1,graphic_max
             if (chr_cmatch(opt,opts(n))) then
               graphic_current_type = n
             endif
           enddo
         endif
         call io_geti( 'Graphic-index (>=1) : ','1',nn,status)
         if (nn.le.0) then
           nn = 1
           call cmd_wrerr('SELECT','Index must be > 0 -- reset to 1')
         endif
         call graphic_get_index(graphic_current_type,nn,
     *                          graphic_current,new_graphic,status)

         if (graphic_current.eq.-1) then
           graphic_current = save_graphic_current
           graphic_current_type = save_graphic_current_type
           call graphic_copy_graphic( graphic_array(1,graphic_current),
     *                                graphic,
     *                                status   )
           call cmd_wrerr('SELECT','Unable to find/allocate selection')
         elseif (new_graphic) then
           graphic_index = nn
           graphic_type = graphic_current_type
           graphic_view_port(1) = 0.0
           graphic_view_port(2) = 1.0
           graphic_view_port(3) = 0.0
           graphic_view_port(4) = 1.0
           graphic_device = 0
           call graphic_copy_graphic( graphic,
     *                                graphic_array(1,graphic_current),
     *                                status   )
*           call cmd_exec(graphic_command_init(graphic_current_type),
*     *                   status)
           cmd_results = graphic_command_init(graphic_current_type)
           graphic_default_current(graphic_current_type)=graphic_current
         endif
         graphic_default_current(graphic_current_type)=graphic_current

       else if (chr_cmatch('plot',liscom(icom))) then
         call graphic_check_current(graphic,status)
         call cmd_exec(
     *        graphic_command_plot(graphic_current_type), status)
         cmd_results = graphic_command_plot(graphic_current_type)

       else if (chr_cmatch('refresh',liscom(icom))) then
         call graphic_check_current(graphic,status)
         call cmd_exec(
     *        graphic_command_refresh(graphic_current_type), status)
         cmd_results = graphic_command_refresh(graphic_current_type)

       else if (chr_cmatch('initialise',liscom(icom))) then
         call graphic_check_current(graphic,status)
         call cmd_exec(
     *        graphic_command_init(graphic_current_type), status)
         cmd_results = graphic_command_init(graphic_current_type)

       else if (chr_cmatch('default-text-style',liscom(icom))) then
         call graphic_check_current(graphic,status)
         call graphic_get_text_opt(graphic_text_opts, status)
         call graphic_copy_graphic( graphic,
     *                              graphic_array(1,graphic_current),
     *                              status )
         call cmd_err(status,'default-text-style',' ')

       else if (chr_cmatch('default-line-style',liscom(icom))) then
         call graphic_check_current(graphic,status)
         call graphic_get_line_opt(graphic_line_opts, status)
         call graphic_copy_graphic( graphic,
     *                              graphic_array(1,graphic_current),
     *                              status )
         call cmd_err(status,'default-line-style',' ')

       else if (chr_cmatch('view-port',liscom(icom))) then
         call graphic_check_current(graphic,status)
         call io_getnr(
     *          'View-port (Normalised-coordinates 0->1, 0->1) : ',
     *          '0,1,0,1',graphic_view_port,4,status)
         call graphic_copy_graphic( graphic,
     *                              graphic_array(1,graphic_current),
     *                              status )
         call cmd_err(status,'view-port','View-port unchanged')

       else if (chr_cmatch('transparent',liscom(icom))) then
         call graphic_check_current(graphic,status)
         graphic_transparent = .true.
         call graphic_copy_graphic( graphic,
     *                              graphic_array(1,graphic_current),
     *                              status )
         call cmd_err(status,'transparent',' ')

       else if (chr_cmatch('opaque',liscom(icom))) then
         call graphic_check_current(graphic,status)
         graphic_transparent = .false.
         call graphic_copy_graphic( graphic,
     *                              graphic_array(1,graphic_current),
     *                              status )
         call cmd_err(status,'opaque',' ')

       elseif (chr_cmatch('depth',liscom(icom))) then
         call graphic_check_current(graphic,status)
         call io_getnr('Depth-in-plot (0=near 5=far) : ','0',
     *                  graphic_depth, status )
         if (graphic_depth.lt.0) then
           graphic_depth = 0
         elseif (graphic_depth.gt.5) then
           graphic_depth = 5
         endif
         call graphic_copy_graphic( graphic,
     *                              graphic_array(1,graphic_current),
     *                              status )
         call cmd_err(status,'depth',' ')

       else if (chr_cmatch('default-device',liscom(icom))) then
         call io_geti('Default-device-number (0=use default) : ','0',
     *                n,status)
         if (status.eq.0) then
           if (n.ge.0 .and. n.le.plot_list_max) then
             graphic_device = n
           endif
         endif
         call graphic_copy_graphic( graphic,
     *                              graphic_array(1,graphic_current),
     *                              status )
         call cmd_err(status,'default-device',' ')

       elseif (chr_cmatch('list-graphics',liscom(icom))) then
         call io_enqout(iout)
         do n=1,max_graphic_structures
           call graphic_copy_graphic( graphic_array(1,n),
     *                                graphic, status   )
           if (graphic_index.ne.0) then
             if (graphic_transparent) then
               write(iout,10) graphic_name(graphic_type),
     *                        graphic_index, graphic_status,
     *                        'transparent', graphic_depth,
     *                        graphic_view_port
             else
               write(iout,10) graphic_name(graphic_type),
     *                        graphic_index, graphic_status,
     *                        'opaque', graphic_depth,
     *                        graphic_view_port
             endif
 10          format(' GRAPHIC: Type=',A,' Index=',I2,' Status=',I1/
     *              '          Transparency=',A,' Depth=',I3,4F6.2)
           endif
         enddo


       elseif (chr_cmatch('get',liscom(icom))) then
         call graphic_copy_graphic( graphic_array(1,graphic_current),
     *                              graphic, status )
         opts(1) = 'cursor-position ... cursor position on graphic'
         opts(2) = 'current-index ..... index of the current graphic'
         opts(3) = 'current-graphic ... number of the current graphic'
         opts(4) = 'current-type ...... type of the current graphic'
         opts(5) = 'maximum-graphics .. maximum number of graphics'
         opts(6) = 'graphic ........... graphic information'
         opts(7) = 'active-list ....... list of active graphics'
         opts(8) = 'current-device .... number of current-device'

         call io_getopt( 'Get-option (?=list) : ','cursor-position',
     *                   opts,8,opt,status)

         if (chr_cmatch('cursor-position',opt)) then
           call pgsetvp(0.0,1.0,0.0,1.0)
           call pgsetwin(0.0,1.0,0.0,1.0)
           call graphic_cursor(status)
         elseif (chr_cmatch('current-index',opt)) then
           call graphic_check_current(graphic,status)
           call chr_chitoc(graphic_index,string,n)
           call cmd_setlocal('%current-index',string(1:n),status)
         elseif (chr_cmatch('current-graphic',opt)) then
           call graphic_check_current(graphic,status)
           call chr_chitoc(graphic_current,string,n)
           call cmd_setlocal('%current-graphic',string(1:n),status)
         elseif (chr_cmatch('current-type',opt)) then
           call graphic_check_current(graphic,status)
           call cmd_setlocal('%current-type',
     *                       graphic_name(graphic_current_type),status)
         elseif (chr_cmatch('maximum-graphics',opt)) then
           call chr_chitoc(max_graphic_structures,string,n)
           call cmd_setlocal('%maximum-graphics',string(1:n),status)
         elseif (chr_cmatch('graphic',opt)) then
           n = graphic_current
           call io_geti('Graphic number : ','*',n,status)
           if (n.le.0 .or. n.gt.max_graphic_structures) then
             n = graphic_current
           endif
           call graphic_copy_graphic( graphic_array(1,n),
     *                                graphic, status   )
           call graphic_pars_line_opt(graphic_line_opts,status)
           call graphic_pars_text_opt(graphic_text_opts,status)
           call graphic_pars_graphic(graphic,status)
           call graphic_copy_graphic(graphic_array(1,graphic_current),
     *                               graphic, status )
         elseif (chr_cmatch('active-list',opt)) then
           nn = 0
           do i=5,0,-1
             do n=1,max_graphic_structures
               call graphic_copy_graphic( graphic_array(1,n),
     *                                    graphic, status   )
               if (graphic_status.ne.0) then
                 if (graphic_depth.eq.i) then
                   nn = nn + 1
                   list(nn) = n
                 endif
               endif
             enddo
           enddo
           call cmd_defparam(.false.,'%graphic-list',
     *                       'integer',nn,status)
           name = ' '
           string = ' '
           do n=1,nn
             write(name,'(''%graphic-list{'',i2,''}'')') n
             call chr_chitoc( list(n), string, ls )
             call cmd_setlocal(name,string(1:ls), status )
           enddo
           call chr_chitoc( nn, string, ls )
           call cmd_setlocal('%graphic-len-list',string(1:ls), status )
           call graphic_copy_graphic(graphic_array(1,graphic_current),
     *                               graphic, status )

         elseif (chr_cmatch('current-device',opt)) then
           call chr_chitoc(plot_list_selected,string,n)
           call cmd_setlocal('%current-device',string(1:n),status)
         endif

       elseif (chr_cmatch('save',liscom(icom))) then
         call graphic_check_current(graphic,status)
         if (status.eq.0) then
           call io_getfil('Graphic-file : ','graphic.obj',
     *                    file,status)
           call io_operan(iunit,file,'WRITE',len_struct*4,0,status)
           call io_wrfile(iunit,1,graphic_array(1,graphic_current),
     *                    len_struct,status)
           close (iunit)
         endif

       elseif (chr_cmatch('recall',liscom(icom))) then
         save_graphic_current_type = graphic_current_type
         save_graphic_current = graphic_current
         call io_getfil('Graphic-file : ','graphic.obj',
     *                  file,status)
         call io_operan(iunit,file,'READ',len_struct*4,0,status)
         call io_rdfile(iunit,1,buffer,len_struct,status)
         close (iunit)
         call graphic_copy_graphic( buffer, graphic, status )
         call graphic_get_index(graphic_type,graphic_index,
     *                          graphic_current,new_graphic,status)
         if (graphic_current.ne.-1) then
           do n=1,len_struct
             graphic_array(n,graphic_current) = buffer(n)
           enddo
           graphic_current_type = graphic_type
           graphic_default_current(graphic_current_type)=graphic_current
         else
           call cmd_wrerr('GRAPHIC','No spare graphic definitions')
           graphic_current = save_graphic_current
           graphic_current_type = save_graphic_current_type
         endif

       elseif (chr_cmatch('copy',liscom(icom))) then
         new_graphic = .false.
         opts(graphic_image)   = 'image'
         opts(graphic_draw)    = 'drawing'
         opts(graphic_graph)   = 'graph'
         opts(graphic_scratch) = 'scratch'
         call io_getopt( 'From-type (?=list) : ','image',
     *        opts,graphic_max,opt,status)
         if (status.eq.0) then
           do n=1,graphic_max
             if (chr_cmatch(opt,opts(n))) then
               type = n
             endif
           enddo
         endif
         call io_geti( 'From-index (>=1) : ','1',nn,status)
         if (nn.le.0) then
           nn = 1
           call cmd_wrerr('COPY','Index must be > 0 -- reset to 1')
         endif
         call graphic_get_index(type,nn,from,new_graphic,status)
         if (from.gt.0) then
           call io_getopt( 'To-type (?=list) : ','image',
     *          opts,graphic_max,opt,status)
           if (status.eq.0) then
             do n=1,graphic_max
               if (chr_cmatch(opt,opts(n))) then
                 type = n
               endif
             enddo
           endif
           call io_geti( 'To-index (>=1) : ','1',nn,status)
           if (nn.le.0) then
             nn = 1
             call cmd_wrerr('COPY','Index must be > 0 -- reset to 1')
           endif
         else
           call cmd_wrerr('COPY','From struture does not exist')
         endif
         call graphic_get_index(type,nn,to,new_graphic,status)
         if (from.gt.0 .and. to.gt.0) then
           do n=1,len_struct
             graphic_array(n,to) = graphic_array(n,from)
           enddo
         endif
       end if
       goto 100
       end
C
C
*+ graphic_check_current

       subroutine graphic_check_current( gr, s )
C      -----------------------------------------
C
C check that a current graphic has been defined and initialise gr
C
C Returned:
C  graphic structure initialised to the current graphic
       integer     gr(*)
C
C Updated:
C   error status
       integer     s
C
C
       include '../include/plt_basic_defn.inc'
       include '../include/plt_graphic_array.inc'
       include '../include/plt_error_defn.inc'

       if (s.ne.0) return
       if (graphic_current.eq.0) then
         call cmd_wrerr('GRAPHIC','Nor current graphic -- Use select')
         s = no_grcurr
       else
         call graphic_copy_graphic( graphic_array(1,graphic_current),
     *                              gr, s )
       endif
       end
C
C
*+ graphic_get_index

       subroutine graphic_get_index( type, index, nc, new, s )
C      -------------------------------------------------------
C
C Find the index to a graphic specified by type and index
C
C Given:
C   type of structure
       integer     type
C   index of this type
       integer     index
C
C Returned:
C   number of selection ( -1 if none could be found)
       integer     nc
C   flag indicating if this is a new structure
       logical     new
C
C Updated:
C   error status
       integer     s
C
C
       include '../include/plt_basic_defn.inc'
       include '../include/plt_graphic_array.inc'
       include '../include/plt_error_defn.inc'

C local variables
       integer     n

       if (s.ne.0) return

       new = .false.
       nc = -1
       do n=1,max_graphic_structures
         if (nc.eq.-1) then
           call graphic_copy_graphic( graphic_array(1,n),
     *                                graphic, s  )
           if ( graphic_type.eq.type ) then
              if (graphic_index.eq.index) nc = n
           endif
         endif
       enddo

       if (nc.eq.-1) then
         do n=1,max_graphic_structures
           if (nc.eq.-1) then
             call graphic_copy_graphic( graphic_array(1,n),
     *                                  graphic, s  )
             if (graphic_index.eq.0) then
                   new = .true.
                   nc = n
             endif
           endif
         enddo
       endif
       call cmd_err(s,'graphic_get_index',' ')

       end











