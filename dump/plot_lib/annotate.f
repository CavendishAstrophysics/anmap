*+ annotate

       subroutine annotate( annot_data, status )
C      -----------------------------------------
C
C Sub-system to implement a graphic-based annotation system
C
C Updated:
C    control data for the annotations
       integer       annot_data(*)
C Returned:
C    error status
       integer       status
C
C The annotation system uses the object-based graphic-model for Anmap graphics
C to produce a variety of simple graphics drawing structures.  The main use
C of this sub-system is to annotate the presentation graphics within Anmap.
C
C P. Alexander, MRAO, Cambridge
C Version 1.0. February 1993
C-

C include standard include definitions
       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C sub-system specific includes
       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'

C Local variables
C ---------------
C  command line, length and initial check
       character*120      command_line
       integer            len_com, i_comm_done
       logical            exit_on_completion
C  command line interpretation
       integer            number_commands, icom
       parameter         (number_commands = 22)
       character*80       liscom(number_commands),
     *                    opt, opts(5)

C define commands
       data liscom(1) /
     * 'line-draw ................ plot a straigt line'
     *                /
       data liscom(2) /
     * 'arc-draw ................. plot a curved line'
     *                /
       data liscom(3) /
     * 'circle-draw .............. plot a circle'
     *                /
       data liscom(4) /
     * 'ellipse-draw ............. plot an ellipse'
     *                /
       data liscom(5) /
     * 'box-draw ................. plot a box'
     *                /
       data liscom(6) /
     * 'erase-draw ............... erase a specified region'
     *                /
       data liscom(7) /
     * 'cross-draw ............... plot a cross'
     *                /
       data liscom(8) /
     * 'arrow-draw ............... plot an arrow'
     *                /
       data liscom(9) /
     * 'text-draw ................ plot a text label'
     *                /
       data liscom(10)/
     * 'set-text-style ........... set the style for subsequent text'
     *                /
       data liscom(11)/
     * 'set-line-style ........... set the style for subsequent lines'
     *                /
       data liscom(12)/
     * 'set-fill-style ........... set fill-mode option for objects'
     *                /
       data liscom(13)/
     * 'set-arrow-style .......... set style for arrows'
     *                /
       data liscom(14)/
     * 'set-plot-options ......... set options for plotting mode'
     *                /
       data liscom(15)/
     * 'list-objects ............. list all objects'
     *                /
       data liscom(16)/
     * 'delete-object ............ delete an existing object'
     *                /
       data liscom(17)/
     * 'plot ..................... plot unplotted, defined objects'
     *                /
       data liscom(18)/
     * 'initialise ............... initialise the plot and options'
     *                /
       data liscom(19)/
     * 'get ...................... return information for drawing'
     *                /
       data liscom(20)/
     * 'update-object ............ update options for listed object'
     *                /
       data liscom(21)/
     * 'help ..................... help on adding annotations'
     *                /
       data liscom(22)/
     * 'compound-object .......... define a compound object'
     *                /


C counters and pointers
       integer    obj, itext, n, ls
       character  string*80

C check status on entry
       if (status.ne.0) return
       call io_enqcli( command_line, len_com )
       exit_on_completion = len_com.ne.0
       i_comm_done = 0
       call annot_start( annot_data, status )

C command loop-back
100    continue
       if (exit_on_completion .and. i_comm_done.gt.0) then
           call annot_end( annot_data, status)
           return
       end if
       status = 0
       call cmd_scncmd( .false., status )
       call cmd_getcmd( 'Annotate> ',liscom,number_commands,
     *                  icom,status)
       call cmd_scncmd( .true., status )

       if (status.ne.0) then
         call cmd_err(status,'Annotate',' ')
         goto 100
       else
         i_comm_done = i_comm_done + 1
       end if

       if (icom.le.0) then
         status = 0
         call annot_end( annot_data, status)
         return
       end if

C decode command
       if (chr_cmatch('line-draw',liscom(icom))) then
         call annot_find_object(obj,status)
         object_status = 1
         call annot_getdim(object_line,object_dim,status)
         object_type = object_line
         call graphic_copy_line_opt(annot_line_opts,
     *                                object_opt,status)
         call graphic_copy_object(object,annot_object(1,obj),status)
         if (annot_immediate.ge.1) then
           call annot_plot(obj,status)
           if (annot_immediate.eq.2) then
             annot_object(1,obj) = 0
             call graphic_copy_object(annot_object(1,obj),object,status)
             call annot_free_text(object_lable,status)
           endif
         endif
         call cmd_err(status,'line',' ')

       elseif (chr_cmatch('arc-draw',liscom(icom))) then
         call annot_find_object(obj,status)
         object_status = 1
         call annot_getdim(object_arc,object_dim,status)
         object_type = object_arc
         object_fill = graphic_fill_opt
         call graphic_copy_line_opt(annot_line_opts,
     *                                object_opt,status)
         call graphic_copy_object(object,annot_object(1,obj),status)
         if (annot_immediate.ge.1) then
           call annot_plot(obj,status)
           if (annot_immediate.eq.2) then
             annot_object(1,obj) = 0
             call graphic_copy_object(annot_object(1,obj),object,status)
             call annot_free_text(object_lable,status)
           endif
         endif
         call cmd_err(status,'arc',' ')

       elseif (chr_cmatch('circle-draw',liscom(icom))) then
         call annot_find_object(obj,status)
         object_status = 1
         call annot_getdim(object_circle,object_dim,status)
         object_type = object_circle
         object_fill = graphic_fill_opt
         call graphic_copy_line_opt(annot_line_opts,
     *                                object_opt,status)
         call graphic_copy_object(object,annot_object(1,obj),status)
         call graphic_copy_object(object,annot_object(1,obj),status)
         if (annot_immediate.ge.1) then
           call annot_plot(obj,status)
           if (annot_immediate.eq.2) then
             annot_object(1,obj) = 0
             call graphic_copy_object(annot_object(1,obj),object,status)
             call annot_free_text(object_lable,status)
           endif
         endif
         call cmd_err(status,'circle',' ')

       elseif (chr_cmatch('ellipse-draw',liscom(icom))) then
         call annot_find_object(obj,status)
         object_status = 1
         call annot_getdim(object_ellipse,object_dim,status)
         object_type = object_ellipse
         object_fill = graphic_fill_opt
         call graphic_copy_line_opt(annot_line_opts,
     *                                object_opt,status)
         call graphic_copy_object(object,annot_object(1,obj),status)
         if (annot_immediate.ge.1) then
           call annot_plot(obj,status)
           if (annot_immediate.eq.2) then
             annot_object(1,obj) = 0
             call graphic_copy_object(annot_object(1,obj),object,status)
             call annot_free_text(object_lable,status)
           endif
         endif
         call cmd_err(status,'ellipse',' ')

       elseif (chr_cmatch('box-draw',liscom(icom))) then
         call annot_find_object(obj,status)
         object_status = 1
         call annot_getdim(object_box,object_dim,status)
         object_type = object_box
         object_fill = graphic_fill_opt
         call graphic_copy_line_opt(annot_line_opts,
     *                                object_opt,status)
         call graphic_copy_object(object,annot_object(1,obj),status)
         if (annot_immediate.ge.1) then
           call annot_plot(obj,status)
           if (annot_immediate.eq.2) then
             annot_object(1,obj) = 0
             call graphic_copy_object(annot_object(1,obj),object,status)
             call annot_free_text(object_lable,status)
           endif
         endif
         call cmd_err(status,'box',' ')

       elseif (chr_cmatch('cross-draw',liscom(icom))) then
         call annot_find_object(obj,status)
         object_status = 1
         call annot_getdim(object_cross,object_dim,status)
         object_type = object_cross
         object_fill = graphic_fill_opt
         call graphic_copy_line_opt(annot_line_opts,
     *                                object_opt,status)
         call graphic_copy_object(object,annot_object(1,obj),status)
         if (annot_immediate.ge.1) then
           call annot_plot(obj,status)
           if (annot_immediate.eq.2) then
             annot_object(1,obj) = 0
             call graphic_copy_object(annot_object(1,obj),object,status)
             call annot_free_text(object_lable,status)
           endif
         endif
         call cmd_err(status,'cross',' ')

       elseif (chr_cmatch('arrow-draw',liscom(icom))) then
         call annot_find_object(obj,status)
         object_status = 1
         call annot_getdim(object_arrow,object_dim,status)
         object_type = object_arrow
         object_fill = arrow_fill_style
         call graphic_copy_arrow_opt(annot_line_opts,
     *                              object_opt,status)
         call graphic_copy_object(object,annot_object(1,obj),status)
         if (annot_immediate.ge.1) then
           call annot_plot(obj,status)
           if (annot_immediate.eq.2) then
             annot_object(1,obj) = 0
             call graphic_copy_object(annot_object(1,obj),object,status)
             call annot_free_text(object_lable,status)
           endif
         endif
         call cmd_err(status,'arrow',' ')

       elseif (chr_cmatch('erase-draw',liscom(icom))) then
         call annot_find_object(obj,status)
         object_status = 1
         call annot_getdim(object_erase,object_dim,status)
         object_type = object_erase
         object_fill = graphic_fill_opt
         call graphic_copy_line_opt(annot_line_opts,
     *                                object_opt,status)
         call graphic_copy_object(object,annot_object(1,obj),status)
         if (annot_immediate.ge.1) then
           call annot_plot(obj,status)
           if (annot_immediate.eq.2) then
             annot_object(1,obj) = 0
             call graphic_copy_object(annot_object(1,obj),object,status)
             call annot_free_text(object_lable,status)
           endif
         endif
         call cmd_err(status,'erase',' ')

       elseif (chr_cmatch('text-draw',liscom(icom))) then
         call annot_find_object(obj,status)
         object_status = 1
         call annot_getdim(object_text,object_dim,status)
         call io_getwrd('Text (''in quotes'') : ',' ',string,ls,status)
         call annot_find_itext(string, itext, status)
         object_type = object_text
         object_lable = itext
         object_fill = graphic_fill_opt
         call graphic_copy_text_opt(annot_text_opts,
     *                              object_opt,status)
         call graphic_copy_object(object,annot_object(1,obj),status)
         if (annot_immediate.ge.1) then
           call annot_plot(obj,status)
           if (annot_immediate.eq.2) then
             annot_object(1,obj) = 0
             call graphic_copy_object(annot_object(1,obj),object,status)
             call annot_free_text(object_lable,status)
           endif
         endif
         call cmd_err(status,'text',' ')

       elseif (chr_cmatch('compound',liscom(icom))) then
         call annot_find_object(obj,status)
         object_status = 1
         call annot_getdim(object_comp,object_dim,status)
         call io_getwrd('File : ',' ',string,ls,status)
         call annot_find_itext(string, itext, status)
         object_type = object_comp
         object_lable = itext
         call graphic_copy_text_opt(graphic_text_opts,
     *                              object_opt,status)
         call graphic_copy_object(object,annot_object(1,obj),status)
         if (annot_immediate.ge.1) then
           call annot_plot(obj,status)
           if (annot_immediate.eq.2) then
             annot_object(1,obj) = 0
             call graphic_copy_object(annot_object(1,obj),object,status)
             call annot_free_text(object_lable,status)
           endif
         endif
         call cmd_err(status,'compound',' ')

       elseif (chr_cmatch('set-text-style',liscom(icom))) then
         call graphic_get_text_opt( annot_text_opts, status)

       elseif (chr_cmatch('set-line-style',liscom(icom))) then
         call graphic_get_line_opt( annot_line_opts, status)

       elseif (chr_cmatch('set-fill-style',liscom(icom))) then
         call io_geti('Fill-style : ','2',graphic_fill_opt,status)
         if (graphic_fill_opt.le.1) then
           graphic_fill_opt = 1
         else
           graphic_fill_opt = 2
         endif

       elseif (chr_cmatch('set-arrow-style',liscom(icom))) then
         call graphic_get_arrow_opt( annot_arrow_opts, status)
         call io_getr( 'Arrow-head-angle (degrees) : ','*',
     *                 arrow_head_angle,status)
         call io_getr( 'Arrow-head-vent (fraction of head) : ',
     *                 '*',arrow_head_vent,status)
         call io_geti( 'Arrow-fill-style (1=filled 2=empty) : ',
     *                 '*',arrow_fill_style,status)
         if (arrow_fill_style.le.1) then
           arrow_fill_style = 1
         else
           arrow_fill_style = 2
         endif

       elseif (chr_cmatch('set-plot-options',liscom(icom))) then
         opts(1)='auto-plot ..... plot when object defined'
         opts(2)='deferred ...... plot on plot command only'
         opts(3)='temporary ....  plot when defined do not store'
         opts(4)='cursor-input .. select cursor input mode'
         opts(5)='command-input . select command input mode'
         call io_getopt( 'Option-option (?=list) : ','auto-plot',
     *                   opts, 5, opt, status )
         if (chr_cmatch(opt,'auto-plot')) then
           annot_immediate = 1
         elseif (chr_cmatch(opt,'deferred')) then
           annot_immediate = 0
         elseif (chr_cmatch(opt,'temporary')) then
           annot_immediate = 2
         elseif (chr_cmatch(opt,'cursor-input')) then
           annot_cursor = .true.
         elseif (chr_cmatch(opt,'command-input')) then
           annot_cursor = .false.
         endif

       elseif (chr_cmatch('plot',liscom(icom))) then
         opts(1)='all ........... plot everyting'
         opts(2)='object ........ plot specified object'
         opts(3)='refresh ....... refresh the whole plot'
         call io_getopt( 'Plot-option (?=list) : ','all',
     *                   opts, 3, opt, status )
         if (chr_cmatch(opt,'all')) then
           plot_refresh = .false.
           call annot_plot( 0, status )
         elseif (chr_cmatch(opt,'refresh')) then
           plot_refresh = .true.
           call annot_plot( 0, status )
           plot_refresh = .false.
         elseif (chr_cmatch(opt,'object')) then
           call io_geti('Object (0=all; -1=outstanding) : ','-1',
     *                   n, status )
           if (n.eq.-1) then
             plot_refresh = .false.
             n = 0
           else
             plot_refresh = .true.
           endif
           call annot_plot( n, status )
         endif

       elseif (chr_cmatch('initialise',liscom(icom))) then
         opts(1)='all ........... initialise everything for drawing'
         opts(2)='plot .......... initialise for (re-)plotting'
         opts(3)='options ....... initialise options'
         opts(4)='drawing ....... initialise objects in drawing'
         call io_getopt( 'Initialise-option (?=list) : ','all',
     *                   opts, 4, opt, status )
         if (chr_cmatch(opt,'plot')) then
           do n = 1,max_annot_object
             annot_object(1,n) = abs(annot_object(1,n))
           enddo
         endif
         if (chr_cmatch(opt,'all') .or.
     *       chr_cmatch(opt,'drawing')) then
           do n = 1,max_annot_object
             annot_object(1,n) = 0
             call graphic_copy_object(annot_object(1,n),object,status)
             call annot_free_text(object_lable,status)
           enddo
         endif
         if (chr_cmatch(opt,'all') .or.
     *       chr_cmatch(opt,'options')) then
           call graphic_default_line_opt( annot_line_opts, status )
           call graphic_default_text_opt( annot_text_opts, status )
           call graphic_default_arrow_opt( annot_arrow_opts, status )
           arrow_head_angle = 30.0
           arrow_head_vent = 0.1
           arrow_fill_style = 1
         endif

       elseif (chr_cmatch('list-objects',liscom(icom))) then
         do n=1,max_annot_object
           if (annot_object(1,n).ne.0) then
             call graphic_show_object( annot_object(1,n), status )
           endif
         enddo

       elseif (chr_cmatch('delete-object',liscom(icom))) then
         n = -1
         call io_geti( 'Object-to-delete : ',' ',n,status )
         if (n.ne.-1 .and. status.eq.0) then
           if (n.ge.1 .and. n.le.max_annot_object) then
             annot_object(1,n) = 0
             call graphic_copy_object(annot_object(1,n),object,status)
             call annot_free_text(object_lable,status)
           else
             call io_wrout('***(ANNOT-DELETE) Unkown object to delete')
           endif
         endif

       elseif (chr_cmatch('get',liscom(icom))) then
         opts(1)='cursor-input .. cursor input from device'
         opts(2)='coordinates ... current coordinates'
         opts(3)='max-objects ... maximum number of objects'
         opts(4)='object-type ... object type for object'
         opts(5)='object-status . object status for object'
         call io_getopt( 'Get-option (?=list) : ','cursor-input',
     *                   opts, 5, opt, status )

         string = ' '
         if (chr_cmatch(opt,'cursor-input')) then
           call graphic_cursor( status )

         elseif (chr_cmatch(opt,'max-objects')) then
           call chr_chitoc( max_annot_object, string, n )
           call cmd_setlocal('%max-objects',string(1:n),status)

         elseif (chr_cmatch(opt,'object-type')) then
           call io_geti( 'Object-index : ','1',n,status)
           if (n.ge.1 .and. n.le.max_annot_object) then
             call graphic_copy_object(annot_object(1,n),object,status)
             call cmd_setlocal('%object-type',
     *                         object_name(object_type),status)
           endif

         elseif (chr_cmatch(opt,'object-status')) then
           call io_geti( 'Object-index : ','1',n,status)
           if (n.ge.1 .and. n.le.max_annot_object) then
             call graphic_copy_object(annot_object(1,n),object,status)
             call chr_chitoc(object_status,string,n)
             call cmd_setlocal('%object-statys',
     *                         string(1:n),status)
           endif
         endif

       elseif (chr_cmatch('update-object',liscom(icom))) then
         opts(1)='all ........... plot everyting'
         opts(2)='object ........ plot specified object'
         call io_getopt( 'Update-option (?=list) : ','all',
     *                   opts, 2, opt, status )
         if (chr_cmatch(opt,'all')) then
           call annot_update( 0, status )
         elseif (chr_cmatch(opt,'object')) then
           call io_geti('Object (0=all; -1=outstanding) : ','-1',
     *                   n, status )
           if (n.le.-1) then
             n = -1
           endif
           call annot_update( n, status )
         endif

       elseif (chr_cmatch('help',liscom(icom))) then
         call hlp_setfil( annotate_helpfile, status )
         call hlp_system( ' ', ' ', .true., .true., status)
         call cmd_err(status,'HELP (Annotate)',' ')

       end if
       goto 100
       end
C
C
*+ annot_find_object

       subroutine annot_find_object( obj, s )
C      -------------------------------------
C
C Find the next available object
C
C Returned:
C   found object index
       integer    obj
C
C Updated:
C   error status
       integer    s
C
C Find the next available object -- if no more object structures are
C available then return an error status.
C-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'
       include '../include/plt_error_defn.inc'

C local variables
       integer     n

C check status on entry
       if (s.ne.0) return

C loop through the available objects
       n = 1
       obj = 0
       do while (n.le.max_annot_object .and. obj.eq.0)
         if (annot_object(1,n).eq.0) then
           obj = n
         endif
         n = n + 1
       enddo
       if (obj.eq.0) then
         s = no_object
       endif
       if (s.ne.no_object) then
         call cmd_err(s,'annot_find_object',' ')
       endif
       end
C
C
*+ annot_find_itext

       subroutine annot_find_itext( string, itext, s )
C      ----------------------------------------------
C
C Find the next available text-string buffer and save the text
C
C Given:
C   text string to load into text buffer
       character*(*) string
C Returned:
C   found text-string buffer
       integer    itext
C
C Updated:
C   error status
       integer    s
C
C Find the next available text-string buffer -- if no more buffers are
C available then return an error status.  If there is an available
C buffer then the text string is loaded into the buffer.  Long strings
C may overlap more than one buffer.
C-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'
       include '../include/plt_error_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer     ls, nb, n, nn, i1,i2, lb(max_annot_text)

C check status on entry
       if (s.ne.0) return

C calculate the number of buffers needed to hold this string
       ls = chr_lenb(string)
       nb = (ls-1)/len_annot_text + 1

C loop through the available buffers and make sure there are suffient free
       nn = 0
       n = 1
       do while (n.le.max_annot_text .and. nn.lt.nb)
         if (annot_itext(n).eq.0) then
           nn = nn + 1
           lb(nn) = n
         endif
         n = n + 1
       enddo

C load buffers or return an error message
       if (nn.ge.nb) then
         itext = lb(1)
         do n = 1,nb
           annot_itext(lb(n)) = lb(1)
           i1 = len_annot_text*(n-1) + 1
           i2 = min(len_annot_text*n,ls)
           annot_text(lb(n)) = string(i1:i2)
         enddo
       else
         s = no_itext
       endif
       if (s.ne.no_itext) then
         call cmd_err(s,'annot_find_itext',' ')
       endif
       end
C
C
*+ annot_free_text

       subroutine annot_free_text( itext, s )
C      -------------------------------------
C
C Free the text buffers associated with itext
C
C Given:
C text-string buffer
       integer    itext
C
C Updated:
C   error status
       integer    s
C
C-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'
       include '../include/plt_error_defn.inc'
       include '/mrao/include/chrlib_functions.inc'
C local variables
       integer     n

C check status on entry
       if (s.ne.0) return

C loop through the available buffers
       do n = 1,max_annot_text
         if (annot_itext(n).eq.itext) then
           annot_itext(n) = 0
         endif
       enddo
       call cmd_err(s,'annot_free_text',' ')
       end
C
C
*+ annot_get_itext

       subroutine annot_get_itext( itext, string, s )
C      ---------------------------------------------
C
C Get the text-string associated with the text buffer itext
C
C Given:
C   text-string buffer
       integer    itext
C Returned:
C   text string
       character*(*) string
C
C Updated:
C   error status
       integer    s
C
C Return the text in the text buffer pointed to by "itext"
C-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'
       include '../include/plt_error_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer     nb, n

C check status on entry
       if (s.ne.0) return

C loop through the available buffers and find matching buffers
       string = ' '
       nb = 0
       do n = 1,max_annot_text
         if (annot_itext(n).eq.itext) then
           nb = nb + 1
           if (nb.eq.1) then
             string = annot_text(n)
           else
             string = string(1:(nb-1)*len_annot_text)//annot_text(n)
           endif
         endif
       enddo
       call cmd_err(s,'annot_get_itext',' ')
       end
C
C
*+ annot_plot

       subroutine annot_plot( obj, s )
C      ------------------------------
C
C Plot the specified object or all objects
C
C Given:
C   object to plot, or 0 == ALL objects
       integer    obj
C
C Updated:
C   error status
       integer    s
C
C The specified object (or all objects) are plotted.  If a device has not
C been specified and the device is not open then the user will be prompted
C for an output device.
C-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C local variables
       integer    n, n1, n2, iunit
       integer    max_pts, i
       parameter (max_pts = 100)
       real*4     x(max_pts), y(max_pts), u, v, w, z
       character  string*256

C check status on entry
       if (s.ne.0) return

C check object index
       if (obj.le.0 .or. obj.gt.max_annot_object) then
         n1 = 1
         n2 = max_annot_object
       else
         n1 = obj
         n2 = obj
       endif
C list through objects to draw
       do n = n1,n2
         call graphic_copy_object( annot_object(1,n), object, s )
         if (object_status.ne.0) then
           if ( (plot_refresh.and.object_status.lt.0) .or.
     *          (.not.plot_refresh.and.object_status.gt.0)) then

             if (object_type.eq.object_line) then
               call graphic_set_line_opt( object_opt, s )
               call pgmove( object_dim(1), object_dim(2) )
               call pgdraw( object_dim(3), object_dim(4) )

             elseif (object_type.eq.object_arc) then
               call graphic_set_line_opt( object_opt, s )

             elseif (object_type.eq.object_circle) then
               call graphic_set_line_opt( object_opt, s )
               do i = 1,max_pts-1
                 z = 2.0*3.14159265*float(i-1)/float(max_pts-1)
                 x(i) = object_dim(1) +
     *                 object_dim(3) * cos(z)
                 y(i) = object_dim(2) +
     *                 object_dim(3) * sin(z)
               enddo
               x(max_pts) = x(1)
               y(max_pts) = y(1)
               if (object_fill.eq.1) then
                 call pgsfs( object_fill )
                 call pgpoly( (max_pts-1), x, y )
                 call pgsfs( 2 )
               endif
               call pgline( max_pts, x, y )

             elseif (object_type.eq.object_ellipse) then
               call graphic_set_line_opt( object_opt, s )
               w = object_dim(5) * 3.14159265 / 180.0
               do i = 1,max_pts-1
                 z = 2.0*3.14159265*float(i-1)/float(max_pts-1)
                 u = object_dim(3) * sin(z)
                 v = object_dim(4) * cos(z)
                 y(i) = v*cos(w) - u*sin(w) + object_dim(2)
                 x(i) = u*cos(w) + v*sin(w) + object_dim(1)
               enddo
               x(max_pts) = x(1)
               y(max_pts) = y(1)
               if (object_fill.eq.1) then
                 call pgsfs( object_fill )
                 call pgpoly( (max_pts-1), x, y )
                 call pgsfs( 2 )
               endif
               call pgline( max_pts, x, y )

             elseif (object_type.eq.object_box) then
               call graphic_set_line_opt( object_opt, s )
               if (object_fill.eq.1) then
                 call pgsfs( object_fill )
                 call pgrect( object_dim(1), object_dim(3),
     *                        object_dim(2), object_dim(4) )
                 call pgsfs( 2 )
               endif
               call pgmove( object_dim(1), object_dim(2))
               call pgdraw( object_dim(3), object_dim(2))
               call pgdraw( object_dim(3), object_dim(4))
               call pgdraw( object_dim(1), object_dim(4))
               call pgdraw( object_dim(1), object_dim(2))

             elseif (object_type.eq.object_erase) then
               call pgsci( colour_background )
               call pgsfs( 1 )
               call pgrect( object_dim(1), object_dim(3),
     *                      object_dim(2), object_dim(4) )
               call pgsfs( 2 )
               call pgsci( colour_foreground )

             elseif (object_type.eq.object_cross) then
               call graphic_set_line_opt( object_opt, s )
               call pgmove( (object_dim(1)-object_dim(3)),
     *                      object_dim(2) )
               call pgdraw( (object_dim(1)+object_dim(3)),
     *                      object_dim(2) )
               call pgmove( object_dim(1),
     *                      (object_dim(2)-object_dim(3)) )
               call pgdraw( object_dim(1),
     *                      (object_dim(2)+object_dim(3)) )

             elseif (object_type.eq.object_arrow) then
               call graphic_set_arrow_opt( object_opt, s )
               call pgsah( object_fill, object_dim(5), object_dim(6))
               call pgarro(object_dim(1),object_dim(2),
     *                     object_dim(3),object_dim(4))
               call pgsfs( 2 )

             elseif (object_type.eq.object_text) then
               call graphic_set_text_opt( object_opt, s )
               call annot_get_itext( object_lable, string, s )
               call pgptext( object_dim(1), object_dim(2),
     *                       object_dim(3), 0.0,
     *                       string(1:chr_lenb(string)) )

             elseif (object_type.eq.object_comp) then
               string = ' '
               call graphic_set_text_opt( object_opt, s )
               call annot_get_itext( object_lable, string, s )
               if (chr_lenb(string).gt.0) then
                 call graphic_open_compound( iunit, string, s )
                 if (s.eq.0) then
                   call io_setin(iunit)
                 endif
               endif
               if (s.eq.0) then
                 call graphic_compound(object_dim,s)
                 call io_setin(terminal_in)
                 if (iunit.ne.terminal_in) then
                   close(iunit)
                 endif
               endif
               call cmd_err(s,'draw','compound object error')
               s = 0

             endif
             object_status = -1
             call graphic_copy_object( object, annot_object(1,n), s )
           endif
         endif
       enddo
       end
C
C
*+ annot_getdim

       subroutine annot_getdim( obj, dim, s )
C      -------------------------------------
C
C Get from the user dimensions for the specified graphics object
C
C Given:
C   object type
       integer    obj
C Returned:
C   dimensions for this object
       real*4     dim(*)
C
C Updated:
C   error status
       integer    s
C
C Find the next available object -- if no more object structures are
C available then return an error status.
C-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'

C local variables
       real*4    xy(2)

C debug/report function
       logical   cmd_dblev

C check status on entry
       if (s.ne.0) return

C use different methods for cursor/text input for each of the input
C options
       if (obj.eq.object_line) then
         if (annot_cursor) then
           if (cmd_dblev(1)) then
             call io_wrout('ANNOTATE: Mark end of line')
           endif
           call graphic_getpos(dim(1), s)
           call graphic_getpos(dim(3), s)
         else
           call io_getnr('Start-xy : ','0,0',dim(1),2,s)
           call io_getnr('End-xy : ','1,1',dim(3),2,s)
         endif
       elseif (obj.eq.object_arc) then
         if (annot_cursor) then
           if (cmd_dblev(1)) then
             call io_wrout(
     *            'ANNOTATE: Mark arc start, end and one mid-point')
           endif
           call graphic_getpos(dim(1), s)
           call graphic_getpos(dim(3), s)
           call graphic_getpos(dim(5), s)
         else
           call io_getnr('Start-arc : ','0,0',dim(1),2,s)
           call io_getnr('End-arc : ','0,0',dim(3),2,s)
           call io_getr('Mid-point-on-arc : ','1',dim(5),s)
         endif
       elseif (obj.eq.object_circle) then
         if (annot_cursor) then
           if (cmd_dblev(1)) then
             call io_wrout(
     *            'ANNOTATE: Mark centre and one point on circle')
           endif
           call graphic_getpos(dim(1), s)
           call graphic_getpos(xy, s)
           dim(3) = sqrt( (dim(1)-xy(1))**2 + (dim(2)-xy(2))**2 )
         else
           call io_getnr('Centre : ','0,0',dim(1),2,s)
           call io_getr('Radius : ','1',dim(3),s)
         endif
       elseif (obj.eq.object_ellipse) then
         if (annot_cursor) then
           if (cmd_dblev(1)) then
             call io_wrout(
     *           'ANNOTATE: Mark centre major and minor semi-axes')
           endif
           call graphic_getpos(dim(1), s)
           call graphic_getpos(xy, s)
           dim(3) = sqrt( (dim(1)-xy(1))**2 + (dim(2)-xy(2))**2 )
           dim(5) = 180.0*atan2( (xy(2)-dim(2)), (xy(1)-dim(1)) ) /
     *                   3.14159265
           call graphic_getpos(xy, s)
           dim(4) = sqrt( (dim(1)-xy(1))**2 + (dim(2)-xy(2))**2 )
         else
           call io_getnr('Centre : ','0,0',dim(1),2,s)
           call io_getr('Semi-Major-axis : ','1',dim(3),s)
           call io_getr('Semi-Minor-axis : ','1',dim(4),s)
           call io_getr('Position-angle (degrees) : ','0',dim(5),s)
         endif
       elseif (obj.eq.object_box) then
         if (annot_cursor) then
           if (cmd_dblev(1)) then
             call io_wrout('ANNOTATE: Mark opposite corners of box')
           endif
           call graphic_getpos(dim(1), s)
           call graphic_getpos(dim(3), s)
         else
           call io_getnr('Corner-1 : ','0,0',dim(1),2,s)
           call io_getnr('Corner-2 : ','1,1',dim(3),2,s)
         endif
       elseif (obj.eq.object_erase) then
         if (annot_cursor) then
           if (cmd_dblev(1)) then
             call io_wrout('ANNOTATE: Mark opposite corners of box')
           endif
           call graphic_getpos(dim(1), s)
           call graphic_getpos(dim(3), s)
         else
           call io_getnr('Corner-1 : ','0,0',dim(1),2,s)
           call io_getnr('Corner-2 : ','1,1',dim(3),2,s)
         endif
       elseif (obj.eq.object_cross) then
         if (annot_cursor) then
           if (cmd_dblev(1)) then
             call io_wrout('ANNOTATE: Mark centre and end of one arm')
           endif
           call graphic_getpos(dim(1), s)
           call graphic_getpos(xy, s)
           dim(3) = sqrt( (dim(1)-xy(1))**2 + (dim(2)-xy(2))**2 )
         else
           call io_getnr('Centre : ','0,0',dim(1),2,s)
           call io_getr('Arm-length : ','1',dim(3),s)
         endif
       elseif (obj.eq.object_arrow) then
         if (annot_cursor) then
           if (cmd_dblev(1)) then
             call io_wrout('ANNOTATE: Mark tail and head of arrow')
           endif
           call graphic_getpos(dim(3), s)
           call graphic_getpos(dim(1), s)
           dim(5) = arrow_head_angle
           dim(6) = arrow_head_vent
         else
           call io_getnr('Tail : ','0,0',dim(3),2,s)
           call io_getnr('Head : ','1,1',dim(1),2,s)
           dim(5) = arrow_head_angle
           dim(6) = arrow_head_vent
         endif
       elseif (obj.eq.object_comp) then
         if (annot_cursor) then
           if (cmd_dblev(1)) then
             call io_wrout(
     *            'ANNOTATE: Mark opposite corners of bounding box')
           endif
           call graphic_getpos(dim(1), s)
           call graphic_getpos(dim(3), s)
         else
           call io_getnr('Corner-1 : ','0,0',dim(1),2,s)
           call io_getnr('Corner-2 : ','1,1',dim(3),2,s)
         endif
         dim(5) = dim(2)
         dim(2) = dim(3)
         dim(3) = dim(5)
         dim(5) = min(dim(1),dim(2))
         dim(6) = max(dim(1),dim(2))
         dim(1) = dim(5)
         dim(2) = dim(6)
         dim(5) = min(dim(3),dim(4))
         dim(6) = max(dim(3),dim(4))
         dim(3) = dim(5)
         dim(4) = dim(6)
       elseif (obj.eq.object_text) then
         if (annot_cursor) then
           if (cmd_dblev(1)) then
             call io_wrout('ANNOTATE: Mark position for text')
           endif
           call graphic_getpos(dim(1), s)
           dim(3) = 0.0
         else
           call io_getnr('Position : ','0,0',dim(1),2,s)
           call io_getr('Angle (degrees) : ','0',dim(3),s)
         endif
       endif
       end
C
C
*+ annot_update

       subroutine annot_update( obj, s )
C      --------------------------------
C
C Update style for the specified object or all objects
C
C Given:
C   object to update, or 0 == ALL objects
       integer    obj
C
C Updated:
C   error status
       integer    s
C
C The style of the specified object (or all objects) are et equal to the
C current style for the object of their given type
C-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer    n, n1, n2
       logical    outst


C check status on entry
       if (s.ne.0) return

C check object index
       if (obj.le.0 .or. obj.gt.max_annot_object) then
         n1 = 1
         n2 = max_annot_object
         if (obj.eq.-1) then
           outst = .true.
         else
           outst = .false.
         endif
       else
         outst = .false.
         n1 = obj
         n2 = obj
       endif


C list through objects to update
       if (s.ne.0) goto 999
       do n = n1,n2
         call graphic_copy_object( annot_object(1,n), object, s )
         if (object_status.ne.0) then
           if ( (outst .and. object_status.gt.0) .or.
     *          (.not.outst) ) then
             if (object_type.eq.object_text) then
               call graphic_copy_text_opt( annot_text_opts,
     *                                     object_opt, s )
             elseif (object_type.eq.object_arrow) then
               call graphic_copy_arrow_opt( annot_arrow_opts,
     *                                      object_opt, s )
               object_fill = arrow_fill_style
               object_dim(5) = arrow_head_angle
               object_dim(6) = arrow_head_vent
               object_fill = graphic_fill_opt
             else
               call graphic_copy_line_opt( annot_line_opts,
     *                                     object_opt, s )
               object_fill = graphic_fill_opt
             endif
             call graphic_copy_object( object, annot_object(1,n), s )
           endif
         endif
       enddo

 999   call cmd_err(s,'annot_update',' ')

       end
C
C
       subroutine annot_start( annot_data, status )
C      --------------------------------------------
C
C Initialise the annotation data structure
C
C Given:
C   annotation data
       integer    annot_data(*)
C
C Updated:
C   error status
       integer   status
C
C The annotation data passed via annot_data is loaded into the standard
C annotation data structure.
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'

C local variables
       integer     n, n1, n2, ip

       if (status.ne.0) return

C copy basic control data
       do n=1,ipa1-1
         annot(n) = annot_data(n)
       enddo

C partition this data structure
       annot_control(2) = (annot_control(1)-ipa1+1)/
     *                    (len_object+1+len_annot_text/4)
       annot_control(2) = min(annot_control(2),max_annot_object)
       annot_control(3) = annot_control(2)

C copy vaiable length structures
       n1 = ipa1
       n2 = n1 + annot_control(2)*len_object - 1
       ip = ipa1
       do n=n1,n2
         annot(ip) = annot_data(n)
         ip = ip+1
       enddo
       n1 = n2+1
       n2 = n1 + annot_control(3)*len_annot_text/4 - 1
       ip = ipa2
       do n=n1,n2
         annot(ip) = annot_data(n)
         ip = ip+1
       enddo
       n1 = n2+1
       n2 = n1 + annot_control(3) - 1
       ip = ipa3
       do n=n1,n2
         annot(ip) = annot_data(n)
         ip = ip+1
       enddo

C update the coordinate systen
       call graphic_scale_coord( status )

       call cmd_err( status, 'annot_start', ' ')
       end
C
C
       subroutine annot_end( annot_data, status )
C      ------------------------------------------
C
C Copy the annotation data structure into the annotation data array
C
C Returned:
C   annotation data
       integer    annot_data(*)
C
C Updated:
C   error status
       integer   status
C
C The annotation data, annot_data, is updated from the standard
C annotation data structure.
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'

C local variables
       integer     n, n1, n2, ip

       if (status.ne.0) return

C copy basic control data
       do n=1,ipa1-1
         annot_data(n) = annot(n)
       enddo

C copy vaiable length structures
       n1 = ipa1
       n2 = n1 + annot_control(2)*len_object - 1
       ip = ipa1
       do n=n1,n2
         annot_data(n) = annot(ip)
         ip = ip+1
       enddo
       n1 = n2+1
       n2 = n1 + annot_control(3)*len_annot_text/4 - 1
       ip = ipa2
       do n=n1,n2
         annot_data(n) = annot(ip)
         ip = ip+1
       enddo
       n1 = n2+1
       n2 = n1 + annot_control(3) - 1
       ip = ipa3
       do n=n1,n2
         annot_data(n) = annot(ip)
         ip = ip+1
       enddo

C restore coordinate system
       call graphic_pop_coord( status )
       call cmd_err( status, 'annot_end', ' ')
       end
C
C
       subroutine annotate_init( annot_data, opt, status )
C      ---------------------------------------------------
C
C Perform full initialisation for annotations
C
C Updated:
C   annotation data
       integer        annot_data(*)
C Given:
C   initialisation option
       character*(*)  opt
C Updated:
C   error status
       integer        status
C
C Initialisation for annotations is performed accoding to opt.  If opt
C is not recognized no action is performed; this routine therefore provides
C for standard graphics initialisations.
C-

       include '/mrao/include/chrlib_functions.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'

       integer  n

       if (status.ne.0) return
       call annot_start( annot_data, status )

         if (chr_cmatch(opt,'plot')) then
           do n = 1,max_annot_object
             annot_object(1,n) = abs(annot_object(1,n))
           enddo
         endif
         if (chr_cmatch(opt,'all') .or.
     *       chr_cmatch(opt,'plot')) then
           do n = 1,max_annot_object
             annot_object(1,n) = 0
           enddo
         endif
         if (chr_cmatch(opt,'all') .or.
     *       chr_cmatch(opt,'options')) then
           call graphic_default_line_opt( annot_line_opts, status )
           call graphic_default_text_opt( annot_text_opts, status )
           arrow_head_angle = 30.0
           arrow_head_vent = 0.1
           arrow_fill_style = 1
         endif

       call annot_end( annot_data, status )
       call cmd_err(status,'Annotate_init',' ')

       end
C
C
       subroutine annotate_plot( annot_data, opt, status )
C      ---------------------------------------------------
C
C Perform plot options for annotations
C
C Updated:
C   annotation data
       integer        annot_data(*)
C Given:
C   initialisation option
       character*(*)  opt
C Updated:
C   error status
       integer        status
C
C Plotting for annotions is performed according to opt which is assumed
C to take one of the standard graphic system values.
C-

       include '/mrao/include/chrlib_functions.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_annot_defn.inc'

       if (status.ne.0) return
       call annot_start( annot_data, status )

         call annot_plot( 0, status )

       call annot_end( annot_data, status )
       call cmd_err(status,'Annotate_init',' ')

       end








