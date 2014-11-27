*+ graphic_get_frame_opt

       subroutine graphic_get_frame_opt( fopt, s )
C      -------------------------------------------
C
C Read a description for frame style options from the user
C
C Updated:
C   line options structure
       integer   fopt(*)
C   error status
       integer   s
C
C The user is prompted for all the elements of a frame style
C definition.
C-

       include '../include/plt_basic_defn.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C local variables
       integer       l, lcli
       integer       nopt
       character*60  option(21), opt, cli*256
       logical       l1, l2
       integer       i1, i2, line(len_line_opt)
       real*4        r1

C check status on entry
       if ( s.ne.0 ) return

C copy the structure to the standard definition
       do l = 1,len_frame_opt
         frame_opt(l) = fopt(l)
       enddo

C get default options if not initialised
       if (frame_opt_set.eq.0) then
         call graphic_default_frame_opt( frame_opt, s )
       endif

C setup sub-options for this command
       nopt = 21
       option(1) = 'reset ............ reset to default options'
       option(2) = 'grid-option ...... turn grid on/off'
       option(3) = 'rotate-labels .... rotate labels to frame'
       option(4) = 'line-style ....... set line-style for frame'
       option(5) = 'grid-style ....... set line-style for grid'
       option(6) = 'text-style ....... set text style for labels'
       option(7) = 'ticks ............ set tick options for X,Y axes'
       option(8) = 'x-ticks .......... set tick options for X axis'
       option(9) = 'y-ticks .......... set tick options for X axis'
       option(10)= 'axes ............. turn axes on/off'
       option(11)= 'x-axis ........... turn X-axis on/off'
       option(12)= 'y-axis ........... turn Y-axis on/off'
       option(13)= 'labels ........... options for labels'
       option(14)= 'x-labels ......... options for x-labels'
       option(15)= 'y-labels ......... options for y-labels'
       option(16)= 'side ............. list sides to draw'
       option(17)= 'x-side ........... list sides (top/bottom) to draw'
       option(18)= 'y-side ........... list sides (left/right) to draw'
       option(19)= 'log-labels ....... select logarithmic labelling'
       option(20)= 'log-x-labels ..... select logarithmic X labelling'
       option(21)= 'log-y-labels ..... select logarithmic Y labelling'

 1     call io_getopt( 'Option (?=list) : ','reset',option,nopt,
     *                 opt,s)

C take action on options
       if (chr_cmatch(opt,'reset')) then
         call graphic_default_frame_opt( frame_opt, s )

       elseif (chr_cmatch(opt,'grid-option')) then
         l1 = io_onoff('Grid (on/off) : ','off',s)
         call graphic_copy_axis_opt(frame_opt_x_axis,axis_opt,s)
         axis_opt_grid = l1
         call graphic_copy_axis_opt(axis_opt,frame_opt_x_axis,s)
         call graphic_copy_axis_opt(frame_opt_y_axis,axis_opt,s)
         axis_opt_grid = l1
         call graphic_copy_axis_opt(axis_opt,frame_opt_y_axis,s)

       elseif (chr_cmatch(opt,'rotate-labels')) then
         call graphic_copy_axis_opt(frame_opt_y_axis,axis_opt,s)
         axis_opt_labelr = io_onoff('Rotate-labels (on/off) : ','off',s)
         call graphic_copy_axis_opt(axis_opt,frame_opt_y_axis,s)

       elseif (chr_cmatch(opt,'line-style')) then
         call graphic_get_line_opt(line,s)
         call graphic_copy_axis_opt(frame_opt_x_axis,axis_opt,s)
         call graphic_copy_line_opt(line,axis_opt_line_axis,s)
         call graphic_copy_axis_opt(axis_opt,frame_opt_x_axis,s)
         call graphic_copy_axis_opt(frame_opt_y_axis,axis_opt,s)
         call graphic_copy_line_opt(line,axis_opt_line_axis,s)
         call graphic_copy_axis_opt(axis_opt,frame_opt_y_axis,s)

       elseif (chr_cmatch(opt,'grid-style')) then
         call graphic_get_line_opt(line,s)
         call graphic_copy_axis_opt(frame_opt_x_axis,axis_opt,s)
         call graphic_copy_line_opt(line,axis_opt_line_grid,s)
         call graphic_copy_axis_opt(axis_opt,frame_opt_x_axis,s)
         call graphic_copy_axis_opt(frame_opt_y_axis,axis_opt,s)
         call graphic_copy_line_opt(line,axis_opt_line_grid,s)
         call graphic_copy_axis_opt(axis_opt,frame_opt_y_axis,s)

       elseif (chr_cmatch(opt,'text-style')) then
         call graphic_get_text_opt(frame_opt_text_opt,s)

       elseif (chr_cmatch(opt,'ticks').or.
     *         chr_cmatch(opt,'x-ticks').or.
     *         chr_cmatch(opt,'y-ticks')    ) then
         l1 = io_onoff('Major-ticks (on/off) : ','on',s)
         l2 = io_onoff('Minor-ticks (on/off) : ','on',s)
         call io_getr('Major-tick-separation (0=default) : ','0',
     *                r1,s)
         call io_geti('Number-minor-ticks (0=default) : ','0',
     *                i1,s)
         call io_geti('Tick-type (1=in, 2=out, 3=both in & out) : ',
     *                '1',i2,s)
         if ( chr_cmatch(opt,'x-ticks') .or.
     *        chr_cmatch(opt,'ticks')) then
           call graphic_copy_axis_opt(frame_opt_x_axis,axis_opt,s)
           axis_opt_tickl = l1
           axis_opt_ticks = l2
           axis_opt_tickt = i2
           axis_opt_ntick = i1
           axis_opt_xtick = r1
           call graphic_copy_axis_opt(axis_opt,frame_opt_x_axis,s)
         endif
         if ( chr_cmatch(opt,'y-ticks') .or.
     *            chr_cmatch(opt,'ticks')) then
           call graphic_copy_axis_opt(frame_opt_y_axis,axis_opt,s)
           axis_opt_tickl = l1
           axis_opt_ticks = l2
           axis_opt_tickt = i2
           axis_opt_ntick = i1
           axis_opt_xtick = r1
           call graphic_copy_axis_opt(axis_opt,frame_opt_y_axis,s)
         endif

       elseif (chr_cmatch(opt,'axes').or.
     *         chr_cmatch(opt,'x-axis').or.
     *         chr_cmatch(opt,'y-axis')    ) then
         l1 = io_onoff('Axis (on/off) : ','off',s)
         if ( chr_cmatch(opt,'x-axis') .or.
     *        chr_cmatch(opt,'axes')) then
           call graphic_copy_axis_opt(frame_opt_x_axis,axis_opt,s)
           axis_opt_axis = l1
           call graphic_copy_axis_opt(axis_opt,frame_opt_x_axis,s)
         endif
         if ( chr_cmatch(opt,'y-axis') .or.
     *            chr_cmatch(opt,'axes')) then
           call graphic_copy_axis_opt(frame_opt_y_axis,axis_opt,s)
           axis_opt_axis = l1
           call graphic_copy_axis_opt(axis_opt,frame_opt_y_axis,s)
         endif

       elseif (chr_cmatch(opt,'labels').or.
     *         chr_cmatch(opt,'x-labels').or.
     *         chr_cmatch(opt,'y-labels')    ) then
         call io_geti(
     *        'Label (0=none 1=bottom/left 2=top/right 3=both) : ',
     *        '1',i1,s)
         if (i1.gt.3) then
           i1 = 3
         endif
         if ( chr_cmatch(opt,'x-labels') .or.
     *        chr_cmatch(opt,'labels')) then
           call graphic_copy_axis_opt(frame_opt_x_axis,axis_opt,s)
           axis_opt_label = i1
           call graphic_copy_axis_opt(axis_opt,frame_opt_x_axis,s)
         endif
         if ( chr_cmatch(opt,'y-labels') .or.
     *            chr_cmatch(opt,'labels')) then
           call graphic_copy_axis_opt(frame_opt_y_axis,axis_opt,s)
           axis_opt_label = i1
           call graphic_copy_axis_opt(axis_opt,frame_opt_y_axis,s)
         endif

       elseif (chr_cmatch(opt,'side').or.
     *         chr_cmatch(opt,'x-side').or.
     *         chr_cmatch(opt,'y-side')    ) then
         call io_geti(
     *        'Plot-side (0=none 1=bottom/left 2=top/right 3=both) : ',
     *        '3',i1,s)
         if (i1.lt.0) then
           i1 = 0
         elseif (i1.gt.3) then
           i1 = 3
         endif
         if ( chr_cmatch(opt,'x-side') .or.
     *        chr_cmatch(opt,'side')) then
           call graphic_copy_axis_opt(frame_opt_x_axis,axis_opt,s)
           axis_opt_side = i1
           call graphic_copy_axis_opt(axis_opt,frame_opt_x_axis,s)
         endif
         if ( chr_cmatch(opt,'y-side') .or.
     *            chr_cmatch(opt,'side')) then
           call graphic_copy_axis_opt(frame_opt_y_axis,axis_opt,s)
           axis_opt_side = i1
           call graphic_copy_axis_opt(axis_opt,frame_opt_y_axis,s)
         endif

       elseif (chr_cmatch(opt,'log-labels').or.
     *         chr_cmatch(opt,'log-x-labels').or.
     *         chr_cmatch(opt,'log-y-labels')    ) then
         l1 = io_onoff('Label-logarithmically : ','off',s)
         if ( chr_cmatch(opt,'log-x-labels') .or.
     *        chr_cmatch(opt,'log-labels')) then
           call graphic_copy_axis_opt(frame_opt_x_axis,axis_opt,s)
           axis_opt_log = l1
           call graphic_copy_axis_opt(axis_opt,frame_opt_x_axis,s)
         endif
         if ( chr_cmatch(opt,'log-y-labels') .or.
     *            chr_cmatch(opt,'log-labels')) then
           call graphic_copy_axis_opt(frame_opt_y_axis,axis_opt,s)
           axis_opt_log = l1
           call graphic_copy_axis_opt(axis_opt,frame_opt_y_axis,s)
         endif

       endif

       call io_enqcli(cli,lcli)
       if (lcli.gt.0) goto 1

C copy structure to output
       if (s.eq.0) then
         frame_opt_set = 1
         do l=1,len_frame_opt
           fopt(l) = frame_opt(l)
         enddo
       endif
999    continue
       call cmd_err( s, 'graphic_get_frame_opt', ' ')

       end
