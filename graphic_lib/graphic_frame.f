*+ graphic_frame

       subroutine graphic_frame( fopt, s )
C      -----------------------------------
C
C Draw a frame according to the specified frame options
C
C Given:
C   frame options structure
       integer    fopt(*)
C
C Updated:
C   Error status
       integer    s
C
C A frame is drawn in the current window using the options specified
C in the supplied frame options structure.
C
C-

       include '../include/plt_basic_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       character*10   xopt, yopt, opt
       integer        l, lx, xntick, yntick
       real*4         xtick, ytick

C check status on entry and copy structure
       if (s.ne.0) return
       call graphic_copy_frame_opt( fopt, frame_opt, s )
C check for need to initialise
       if (frame_opt_set.eq.0) then
         call graphic_default_frame_opt( frame_opt, s )
       endif

C interprete structure
C .. line style
       call graphic_set_text_opt( frame_opt_text_opt, s )
       call graphic_copy_axis_opt( frame_opt_x_axis, axis_opt, s)
       call graphic_set_line_opt( axis_opt_line_axis, s )
C .. assemble axis options
       do lx=1,2
         opt = ' '
         if (lx.eq.1) then
           call graphic_copy_axis_opt( frame_opt_x_axis, axis_opt, s)
         else
           call graphic_copy_axis_opt( frame_opt_y_axis, axis_opt, s)
         endif
C ... sides
         if (axis_opt_side.eq.1) then
           opt = 'QB'
         elseif (axis_opt_side.eq.2) then
           opt = 'QC'
         elseif (axis_opt_side.eq.3) then
           opt = 'QBC'
         else
           opt = 'Q'
         endif
C ... tick type
         l = chr_lenb(opt)
         if (axis_opt_tickt.eq.2) then
           opt = opt(1:l)//'I'
         elseif (axis_opt_tickt.eq.3) then
           opt = opt(1:l)//'P'
         endif
C ... axes
         l = chr_lenb(opt)
         if (axis_opt_axis) then
           opt = opt(1:l)//'A'
         endif
C ... ticks
         l = chr_lenb(opt)
         if (axis_opt_tickl) then
           opt = opt(1:l)//'T'
         endif
         l = chr_lenb(opt)
         if (axis_opt_ticks) then
           opt = opt(1:l)//'S'
         endif
C label logarithmically
         l = chr_lenb(opt)
         if (axis_opt_log) then
           opt = opt(1:l)//'L'
         endif

         if (lx.eq.1) then
           xopt = opt(2:chr_lenb(opt))
           xtick = axis_opt_xtick
           xntick = axis_opt_ntick
         else
           yopt = opt(2:chr_lenb(opt))
           ytick = axis_opt_xtick
           yntick = axis_opt_ntick
         endif
       enddo

C draw box
       call pgbox(xopt,xtick,xntick,yopt,ytick,yntick)

C add grid if required
       if (axis_opt_grid) then
         call graphic_set_line_opt(axis_opt_line_grid,s)
         call pgbox('G',xtick,xntick,'G',ytick,yntick)
       endif

C add text to this box
       do lx=1,2
         opt = 'Q'
         if (lx.eq.1) then
           call graphic_copy_axis_opt( frame_opt_x_axis, axis_opt, s)
         else
           call graphic_copy_axis_opt( frame_opt_y_axis, axis_opt, s)
         endif
C ... label logarithmically
         l = chr_lenb(opt)
         if (axis_opt_log) then
           opt = opt(1:l)//'L'
         endif
C ... labels
         l = chr_lenb(opt)
         if ( (abs(axis_opt_label).eq.1) .or.
     *        (abs(axis_opt_label).eq.10) ) then
           opt = opt(1:l)//'N'
         elseif ( (abs(axis_opt_label).eq.1) .or.
     *            (abs(axis_opt_label).eq.10) ) then
           opt = opt(1:l)//'M'
         elseif ( (abs(axis_opt_label).eq.3) .or.
     *            (abs(axis_opt_label).eq.30) ) then
           opt = opt(1:l)//'NM'
         endif
         l = chr_lenb(opt)
         if ( (axis_opt_label.lt.0) .and.
     *        (axis_opt_label.gt.-9) ) then
           opt = opt(1:l)//'2'
         else if (axis_opt_label.lt.-9 ) then
           opt = opt(1:l)//'1'
         endif
         l = chr_lenb(opt)
         if (axis_opt_labelr) then
           opt = opt(1:l)//'V'
         endif

         if (lx.eq.1) then
           xopt = opt(2:chr_lenb(opt))
           xtick = axis_opt_xtick
           xntick = axis_opt_ntick
         else
           yopt = opt(2:chr_lenb(opt))
           ytick = axis_opt_xtick
           yntick = axis_opt_ntick
         endif
       enddo
       call graphic_set_text_opt( frame_opt_text_opt, s )
       call pgbox(xopt,xtick,xntick,yopt,ytick,yntick)

       call cmd_err(s,'graphic_frame',' ')
       end

