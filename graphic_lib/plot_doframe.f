C
C
*+ plot_doframe

       subroutine plot_doframe(status)
C      -------------------------------
C
C Plot frame, pips and grid
C
C Updated:
C  Error status
       integer    status
C
C The frame is plotted.
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C box options and scaling for pips
       character*10   opt_x, opt_y
       real*4         x_scl, y_scl


C check status on entry
       if (status.ne.0) return

C check for map
       if (.not.pframe_opt) return

C set style options for frame box
       call graphic_set_line_opt(frame_line_style,status)

C switch to deffered update state
       call pgbbuf

C  box
       opt_x = ' '
       opt_y = ' '
       x_scl = 0.0
       y_scl = 0.0
       call graphic_set_text_opt( frame_text_style, status )
       call graphic_set_line_opt( frame_line_style, status )
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

C plot the box
       call pgbox(opt_x,x_scl,0,opt_y,y_scl,0)
C add pips
       if (pips_opt) then
         call plot_dopips(status)
       endif

C add grid
       if (grid_opt) then
         call plot_dogrid(status)
       endif

C return to immediate update state
       call pgebuf

C report error message if required
       call cmd_err(status,'PLOT_DOFRAME',' ')

       end
