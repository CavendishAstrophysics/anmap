*+ graphic_default_frame_opt

       subroutine graphic_default_frame_opt( opt, s )
C      ----------------------------------------------
C
C Setup default frame options
C
C Given:
C   frame opt structure
       integer    opt(*)
C
C Updated:
C   error status
       integer    s
C
C Setup default line style options.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return

C setup options (see structure definition)
       axis_opt_set = 1
       axis_opt_axis = .false.
       axis_opt_grid = .false.
       axis_opt_side = 3
       axis_opt_label = 1
       axis_opt_labelr = .false.
       axis_opt_tickl = .true.
       axis_opt_ticks = .true.
       axis_opt_tickt = 1
       axis_opt_xtick = 0.0
       axis_opt_ntick = 0
       axis_opt_log = .false.
       call graphic_default_line_opt(axis_opt_line_axis,s)
       call graphic_default_line_opt(axis_opt_line_grid,s)
       call graphic_default_text_opt(frame_opt_text_opt,s)
       do l =1,len_axis_opt
         frame_opt_x_axis(l) = axis_opt(l)
         frame_opt_y_axis(l) = axis_opt(l)
       enddo
       frame_opt_set = 1

C copy to structure
       do l = 1,len_frame_opt
         opt(l) = frame_opt(l)
       enddo
       call cmd_err(s,'graphic_default_frame_opt',' ')

       end
