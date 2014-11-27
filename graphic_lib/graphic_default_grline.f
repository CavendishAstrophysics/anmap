*+ graphic_default_grline

       subroutine graphic_default_grline( opt, code, s )
C      -------------------------------------------------
C
C Setup default grline (graphic line) options
C
C Given:
C    grline structure
       integer    opt(*)
C    code to specify type of setting up
C     = 0   plot
C     = 1   style options
C     = 2   file options
C     = 3   everything
       integer    code
C
C Updated:
C   error status
       integer    s
C
C Setup default graphic line (grline) options.
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return

C copy from structure
       do l = 1,len_grline
         grline(l) = opt(l)
       enddo
C setup options (see structure definition)

       if (code.eq.2 .or. code.eq.3) then
         grline_x_file = 0
         grline_y_file = 0
         grline_x_col = 1
         grline_y_col = 2
         grline_ex_file = 0
         grline_ex_col = 3
         grline_ey_opt = 0
         grline_ey_file = 0
         grline_ey_col = 3
       endif
       if (code.eq.1 .or. code.eq.3) then
         grline_symbol = 0
         grline_type = 1
         grline_offset_x = 0.0
         grline_offset_y = 0.0
         grline_scale_x = 1.0
         grline_scale_y = 1.0
         grline_auto_x = .false.
         grline_auto_y = .false.
         grline_ex_opt = 0
         grline_ey_opt = 0
         grline_ex_top = 1.0
         grline_ey_top = 1.0
         grline_x_log = .false.
         grline_y_log = .false.
         call graphic_default_line_opt(grline_line_opt,s)
         call graphic_default_line_opt(grline_error_opt,s)
         call graphic_default_text_opt(grline_text_opt,s)
         grline_key_opt = 1
         grline_key_text = 0
       endif
       if (code.eq.0) then
         grline_status = abs(grline_status)
       endif

C copy to structure
       do l = 1,len_grline
         opt(l) = grline(l)
       enddo
       call cmd_err(s,'graphic_default_grline',' ')

       end
