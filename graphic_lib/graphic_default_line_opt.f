*+ graphic_default_line_opt

       subroutine graphic_default_line_opt( opt, s )
C      ---------------------------------------------
C
C Setup default line options
C
C Given:
C   line style structure
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

C copy structure for use
       do l = 1,len_line_opt
         line_opt(l) = opt(l)
       enddo
       line_opt_style = line_solid
       line_opt_colour = colour_foreground
       line_opt_width = 1
       line_opt_set = 1
       do l = 1,len_line_opt
         opt(l) = line_opt(l)
       enddo
       end

