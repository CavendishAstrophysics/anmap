*+ graphic_default_arrow_opt

       subroutine graphic_default_arrow_opt( opt, s )
C      ----------------------------------------------
C
C Setup default arrow options
C
C Given:
C   arrow style structure
       integer    opt(*)
C
C Updated:
C   error status
       integer    s
C
C Setup default arrow style options.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return

C copy structure for use
       do l = 1,len_arrow_opt
         arrow_opt(l) = opt(l)
       enddo
       arrow_opt_style = line_solid
       arrow_opt_colour = colour_foreground
       arrow_opt_width = 1
       arrow_opt_set = 1
       arrow_opt_size = 1.0
       do l = 1,len_arrow_opt
         opt(l) = arrow_opt(l)
       enddo
       end


