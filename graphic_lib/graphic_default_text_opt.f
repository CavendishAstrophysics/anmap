*+ graphic_set_text_opt

       subroutine graphic_default_text_opt( opt, s )
C      ---------------------------------------------
C
C Setup default text options
C
C Given:
C   text style structure
       integer    opt(*)
C
C Updated:
C   error status
       integer    s
C
C Setup default text style options.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return

C copy structure for use
       do l = 1,len_text_opt
         text_opt(l) = opt(l)
       enddo
       text_opt_font = font_normal
       text_opt_colour = colour_foreground
       text_opt_size = 1
       text_opt_width = 1
       text_opt_set = 1
       do l = 1,len_text_opt
         opt(l) = text_opt(l)
       enddo
       end

