*+ graphic_show_line_opt

       subroutine graphic_show_line_opt( opt, s )
C      ------------------------------------------
C
C Display information for the supplied line-style options
C
C Given:
C  definition
       integer    opt(*)
C
C Updated:
C  error status
       integer    s
C
C Information about the supplied line-style is output
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer    iout

C check status on entry
       if (s.ne.0) return

C find output unit
       call io_enqout( iout )

C copy object object to standard definition
       call graphic_copy_line_opt (opt, line_opt, s )

C output information to unit iout
       write(iout,10) line_opt_set,line_name(line_opt_style),
     *                colour_name(line_opt_colour),line_opt_width
10     format(' LINE: Set=',i1,' Style=',A,' Colour=',A,' Width=',I2)

       end
