*+ graphic_show_arrow_opt

       subroutine graphic_show_arrow_opt( opt, s )
C      ------------------------------------------
C
C Display information for the supplied arrow-style options
C
C Given:
C  definition
       integer    opt(*)
C
C Updated:
C  error status
       integer    s
C
C Information about the supplied arrow-style is output
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer    iout

C check status on entry
       if (s.ne.0) return

C find output unit
       call io_enqout( iout )

C copy object object to standard definition
       call graphic_copy_arrow_opt (opt, arrow_opt, s )

C output information to unit iout
       write(iout,10) arrow_opt_set,line_name(arrow_opt_style),
     *                colour_name(arrow_opt_colour),arrow_opt_width,
     *                arrow_opt_size
10     format(' ARROW: Set=',i1,' Style=',A,' Colour=',A,
     *        ' Width=',I2,'Size=',F6.1)

       end

