*+ graphic_show_text_opt

       subroutine graphic_show_text_opt( opt, s )
C      ------------------------------------------
C
C Display information for the supplied text-style options
C
C Given:
C  definition
       integer    opt(*)
C
C Updated:
C  error status
       integer    s
C
C Information about the supplied text-style is output
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer    iout

C check status on entry
       if (s.ne.0) return

C find output unit
       call io_enqout( iout )

C copy object text_opt to standard definition
       call graphic_copy_text_opt (opt, text_opt, s )

C output information to unit iout
       write(iout,10) text_opt_set,font_name(text_opt_font),
     *                colour_name(text_opt_colour),
     *                text_opt_size,text_opt_width
10     format(' LINE: Set=',i1,' Font=',A,' Colour=',A,
     *        ' Size=',1PE9.1,' Width=',I2)

       end
