*+ graphic_get_line_opt

       subroutine graphic_get_line_opt( opt, s )
C      -----------------------------------------
C
C Read a description for line style options from the user
C
C Updated:
C   line options structure
       integer   opt(*)
C   error status
       integer   s
C
C The user is prompted for all the elements of a line style
C definition.
C-

       include '../include/plt_basic_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer       l, lc, istat
       logical       use_default
       character     def*20, colour*20

C check status on entry
       if ( s.ne.0 ) return

C copy the structure to the standard definition
       do l = 1,len_line_opt
         line_opt(l) = opt(l)
       enddo

C indicate that this structure has been initialised
       use_default = line_opt_set.eq.0
       line_opt_set = 1

C find:
C  .. style
       if (use_default) then
         def = '1'
       else
         def = '*'
       endif
       call io_geti( 'Line-style : ',def,line_opt_style,s)
       if (s.ne.0) goto 999
       if (line_opt_style.lt.line_min .or.
     *     line_opt_style.gt.line_max     ) then
         call cmd_wrerr('LINE-STYLE','Illegal line style')
         line_opt_style = 1
       endif
C  .. width
       if (use_default) then
         def = '1'
       else
         def = '*'
       endif
       call io_geti( 'Line-width : ',def,line_opt_width,s)
       if (s.ne.0) goto 999
       if (line_opt_width.le.0) then
         call cmd_wrerr('LINE-STYLE',
     *                  'Illegal line width; must be >= 0')
         line_opt_width = 1
       endif
C  .. colour
       if (use_default) then
         def = 'white'
       else
         def = colour_name(line_opt_colour)
       endif
       call io_getwrd( 'Line-colour : ',def,colour,lc,s)
       if (s.ne.0) goto 999
       line_opt_colour = -1
       do l=colour_min,colour_max
         if (chr_cmatch(colour(1:lc),colour_name(l))) then
           line_opt_colour = l
         endif
       enddo
       if (line_opt_colour.eq.-1) then
         istat = 0
         call chr_chctoi( colour, line_opt_colour, istat)
         if (istat.ne.0) line_opt_colour = 1
       endif
       if (line_opt_colour.lt.colour_min .or.
     *     line_opt_colour.gt.colour_max     ) then
         call cmd_wrerr('LINE-STYLE','Illegal colour index')
         line_opt_colour = 1
       endif

       if (s.eq.0) then
         line_opt_set = 1
C .. copy structure to output
         do l=1,len_line_opt
           opt(l) = line_opt(l)
         enddo
       endif
999    continue
       call cmd_err( s, 'graphic_get_line_opt', ' ')

       end
