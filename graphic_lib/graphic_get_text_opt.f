*+ graphic_get_text_opt

       subroutine graphic_get_text_opt( opt, s )
C      -----------------------------------------
C
C Read a description for text style options from the user
C
C Updated:
C   text options structure
       integer   opt(*)
C   error status
       integer   s
C
C The user is prompted for all the elements of a text style
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
       do l = 1,len_text_opt
         text_opt(l) = opt(l)
       enddo

C indicate that this structure has been initialised
       use_default = text_opt_set.eq.0
       text_opt_set = 1

C find:
C  .. font
       if (use_default) then
         def = '1'
       else
         def = '*'
       endif
       call io_geti( 'Text-font : ',def,text_opt_font,s)
       if (s.ne.0) goto 999
       if (text_opt_font.lt.font_min .or.
     *     text_opt_font.gt.font_max     ) then
         call cmd_wrerr('TEXT-STYLE','Illegal font number')
         text_opt_font = 1
       endif
C  .. size
       if (use_default) then
         def = '1.0'
       else
         def = '*'
       endif
       call io_getr( 'Text-size : ',def,text_opt_size,s)
       if (s.ne.0) goto 999

C  .. colour
       if (use_default) then
         def = 'white'
       else
         def = colour_name(text_opt_colour)
       endif
       call io_getwrd( 'Text-colour : ',def,colour,lc,s)
       if (s.ne.0) goto 999
       text_opt_colour = -1
       do l=colour_min,colour_max
         if (chr_cmatch(colour(1:lc),colour_name(l))) then
           text_opt_colour = l
         endif
       enddo
       if (text_opt_colour.eq.-1) then
         istat = 0
         call chr_chctoi( colour, text_opt_colour, istat)
         if (istat.ne.0) text_opt_colour = 1
       endif
       if (text_opt_colour.lt.colour_min .or.
     *     text_opt_colour.gt.colour_max     ) then
         call cmd_wrerr('TEXT-STYLE','Illegal colour index')
         text_opt_colour = 1
       endif
C  .. width
       if (use_default) then
         def = '1'
       else
         def = '*'
       endif
       call io_geti( 'Text-width : ',def,text_opt_width,s)
       if (s.ne.0) goto 999

       if (s.eq.0) then
         text_opt_set = 1
C .. copy structure to output
         do l=1,len_text_opt
           opt(l) = text_opt(l)
         enddo
       endif
999    continue
       call cmd_err( s, 'graphic_get_text_opt', ' ')

       end
