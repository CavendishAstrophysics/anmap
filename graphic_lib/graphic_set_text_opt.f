*+ graphic_set_text_opt

       subroutine graphic_set_text_opt( opt, s )
C      -----------------------------------------
C
C Setup the text options specified by the supplied structure
C
C Given:
C   text style structure
       integer    opt(*)
C
C Updated:
C   error status
       integer    s
C
C Setup the text style options specified by the supplied structure,
C calls to appropriate graphics routines are made to specify the
C options.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l
       real*4      x1, x2, y1, y2, scale

C check status on entry
       if (s.ne.0) return

C copy structure for use
       do l = 1,len_text_opt
         text_opt(l) = opt(l)
       enddo

C if not setup then use standard defaults
       if (text_opt_set.ne.1) then
         call pgscf( font_normal )
         call pgsch( 1.0 )
         call pgsci( colour_foreground )
         call pgslw( 1 )

       else
         if (text_opt_size.lt.0.0) then
C .. provide window-scaled text
           call pgqvp(0,x1,x2,y1,y2)
           scale = min((x2-x1),(y2-y1))
           call pgsch( -text_opt_size*scale)
         else
           call pgsch( text_opt_size )
         endif
         call pgscf( text_opt_font )
         call pgsci( text_opt_colour )
         call pgslw( text_opt_width )

       endif
       end

