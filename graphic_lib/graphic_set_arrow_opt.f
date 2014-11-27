*+ graphic_set_arrow_opt

       subroutine graphic_set_arrow_opt( opt, s )
C      ------------------------------------------
C
C Setup the arrow options specified by the supplied structure
C
C Given:
C   arrow style structure
       integer    opt(*)
C
C Updated:
C   error status
       integer    s
C
C Setup the arrow style options specified by the supplied structure,
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
       do l = 1,len_arrow_opt
         arrow_opt(l) = opt(l)
       enddo

C if not setup then use standard defaults
       if (arrow_opt_set.ne.1) then
         call pgsls( line_solid )
         call pgslw( 1 )
         call pgsci( colour_foreground )
         call pgsch( 1.0 )

       else
         if (arrow_opt_size.lt.0.0) then
           call pgqvp(0,x1,x2,y1,y2)
           scale = min((x2-x1),(y2-y1))
           call pgsch( -arrow_opt_size*scale)
         else
           call pgsch( arrow_opt_size )
         endif
         call pgsls( arrow_opt_style )
         call pgslw( arrow_opt_width )
         call pgsci( arrow_opt_colour )

       endif
       end


