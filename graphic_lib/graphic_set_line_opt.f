*+ graphic_set_line_opt

       subroutine graphic_set_line_opt( opt, s )
C      -----------------------------------------
C
C Setup the line options specified by the supplied structure
C
C Given:
C   line style structure
       integer    opt(*)
C
C Updated:
C   error status
       integer    s
C
C Setup the line style options specified by the supplied structure,
C calls to appropriate graphics routines are made to specify the
C options.
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

C if not setup then use standard defaults
       if (line_opt_set.ne.1) then
         call pgsls( line_solid )
         call pgslw( 1 )
         call pgsci( colour_foreground )

       else
         call pgsls( line_opt_style )
         call pgslw( line_opt_width )
         call pgsci( line_opt_colour )

       endif
       end

