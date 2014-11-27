       subroutine pgsetvp( x1, x2, y1, y2 )
C      ------------------------------------
C
C Set the pgplot viewport for an open device
C
       real*4     x1, x2, y1, y2
C
C-
       character*6 state
       integer     il

       call pgqinf( 'STATE', state, il)
       if (state(1:1).eq.'O') then
         call pgsvp( x1, x2, y1, y2)
       endif
       end


       subroutine pgsetwin( x1, x2, y1, y2 )
C      -------------------------------------
C
C Set the pgplot window for an open device
C
       real*4     x1, x2, y1, y2
C
C-
       character*6 state
       integer     il

       call pgqinf( 'STATE', state, il)
       if (state(1:1).eq.'O') then
         call pgswin( x1, x2, y1, y2)
       endif
       end

