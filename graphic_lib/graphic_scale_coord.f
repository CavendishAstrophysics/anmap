*+ graphic_scale_coord

       subroutine graphic_scale_coord( s )
C      -----------------------------------
C
C Scale the current coordinate system to the entire plot area
C
C Updated:
C   error status
       integer    s
C
C The current coordinate system -- window and view-port -- is scaled up so
C that the entire plot area is available to be used using the current
C coordinate system.
C-
       include '/mrao/include/chrlib_functions.inc'

C local variables
       character  value*10
       integer    len_val
       real*4 u1, u2, v1, v2, x1, x2, y1, y2, xr, yr

       if (s.ne.0) return

C push the coordinates
       call graphic_push_coord(s)

C find coordinates and scale
       call pgqinf( 'STATE', value, len_val )
       if (chr_cmatch(value(1:len_val),'OPEN')) then
         call pgqvp(0,u1,u2,v1,v2)
         call pgqwin(x1,x2,y1,y2)
         if (u1.ne.u2 .and. v1.ne.v2) then
           xr = (x2-x1)/(u2-u1)
           yr = (y2-y1)/(v2-v1)
           x1 = x1 - xr*u1
           x2 = x2 + xr*(1.0-u2)
           y1 = y1 - yr*v1
           y2 = y2 + yr*(1.0-v2)
         endif

C reset coordinate system
         call pgsetvp(0.0,1.0,0.0,1.0)
         call pgsetwin(x1,x2,y1,y2)
       endif

       call cmd_err(s,'graphic_scale_coord',' ')
       end
