C
C
*+ graphic_cursor

       subroutine graphic_cursor( s )
C      ------------------------------
C
C Read the current cursor position and return via parameters
C
C Updated:
C   error status
       integer    s
C
C The cursor position is read in the current coordinate system and
C the standard parameters are defined as follows:
C
C   Position in current coordinates:               %x %y
C   Position on device (Normalized coordinates)%   %gr-x %gr-y
C-
       integer      l
       real*4       xy(2), x1, x2, y1, y2, wx1, wx2, wy1, wy2
       character    value*20

       if (s.ne.0) return
       call graphic_getpos(xy,s)
       if (s.eq.0) then
C .. set local coordinates
         call chr_chrtoc(xy(1),value,l)
         call cmd_setlocal('%x',value(1:l),s)
         call chr_chrtoc(xy(2),value,l)
         call cmd_setlocal('%y',value(1:l),s)
C .. work out graphic coordinates
         call pgqvp(0,x1,x2,y1,y2)
         call pgqwin(wx1,wx2,wy1,wy2)
         xy(1) = x1 + (xy(1)-wx1)*(x2-x1)/(wx2-wx1)
         xy(2) = y1 + (xy(2)-wy1)*(y2-y1)/(wy2-wy1)
         call chr_chrtoc(xy(1),value,l)
         call cmd_setlocal('%gr-x',value(1:l),s)
         call chr_chrtoc(xy(2),value,l)
         call cmd_setlocal('%gr-y',value(1:l),s)
       endif
       call cmd_err(s,'graphic_cursor',' ')
       end
