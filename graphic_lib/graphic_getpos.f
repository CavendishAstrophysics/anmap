C
C
*+ graphic_getpos

       subroutine graphic_getpos( xy, s )
C      ----------------------------------
C
C Read a position from the plot device using the current coordinates
C
C Returned
C  the XY position
       real*4    xy(2)
C
C Updated:
C  error status
       integer   s
C
C The cursur is displayed and the position in the current coordinate
C system is returned.
C-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_error_defn.inc'

C local variables
       logical      cursor_available
       real*4       x, y
       character    char*1
       integer      pgcurse

       if (s.ne.0) return

       call pgqcur(cursor_available)
       if (.not.cursor_available) then
         call cmd_wrerr('GRAPHIC-CURSOR',
     *                  'Device does not support cursor input')
         s = ill_curse
         return
       end if
       if (.not.plot_open) then
         call cmd_wrerr('GRAPHIC-CURSOR',
     *                  'Device is not open for cursor input')
         s = ill_curse
         return
       endif
       s = pgcurse(x,y,char) - 1
       call chr_chucas(char)
       if (char.eq.'Q') then
           call cmd_wrerr('GRAPHIC-CURSOR','Input abandoned')
           s = ill_inter
           return
       elseif (s.ne.0) then
           call cmd_wrerr('GRAPHIC-CURSOR','Error reading cursor')
           s = ill_curse
           return
       end if
       xy(1) = x
       xy(2) = y
       end
