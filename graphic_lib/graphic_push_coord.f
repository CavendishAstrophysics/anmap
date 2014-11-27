*+ graphic_push_coord

       subroutine graphic_push_coord( s )
C      ----------------------------------
C
C Push the current coordinate system onto the coordinate system stack
C
C Updated:
C   error status
       integer    s
C
C The current coordinate system -- window and view-port -- is put onto
C the coordinate stack.
C-

       include '../include/plt_coord_stack.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer        m,n,len_val
       character      value*10

       if (s.ne.0) return

C find coordinates and scale
       call pgqinf( 'STATE', value, len_val )
       if (chr_cmatch(value(1:len_val),'OPEN')) then
         do n=len_coord_stack,2,-1
           do m=1,8
             coord_stack(m,n) = coord_stack(m,n-1)
           enddo
         enddo
        call pgqvp(0,coord_stack(1,1),coord_stack(2,1),
     *               coord_stack(3,1),coord_stack(4,1))
         call pgqwin(coord_stack(5,1),coord_stack(6,1),
     *               coord_stack(7,1),coord_stack(8,1))
       endif
       call cmd_err(s,'graphic_push_coord',' ')
       end
