*+ graphic_pop_coord

       subroutine graphic_pop_coord( s )
C      ---------------------------------
C
C Pop the coordinate system stack and reset the coordinate system
C
C Updated:
C   error status
       integer    s
C
C The current coordinate system -- window and view-port -- is put onto
C the coordinate stack.
C-
       integer   m,n

       include '../include/plt_coord_stack.inc'

       if (s.ne.0) return

       call pgsetvp(coord_stack(1,1),coord_stack(2,1),
     *              coord_stack(3,1),coord_stack(4,1))
       call pgsetwin(coord_stack(5,1),coord_stack(6,1),
     *               coord_stack(7,1),coord_stack(8,1))
       do n=len_coord_stack-1,1,-1
         do m=1,8
           coord_stack(m,n) = coord_stack(m,n+1)
         enddo
       enddo


       call cmd_err(s,'graphic_pop_coord',' ')
       end
