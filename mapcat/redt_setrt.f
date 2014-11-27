C
C
*+ redt_setrt

       subroutine redt_setrt(minirt,status)
C      ------------------------------------
C
C Set the current value of the mini redtape
C
C Given:
C    mini redtape
       integer         minirt(*)
C Returned:
C    status word
       integer         status
C
C The current value of the mini redtape is reset
C-
       include 'mapcat_stack.inc'
       integer   i

       if (status.ne.0) return
       do i=1,8
         current_minirt(i) = minirt(i)
       end do
       call mapcat_err(status,'redt_setrt',' ')
       end
