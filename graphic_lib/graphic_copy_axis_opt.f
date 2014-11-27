*+ graphic_copy_axis_opt

       subroutine graphic_copy_axis_opt( axis_from, axis_to, s )
C      ---------------------------------------------------------
C
C Copy the axis structure from axis_from to axis_to
C
C Given:
C   structure to copy axis from
       integer    axis_from(*)
C Returned:
C   structure to copy axis to
       integer    axis_to(*)
C
C Updated:
C   error status
       integer    s
C
C Copy the axis structure between the two arrays used to hold the
C structure.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return
       do l=1,len_axis_opt
          axis_to(l) = axis_from(l)
       enddo

       end

