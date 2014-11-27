*+ graphic_copy_frame_opt

       subroutine graphic_copy_frame_opt( frame_from, frame_to, s )
C      ------------------------------------------------------------
C
C Copy the frame structure from frame_from to frame_to
C
C Given:
C   structure to copy frame from
       integer    frame_from(*)
C Returned:
C   structure to copy frame to
       integer    frame_to(*)
C
C Updated:
C   error status
       integer    s
C
C Copy the frame structure between the two arrays used to hold the
C structure.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return
       do l=1,len_frame_opt
          frame_to(l) = frame_from(l)
       enddo

       end

