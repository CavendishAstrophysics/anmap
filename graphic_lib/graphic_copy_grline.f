*+ graphic_copy_grline

       subroutine graphic_copy_grline( grline_from, grline_to, s )
C      -----------------------------------------------------------
C
C Copy the grline structure from grline_from to grline_to
C
C Given:
C   structure to copy grline from
       integer    grline_from(*)
C Returned:
C   structure to copy graphic to
       integer    grline_to(*)
C
C Updated:
C   error status
       integer    s
C
C Copy the grline structure between the two arrays used to hold the
C structure.
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return
       do l=1,len_grline
          grline_to(l) = grline_from(l)
       enddo

       end

