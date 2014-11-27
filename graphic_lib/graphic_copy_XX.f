*+ graphic_copy_XX

       subroutine graphic_copy_( XX_from, XX_to, s )
C      --------------------------------------------------------------
C
C Copy the XX structure from XX_from to XX_to
C
C Given:
C   structure to copy XX from
       integer    XX_from(*)
C Returned:
C   structure to copy graphic to
       integer    XX_to(*)
C
C Updated:
C   error status
       integer    s
C
C Copy the XX structure between the two arrays used to hold the
C structure.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return
       do l=1,len_XX
          XX_to(l) = XX_from(l)
       enddo

       end

