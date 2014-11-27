*+ graphic_copy_graphic

       subroutine graphic_copy_graphic( graphic_from, graphic_to, s )
C      --------------------------------------------------------------
C
C Copy the graphic structure from graphic_from to graphic_to
C
C Given:
C   structure to copy graphic from
       integer    graphic_from(*)
C Returned:
C   structure to copy graphic to
       integer    graphic_to(*)
C
C Updated:
C   error status
       integer    s
C
C Copy the graphic structure between the two arrays used to hold the
C structure.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return
       do l=1,len_graphic
          graphic_to(l) = graphic_from(l)
       enddo

       end

