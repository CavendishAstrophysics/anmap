*+ graphic_copy_arrow_opt

       subroutine graphic_copy_arrow_opt( arrow_style_from,
     *                                    arrow_style_to, s )
C      -----------------------------------------------------
C
C Copy the arrow_style structure from arrow_style_from to arrow_style_to
C
C Given:
C   structure to copy arrow_style from
       integer    arrow_style_from(*)
C Returned:
C   structure to copy graphic to
       integer    arrow_style_to(*)
C
C Updated:
C   error status
       integer    s
C
C Copy the arrow_style structure between the two arrays used to hold the
C structure.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return
       do l=1,len_arrow_opt
          arrow_style_to(l) = arrow_style_from(l)
       enddo

       end


