*+ graphic_copy_line_opt

       subroutine graphic_copy_line_opt( line_style_from,
     *                                    line_style_to, s )
C      -----------------------------------------------------
C
C Copy the line_style structure from line_style_from to line_style_to
C
C Given:
C   structure to copy line_style from
       integer    line_style_from(*)
C Returned:
C   structure to copy graphic to
       integer    line_style_to(*)
C
C Updated:
C   error status
       integer    s
C
C Copy the line_style structure between the two arrays used to hold the
C structure.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return
       do l=1,len_line_opt
          line_style_to(l) = line_style_from(l)
       enddo

       end

