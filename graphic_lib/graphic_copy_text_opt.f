*+ graphic_copy_text_opt

       subroutine graphic_copy_text_opt( text_style_from,
     *                                    text_style_to, s )
C      -----------------------------------------------------
C
C Copy the text_style structure from text_style_from to text_style_to
C
C Given:
C   structure to copy text_style from
       integer    text_style_from(*)
C Returned:
C   structure to copy graphic to
       integer    text_style_to(*)
C
C Updated:
C   error status
       integer    s
C
C Copy the text_style structure between the two arrays used to hold the
C structure.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return
       do l=1,len_text_opt
          text_style_to(l) = text_style_from(l)
       enddo

       end

