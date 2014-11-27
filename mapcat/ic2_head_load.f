*+ ic2_head_load

       subroutine ic2_head_load(imap,status)
C      -------------------------------------
C
C Load header for IMAP and make it current
C
C Given:
C    Map entry
       integer      imap
C Returned:
C    Status word
       integer      status
C
C Load the header for map IMAP and make it the current header.
C Accessing header items is then possible provided the.
C MAPLIB common blocks are included in the user subprogram.
C Standard projection parameters are set by calling STPROJ.
C
*-
C
       call redt_load(imap,status)
       end
