C
C
*+ graphic_delete_text

       subroutine graphic_delete_text( ntb, ib, tb, itext, s )
C      -------------------------------------------------------
C
C Delete the text associated with itext from the buffer and free space
C
       include '../include/plt_buffer_defn.inc'
C
C Given:
C   number of buffers
       integer         ntb
C   buffer of string pointers
       integer         ib(ntb)
C   buffers of strings
       character*(ltb) tb(ntb)
C   text-string buffer to delete
       integer    itext
C
C Updated:
C   error status
       integer    s
C
C The text associated with the buffer pointer itext is deleted and the
C associated space in the buffer is freed.
C-
C local variables
       integer     n

C check status on entry
       if (s.ne.0) return

C loop through the available buffers and find matching buffers
       do n = 1,ntb
         if (ib(n).eq.itext) then
           ib(n) = 0
         endif
       enddo
       call cmd_err(s,'graphic_delete_text',' ')
       end
