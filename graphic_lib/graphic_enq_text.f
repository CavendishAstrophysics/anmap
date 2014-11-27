C
C
*+ graphic_enq_text

       subroutine graphic_enq_text( ntb, ib, tb, itext, string, s )
C      ------------------------------------------------------------
C
C Enquire the text associated with itext
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
C   text-string buffer
       integer    itext
C Returned:
C   text string to loaded from text buffer
       character*(*)  string
C
C Updated:
C   error status
       integer    s
C
C The text associated with the buffer pointer itext is returned -- note
C the space in the text buffer is not freed with this routine.
C-
C local variables
       integer     nb, n

C check status on entry
       if (s.ne.0) return

C loop through the available buffers and find matching buffers
       string = ' '
       nb = 0
       do n = 1,ntb
         if (ib(n).eq.itext) then
           nb = nb + 1
           if (nb.eq.1) then
             string = tb(n)
           else
             string = string(1:(nb-1)*ltb)//tb(n)
           endif
         endif
       enddo
       call cmd_err(s,'graphic_enq_text',' ')
       end
