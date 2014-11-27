C
C
*+ graphic_put_text

       subroutine graphic_put_text( ntb, ib, tb, string, itext, s )
C      ------------------------------------------------------------
C
C Find the next available text-string buffer and save the text
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
C   text string to load into text buffer
       character*(*)  string
C Returned:
C   found text-string buffer
       integer    itext
C
C Updated:
C   error status
       integer    s
C
C Find the next available text-string buffer -- if no more buffers are
C available then return an error status.  If there is an available
C buffer then the text string is loaded into the buffer.  Long strings
C may overlap more than one buffer.
C-
       include '/mrao/include/chrlib_functions.inc'
       include '../include/plt_error_defn.inc'

C local variables
       integer     ls, nb, n, nn, i1,i2, lb(256)

C check status on entry
       if (s.ne.0) return

C calculate the number of buffers needed to hold this string
       ls = chr_lenb(string)
       if (ls.eq.0) then
C .. sepcial case zero-length text
         itext = 0
         return
       endif
       nb = (ls-1)/ltb + 1

C loop through the available buffers and make sure there are suffient free
       nn = 0
       n = 1
       do while (n.le.ntb .and. nn.lt.nb)
         if (ib(n).eq.0) then
           nn = nn + 1
           lb(nn) = n
         endif
         n = n + 1
       enddo

C load buffers or return an error message
       if (nn.ge.nb) then
         itext = lb(1)
         do n = 1,nb
           ib(lb(n)) = lb(1)
           i1 = ltb*(n-1) + 1
           i2 = min(ltb*n,ls)
           tb(lb(n)) = string(i1:i2)
         enddo
       else
         s = no_itext
         print *,(ib(n), n=1,ntb)
       endif
       if (s.ne.no_itext) then
         call cmd_err(s,'graphic_put_text',' ')
       endif
       end
