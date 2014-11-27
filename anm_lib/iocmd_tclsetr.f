*+ iocmd_tclsetr

       subroutine iocmd_tclsetr( interp, val, n, var, el, s )
C      ------------------------------------------------------
C
C Set a real value into a tcl variable
C
C Given:
C   pointer to interpreter
       integer    interp(*)
C   value(s) to set and number of values
       real*4     val(*)
       integer    n
C   variable name
       character  var*(*)
C   element name if var is an array
       character  el*(*)
C
C Updated:
C   error status
       integer    s
C
C-
       include '/mrao/include/chrlib_functions.inc'
       character   name*64, str*256
       integer     i, l

       if (chr_lenb(el).gt.0) then
         name = var(1:chr_lenb(var))//
     *          '('//el(1:chr_lenb(el))//')'//char(0)
       else
         name = var(1:chr_lenb(var))//char(0)
       endif
       do i=1,n
         call chr_chrtoc( val(i), str, l)
         str(l+1:l+1) = char(0)
         if (i.eq.1) then
           call iocmd_tclset( interp, name, str )
         else
           call iocmd_tclapplist( interp, name, str )
         endif
       enddo

       end

