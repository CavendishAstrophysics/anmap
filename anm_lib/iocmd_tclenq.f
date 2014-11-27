*+ iocmd_tclenq

       subroutine iocmd_tclenq( interp, var, el, val, s )
C      --------------------------------------------------
C
C Set an integer value into a tcl variable
C
C Given:
C   pointer to interpreter
       integer    interp(*)
C
C   variable name
       character  var*(*)
C   element name if var is an array
       character  el*(*)
C Returned:
C   value enquired 
       character  val*(*)
C Updated:
C   error status
       integer    s
C
C-
       include '/mrao/include/chrlib_functions.inc'
       character   name*64, str*256

       if (chr_lenb(el).gt.0) then
         name = var(1:chr_lenb(var))//
     *          '('//el(1:chr_lenb(el))//')'//char(0)
       else
         name = var(1:chr_lenb(var))//char(0)
       endif
       call iocmd_tclenqv( interp, name, str )
       val = str(1:chr_lenb(str))
       end



