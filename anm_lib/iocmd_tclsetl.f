*+ iocmd_tclsetl

       subroutine iocmd_tclsetl( interp, val, var, el, s )
C      ---------------------------------------------------
C
C Set a logical value into a tcl variable
C
C Given:
C   pointer to interpreter
       integer    interp(*)
C   value to set
       logical    val
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

       if (val) then
         str = '1'//char(0)
       else
         str = '0'//char(0)
       endif
       if (chr_lenb(el).gt.0) then
         name = var(1:chr_lenb(var))//
     *          '('//el(1:chr_lenb(el))//')'//char(0)
       else
         name = var(1:chr_lenb(var))//char(0)
       endif
       call iocmd_tclset( interp, name, str )

       end


