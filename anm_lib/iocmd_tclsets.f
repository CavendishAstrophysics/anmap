*+ iocmd_tclsets

       subroutine iocmd_tclsets( interp, val, var, el, s )
C      ---------------------------------------------------
C
C Set a character value into a tcl variable
C
C Given:
C   pointer to interpreter
       integer    interp(*)
C   value to set
       character  val*(*)
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
       character   name*64, str*2048

       str = val(1:chr_lenb(val))//char(0)
       if (chr_lenb(el).gt.0) then
         name = var(1:chr_lenb(var))//
     *          '('//el(1:chr_lenb(el))//')'//char(0)
       else
         name = var(1:chr_lenb(var))//char(0)
       endif
       call iocmd_tcllset( interp, name, str )

       end



