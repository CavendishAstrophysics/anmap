*+ iocmd_enqvar

       subroutine iocmd_enqvar( name, value, s )
C      -----------------------------------------
C
C Enquire a value from the name table
C
C Given:
C   name of variable
       character*(*)     name
C Returned:
C   value for the variable
       character*(*)     value
C
C Updated:
C   error status
       integer           s
C
C A variable is added to or updated in the name table.
C
C-
       integer  ln, lv, n

       include './iocmd_vars.inc'
       include '/mrao/include/chrlib_functions.inc'

       ln = chr_lenb(name)
       do n=1,nvars
         lv = chr_lenb(iocmd_var(n))
         if (name(1:ln).eq.iocmd_var(n)(1:lv)) then
           value = iocmd_text(n) 
           goto 10
         endif
       enddo
       value = ' '
       call io_enqvar( name, value, s )
  10   continue
       call iocmd_err( s, 'iocmd_setvar', ' ' )
       end
         
