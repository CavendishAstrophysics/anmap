*+ iocmd_setvar

       subroutine iocmd_setvar( name, value, s )
C      -----------------------------------------
C
C Set a value into the name table
C
C Given:
C   name of variable
       character*(*)     name
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
           iocmd_text(n) = value
           goto 10
         endif
       enddo
       do n=1,nvars
         lv = chr_lenb(iocmd_var(n))
         if (lv.eq.0) then
           iocmd_var(n) = name
           iocmd_text(n) = value
           goto 10
         endif
       enddo
  10   continue
       call iocmd_err( s, 'iocmd_setvar', ' ' )
       end
         

