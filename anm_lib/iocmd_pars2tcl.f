C
C
*+ iocmd_pars2tcl

       subroutine iocmd_pars2tcl(interp,status)
C      ----------------------------------------
C
C List information on procedures and parameters
C
C Given:
C   interpreter data structure
       integer            interp(*)
C Returned:
C   error return code
       integer            status
C
C Information in PARAMETERS or PROCEDURES are listed on the current
C output device
*-
       include '/mrao/include/cmd_lang_param.inc'
       include '/mrao/include/cmd_lang_comms.inc'
       include '/mrao/include/chrlib_functions.inc'

C counters
       integer           n, nn, i, i1, i2, mm
C string variables
       character*256     text, string, var
       character*256     name

C check status on entry
       if (status.ne.0) return

         do n=1,len_list
           name = param_list(n)
           if (name(1:1).eq.'%') then
             string = name(2:256)
           else
             string = name
           endif
           call chr_chlcas( string )
           call chr_chswap( string, '-', '_' )
           name = ' '
           var = string(1:chr_lenb(string))
           string = paramtext_list(n)
           text = string(1:chr_lenb(string))//char(0)
           if (chr_lenb(param_list(n)).gt.0) then
             if (paramtype_list(1,n).eq.paramtype_string) then
               name = var(1:chr_lenb(var)) // char(0)
               call iocmd_tclset( interp, name, text )
             elseif (paramtype_list(1,n).eq.paramtype_char) then
               i1 = paramtype_list(2,n)
               i2 = paramtype_list(2,n) + paramtype_list(3,n) - 1
               text = param_char(i1:i2)//char(0)
               name = var(1:chr_lenb(var)) // char(0)
               call iocmd_tclset( interp, name, text )
             elseif (paramtype_list(1,n).eq.paramtype_integer) then
               mm = 0
               do nn=paramtype_list(2,n),
     *               paramtype_list(2,n)+paramtype_list(3,n)-1
                 mm = mm + 1
                 text = ' '
                 string = ' '
                 call chr_chitoc( param_integer(nn), string, i)
                 text = string(1:i)//char(0)
                 name = var(1:chr_lenb(var)) // char(0)
                 if (mm.eq.1) then
                   call iocmd_tclset( interp, name, text )
                 else
                   call iocmd_tclapplist( interp, name, text )
                 endif
               end do
             elseif (paramtype_list(1,n).eq.paramtype_real) then
               mm = 0
               do nn=paramtype_list(2,n),
     *               paramtype_list(2,n)+paramtype_list(3,n)-1
                 mm = mm + 1
                 text = ' '
                 string = ' '
                 call chr_chrtoc( param_real(nn), string, i)
                 text = string(1:i)//char(0)
                 name = var(1:chr_lenb(var)) // char(0)
                 if (mm.eq.1) then
                   call iocmd_tclset( interp, name, text )
                 else
                   call iocmd_tclapplist( interp, name, text )
                 endif
               end do
             end if
           end if
         enddo
       call cmd_err(status,'iocmd_pars2tcl',' ')
       end






