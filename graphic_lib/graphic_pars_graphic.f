*+ graphic_pars_graphic

       subroutine graphic_pars_graphic( opt, s )
C      -----------------------------------------
C
C Define command-language parameters for graphic-option structure
C
C Given:
C    structure
       integer    opt
C
C Updated:
C    error status
       integer    s
C
C-
       include '../include/plt_basic_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

       character  text*80, name*14
       integer    lt, n

C check status on entry
       if (s.ne.0) return

C copy structure
       call graphic_copy_graphic( opt, graphic, s )

C setup parameters
       call chr_chitoc( graphic_status, text, lt )
       call cmd_setlocal('%graphic-status',text(1:lt), s )
       call cmd_setlocal('%graphic-type',graphic_name(graphic_type),s)
       if (graphic_transparent) then
         call cmd_setlocal('%graphic-transparent','1',s)
       else
         call cmd_setlocal('%graphic-transparent','0',s)
       endif
       call chr_chitoc( graphic_index, text, lt )
       call cmd_setlocal('%graphic-index',text(1:lt), s )
       call chr_chitoc( graphic_depth, text, lt )
       call cmd_setlocal('%graphic-depth',text(1:lt), s )
       call chr_chitoc( graphic_fill_opt, text, lt )
       call cmd_setlocal('%graphic-fill',text(1:lt), s )
       call cmd_defparam(.false.,'%graphic-vp','real',4,s)
       do n=1,4
         write(name,'(''%graphic-vp{'',i1,''}'')') n
         call chr_chrtoc( graphic_view_port(n), text, lt )
         call cmd_setlocal(name,text(1:lt), s )
       enddo
       call cmd_err(s,'graphic_pars_graphic',' ')
       end
