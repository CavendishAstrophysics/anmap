*+ graphic_pars_axis

       subroutine graphic_pars_axis( opt, pr, s )
C      ------------------------------------------
C
C Define command-language parameters for axis-option structure
C
C Given:
C    structure
       integer    opt
C    string prefix (usually x or y)
       character  pr*(*)
C
C Updated:
C    error status
       integer    s
C
C-
       include '../include/plt_basic_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

       character  text*80, name*30
       integer    lt, lpr

C check status on entry
       if (s.ne.0) return

C copy structure
       call graphic_copy_axis_opt( opt, axis_opt, s )
       lpr = chr_lenb(pr)

C setup parameters
       call chr_chitoc( axis_opt_set, text, lt )
       name = '%'//pr(1:lpr)//'axis-status'
       call cmd_setlocal(name,text(1:lt), s )
       name = '%'//pr(1:lpr)//'axis-axis'
       if (axis_opt_axis) then
         call cmd_setlocal(name,'1',s)
       else
         call cmd_setlocal(name,'0',s)
       endif
       name = '%'//pr(1:lpr)//'axis-grid'
       if (axis_opt_grid) then
         call cmd_setlocal(name,'1',s)
       else
         call cmd_setlocal(name,'0',s)
       endif
       call chr_chitoc( axis_opt_label, text, lt )
       name = '%'//pr(1:lpr)//'axis-label'
       call cmd_setlocal(name,text(1:lt), s )
       call chr_chitoc( axis_opt_side, text, lt )
       name = '%'//pr(1:lpr)//'axis-side'
       call cmd_setlocal(name,text(1:lt), s )
       name = '%'//pr(1:lpr)//'axis-labelr'
       if (axis_opt_labelr) then
         call cmd_setlocal(name,'1',s)
       else
         call cmd_setlocal(name,'0',s)
       endif
       name = '%'//pr(1:lpr)//'axis-tickl'
       if (axis_opt_tickl) then
         call cmd_setlocal(name,'1',s)
       else
         call cmd_setlocal(name,'0',s)
       endif
       name = '%'//pr(1:lpr)//'axis-ticks'
       if (axis_opt_ticks) then
         call cmd_setlocal(name,'1',s)
       else
         call cmd_setlocal(name,'0',s)
       endif
       name = '%'//pr(1:lpr)//'axis-tickt'
       call chr_chitoc( axis_opt_tickt, text, lt )
       call cmd_setlocal(name,text(1:lt), s )
       call chr_chitoc( axis_opt_ntick, text, lt )
       name = '%'//pr(1:lpr)//'axis-ntick'
       call cmd_setlocal(name,text(1:lt), s )
       call chr_chrtoc( axis_opt_xtick, text, lt )
       name = '%'//pr(1:lpr)//'axis-xtick'
       call cmd_setlocal(name,text(1:lt), s )
       name = '%'//pr(1:lpr)//'axis-log'
       if (axis_opt_log) then
         call cmd_setlocal(name,'1',s)
       else
         call cmd_setlocal(name,'0',s)
       endif
       call graphic_pars_line_opt(axis_opt_line_axis,s)
       call cmd_err(s,'axis_pars_axis',' ')
       end
