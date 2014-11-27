*+ graphic_pars_grline

       subroutine graphic_pars_grline( opt, s )
C      ----------------------------------------
C
C Define command-language parameters for grline-option structure
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
       include '../include/plt_grline_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

       character  text*80
       integer    lt

C check status on entry
       if (s.ne.0) return

C copy structure
       call graphic_copy_grline( opt, grline, s )

C setup parameters
       call chr_chitoc( grline_status, text, lt )
       call cmd_setlocal('%grline-status',text(1:lt), s )
       call chr_chitoc( grline_symbol, text, lt )
       call cmd_setlocal('%grline-symbol',text(1:lt), s )
       call chr_chitoc( grline_type, text, lt )
       call cmd_setlocal('%grline-type',text(1:lt), s )
       call chr_chitoc( grline_x_col, text, lt )
       call cmd_setlocal('%grline-x-col',text(1:lt), s )
       call chr_chitoc( grline_y_col, text, lt )
       call cmd_setlocal('%grline-y-col',text(1:lt), s )
       call chr_chrtoc( grline_offset_x, text, lt )
       call cmd_setlocal('%grline-x-offset',text(1:lt), s )
       call chr_chrtoc( grline_offset_y, text, lt )
       call cmd_setlocal('%grline-y-offset',text(1:lt), s )
       call chr_chrtoc( grline_scale_x, text, lt )
       call cmd_setlocal('%grline-x-scale',text(1:lt), s )
       call chr_chrtoc( grline_scale_y, text, lt )
       call cmd_setlocal('%grline-y-scale',text(1:lt), s )
       if (grline_x_log) then
         call cmd_setlocal('%grline-x-log','1',s)
       else
         call cmd_setlocal('%grline-x-log','0',s)
       endif
       if (grline_y_log) then
         call cmd_setlocal('%grline-y-log','1',s)
       else
         call cmd_setlocal('%grline-y-log','0',s)
       endif
       if (grline_auto_x) then
         call cmd_setlocal('%grline-x-auto','1',s)
       else
         call cmd_setlocal('%grline-x-auto','0',s)
       endif
       if (grline_auto_y) then
         call cmd_setlocal('%grline-y-auto','1',s)
       else
         call cmd_setlocal('%grline-y-auto','0',s)
       endif
       call chr_chitoc( grline_ex_col, text, lt )
       call cmd_setlocal('%grline-errx-col',text(1:lt), s )
       call chr_chitoc( grline_ey_col, text, lt )
       call cmd_setlocal('%grline-erry-col',text(1:lt), s )
       call chr_chitoc( grline_ex_opt, text, lt )
       call cmd_setlocal('%grline-errx-opt',text(1:lt), s )
       call chr_chitoc( grline_ey_opt, text, lt )
       call cmd_setlocal('%grline-erry-opt',text(1:lt), s )
       call chr_chrtoc( grline_ex_top, text, lt )
       call cmd_setlocal('%grline-errx-top',text(1:lt), s )
       call chr_chrtoc( grline_ey_top, text, lt )
       call cmd_setlocal('%grline-erry-top',text(1:lt), s )
       call graphic_pars_line_opt(grline_line_opt,s)
       call graphic_pars_text_opt(grline_text_opt,s)
       call cmd_err(s,'grline_pars_grline',' ')
       end
