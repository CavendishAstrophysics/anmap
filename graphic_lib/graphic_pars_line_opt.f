*+ graphic_pars_line_opt

       subroutine graphic_pars_line_opt( opt, s )
C      ------------------------------------------
C
C Define command-language parameters for line-option structure
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

       character  text*80
       integer    lt

C check status on entry
       if (s.ne.0) return

C copy structure
       call graphic_copy_line_opt( opt, line_opt, s )

C setup parameters
       call chr_chitoc( line_opt_set, text, lt )
       call cmd_setlocal('%line-status',text(1:lt), s )
       call cmd_setlocal('%line-style',line_name(line_opt_style),s)
       call cmd_setlocal('%line-colour',colour_name(line_opt_colour),s)
       call chr_chitoc( line_opt_width, text, lt )
       call cmd_setlocal('%line-width',text(1:lt), s )
       call cmd_err(s,'graphic_pars_line_opt',' ')
       end
