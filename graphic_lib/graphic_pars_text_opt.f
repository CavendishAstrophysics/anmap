*+ graphic_pars_text_opt

       subroutine graphic_pars_text_opt( opt, s )
C      ------------------------------------------
C
C Define command-language parameters for text-option structure
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
       call graphic_copy_text_opt( opt, text_opt, s )

C setup parameters
       call chr_chitoc( text_opt_set, text, lt )
       call cmd_setlocal('%text-status',text(1:lt), s )
       call cmd_setlocal('%text-font',font_name(text_opt_font),s)
       call cmd_setlocal('%text-colour',colour_name(text_opt_colour),s)
       call chr_chitoc( text_opt_width, text, lt )
       call cmd_setlocal('%text-width',text(1:lt), s )
       call chr_chrtoc( text_opt_size, text, lt )
       call cmd_setlocal('%text-size',text(1:lt), s )
       call cmd_err(s,'graphic_pars_text_opt',' ')
       end
