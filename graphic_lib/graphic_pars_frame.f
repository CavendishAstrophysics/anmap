*+ graphic_pars_frame

       subroutine graphic_pars_frame( opt, s )
C      ---------------------------------------
C
C Define command-language parameters for frame-option structure
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
       call graphic_copy_frame_opt( opt, frame_opt, s )

C setup parameters
       call chr_chitoc( frame_opt_set, text, lt )
       call cmd_setlocal('%frame-status',text(1:lt), s )
       call graphic_pars_axis(frame_opt_x_axis,'x-',s)
       call graphic_pars_axis(frame_opt_y_axis,'y-',s)
       call graphic_pars_text_opt(frame_opt_text_opt,s)
       call cmd_err(s,'frame_pars_frame',' ')
       end
