C
C
*+ plot_init_setup

       subroutine plot_init_setup( s )
C      -------------------------------
C
C Initialise the setup for an image plot
C
C Updated:
C   error status
       integer    s
C
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

       if (s.ne.0) return
       map_defined = .false.
       overlay_defined = .false.
       imap_current = 1
       imap = -1
       imap_overlay = -1
       call cmd_err(s,'plot_init_setup',' ')
       end
