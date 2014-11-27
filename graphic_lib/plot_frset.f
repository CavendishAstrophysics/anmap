C
C
*+ plot_frset

       subroutine plot_frset(status)
C      -----------------------------
C
C Define the view-port and window appropriate for contouring/grey-scale
C
C Error return
       integer    status
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'


       if (status.ne.0) return
       if (frame_init) then
         call pgsetvp(frame_vp(1),frame_vp(2),frame_vp(3),frame_vp(4))
         call pgsetwin(frame_win(1),frame_win(2),
     *                 frame_win(3),frame_win(4))
       endif
       call cmd_err(status,'PLT_FRSET',' ')

       end
