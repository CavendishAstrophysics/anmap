C
C
*+ plot_frinit

       subroutine plot_frinit(reset_frame, status)
C      ------------------------------------------
C
C Initialise the image frame
C
C Option to control resetting the frame, viewport and window
       logical    reset_frame
C Error return
       integer    status
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C windows on viewport and window
       real*4     xv1,  xv2,  yv1,  yv2,
     *            vpx1, vpx2, vpy1, vpy2,
     *            x1, x2, y1, y2
C space for the plot within the view-surface
       real*4     space_x, space_y
C scaling parameters for the plot
       real*4     x_uv_size, y_uv_size, ratio, x_convert

C check status on entry
       if (status.ne.0) return


C calculate ratio of sides of plot to give equal scales (mm/gp)
       y_uv_size = uv_range(3) - uv_range(4) + 1.0
       x_uv_size = uv_range(2) - uv_range(1) + 1.0
       ratio = x_uv_size/y_uv_size

C enquire view-port size in normalized device coordinates
       call pgqvp(0,vpx1,vpx2,vpy1,vpy2)
       space_x = vpx2 - vpx1
       space_y = vpy2 - vpy1

C enquire actual size of plotting surface (mm)
       call pgqvp(2,x1,x2,y1,y2)

C ensure a plot with equal scales in X and Y (physical)
       x_convert = (y2-y1)/(x2-x1)
       if (x_convert.gt.1.0) then
         if (frame_space.le.0.0) then
           space_y = space_y/1.2
           space_x = space_x/1.2
         else
           space_y = space_y*frame_space
           space_x = space_x*frame_space
         end if
       else if (ratio.gt.1.0) then
         if (frame_space.gt.0.0) then
           space_x = space_x*frame_space/x_convert
           space_y = space_y/x_convert
         end if
       end if

C check for scaling in Y or X direction
       if (x_convert.gt.1.0) then
         yv2 = vpy2 - space_y*0.05/x_convert
         xv1 = vpx1 + space_x*0.025
         if (ratio.le.1.0) then
           yv1 = yv2 - space_y*0.9/x_convert
           xv2 = ratio*space_x*0.9 + xv1
         else
           yv1 = yv2 - space_y*0.9/(ratio*x_convert)
           xv2 = space_x*0.9 + xv1
         end if
       else
         yv2 = vpy2 - space_y*0.05
         xv1 = vpx1 + space_x*0.025*x_convert
         if (ratio.le.1.0) then
           yv1 = yv2 - space_y*0.9
           xv2 = ratio*space_x*x_convert*0.9 + xv1
         else
           yv1 = yv2 - space_y*0.9/ratio
           xv2 = space_x*x_convert*0.9 + xv1
         end if
       end if

C .. set the viewport
       call pgvport(xv1+frame_offset(1),xv2+frame_offset(1),
     *              yv1+frame_offset(2),yv2+frame_offset(2))

C .. set the window
       x1 = uv_range(1)
       x2 = uv_range(2)
       y1 = uv_range(4)
       y2 = uv_range(3)
       call pgwindow(x1,x2,y1,y2)

       if (reset_frame) then
         call pgqvp(0,frame_vp(1),frame_vp(2),
     *                frame_vp(3),frame_vp(4))
         call pgqwin(frame_win(1),frame_win(2),
     *               frame_win(3),frame_win(4))
       end if
       frame_init = .true.
       call cmd_err(status,'PLOT_FRINIT',' ')

       end
