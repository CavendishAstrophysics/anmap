C
C
*+ plot_dogrey

       subroutine plot_dogrey(map_array,status)
C      ----------------------------------------
C
C Produce a grey scale / false-colour plot
C
C Updated:
C   Error status
       integer    status
C
C Plot the grey-scale representation
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C data arrays
       real*4    map_array(*)
C transformation array
       real*4    trans(6), grey_scale_buffer(2,256)
C device dependent scaling
       real*4    black, white
       character current_device_type*80
       integer   len_cdt
C UV-range and size of plot
       integer   uv_full(4), ius, ivs
C counters etc.
       integer   i, j, i1, i2, j1, j2
C windows and view ports
       real*4    xv1, xv2, yv1, yv2, xw1, xw2, yw1, yw2
C character attributes
       real*4    old_ch_size
       integer   old_font
C string for label and length of the string
       character string*20
       integer   length

C check status on entry
       if (status.ne.0) return

C check for grey-scale option
       if (.not.image_opt) return

C turn off immediate update buffering
       call pgbbuf

C define matrix to transform to map coordinates
       call iuv_load(uv_full,status)
       trans(1) = uv_full(1) - 1
       trans(2) = 1.0
       trans(3) = 0.0
       trans(4) = uv_full(3) + 1
       trans(5) = 0.0
       trans(6) = -1.0
       ius = abs(uv_full(2) - uv_full(1))+1
       ivs = abs(uv_full(3) - uv_full(4))+1
       i1 = uv_range(1) - uv_full(1) + 1
       i2 = uv_range(2) - uv_full(1) + 1
       j1 = uv_full(3) - uv_range(3) + 1
       j2 = uv_full(3) - uv_range(4) + 1


C do the grey-scale
       status = 0
       call pgqinf('TYPE',current_device_type,len_cdt)
       black = image_min
       white = image_max
       call pggray(map_array,ius,ivs,i1,i2,j1,j2,
     *             white,black,trans)
       status = 0
       call graphic_set_line_opt(frame_line_style,status)
C plot scale bar if required
       if (scale_bar_opt.ne.0) then
C .. initialise scaled array for display
         if (scale_bar_opt.gt.0) then
           do i=1,256
             do j=1,2
               grey_scale_buffer(j,i) = black +
     *                          (white-black)*float(i-1)/255.0
             end do
           end do
         else
           do i=1,256
             do j=1,2
               grey_scale_buffer(j,i) = black +
     *                          (white-black)*float(256-i)/255.0
             end do
           end do
         endif
C .. reset the range of the plot -- save current defaults
         call pgqvp( 0, xv1, xv2, yv1, yv2 )
         call pgqwin( xw1, xw2, yw1, yw2 )
         call pgvport( min(xv1,xv1+(xv2-xv1)*scale_bar_width),
     *                 max(xv1,xv1+(xv2-xv1)*scale_bar_width),
     *                 yv1, yv2 )
         call pgwindow( 0.0, 2.0, 0.0, 256.0 )
         trans(1) = -0.5
         trans(2) = 1.0
         trans(3) = 0.0
         trans(4) = -0.5
         trans(5) = 0.0
         trans(6) = 1.0
         call pggray(grey_scale_buffer,2,256,1,2,1,256,
     *               white,black,trans)
         status = 0
         if (scale_bar_text.eq.1) then
           call graphic_copy_text_opt( frame_text_style,
     *                                 text_opt, status )
           call pgmove(0.0,0.0)
           call pgdraw(2.0,0.0)
           call pgdraw(2.0,256.0)
           call pgdraw(0.0,256.0)
           call pgdraw(0.0,0.0)
           call pgqch(old_ch_size)
           call pgsch(text_opt_size)
           call pgqcf(old_font)
           call pgscf(text_opt_font)
           if (scale_bar_opt.gt.0) then
             call chr_chrtoc(black,string,length)
             call pgmtext('L',0.1,0.0,0.0,string(1:length))
             call chr_chrtoc(white,string,length)
             call pgmtext('L',0.1,1.0,1.0,string(1:length))
           else
             call chr_chrtoc(black,string,length)
             call pgmtext('L',0.1,1.0,1.0,string(1:length))
             call chr_chrtoc(white,string,length)
             call pgmtext('L',0.1,0.0,0.0,string(1:length))
           endif
           call pgsch(old_ch_size)
           call pgscf(old_font)
         end if
         call pgvport( xv1, xv2, yv1, yv2 )
         call pgwindow( xw1, xw2, yw1, yw2 )
       end if

C return to immediate update state
       call pgebuf

       call cmd_err(status,'PLOT_DOGREY',' ')

       end
