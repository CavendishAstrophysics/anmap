C
C
*+ plot_init_opt

       subroutine plot_init_opt( s )
C      -----------------------------
C
C Initialise options for image/map-display
C
C Updated:
C   error status
       integer    s
C
C All the options for image/map-displays are initialised.
C
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

       integer   n

       if (s.ne.0) return

C interpolation
       interpolate_opt = .false.

C overlay-map option
       overlay_map = .false.

C line-styles for contours
       do n=1,max_contour_styles
         call graphic_default_line_opt(contour_styles(1,n),s)
       enddo
       positive_style = 1
       negative_style = 2
       zero_style = 3
       call graphic_copy_line_opt(contour_styles(1,negative_style),
     *                            line_opt,s)
       line_opt_style = line_dash
       call graphic_copy_line_opt(line_opt,
     *                            contour_styles(1,negative_style),
     *                            s)
       call graphic_copy_line_opt(contour_styles(1,zero_style),
     *                            line_opt,s)
       line_opt_style = line_dot
       call graphic_copy_line_opt(line_opt,
     *                            contour_styles(1,zero_style),
     *                            s)

C image display
       scale_bar_opt = 1
       scale_bar_text = 1
       scale_bar_width = 0.025
       call graphic_default_text_opt( scale_bar_text_style, s )

C vectors
       vectors_opt = .false.
       vec_u_samp = 1
       vec_v_samp = 1
       vec_rotate = 0.0
       vec_gate = 0.0
       vec_type = 2
       vec_length = 2.0
       call graphic_default_line_opt( vector_line_style, s )

C symbol plot
       symbol_opt = .false.
       symb_max = ichar('+')
       symb_min = ichar('-')
       symb_blank = ichar('o')
       val_symb_max = 1.0E+30
       val_symb_min = -1.0E-30
       call graphic_default_text_opt( symbol_text_style, s )

C titles
       title_opt = .false.
       date_opt = .true.
       cont_opt = .true.
       annotation_drop = 1.0
       call graphic_default_text_opt( title_text_style, s )
       call graphic_default_text_opt( annotations_text_style, s )
       title_plot = ' '
       crosses_opt = 0
       crosses_size = 1.0
       crosses_file = ' '
C frame
       pframe_opt = .true.
       pips_opt = .true.
       grid_opt = .false.
       uvpip_opt = .false.
       uvgrid_opt = .false.
       frame_space = 0.0
       frame_offset(1) = 0.0
       frame_offset(2) = 0.0
       mm_grid_point_x = 1.0
       mm_grid_point_y = 1.0
       pip_size = 1.0
       pip_off_x = 0.1
       pip_off_y = 1.0
       call graphic_default_line_opt( frame_line_style, s )
       call graphic_default_text_opt( frame_text_style, s )
       call graphic_default_line_opt( grid_line_style, s )
       grid_u = 10.0
       grid_v = 10.0

       end

