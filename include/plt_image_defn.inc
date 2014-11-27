C ANMAP plot image structure definition: plt_image_defn
C --------------------------------------
C
C P. Alexander   30/10/92
C
C Copyright P. Alexander, MRAO, Cavendish Laboratory, Cambridge
C 
C Current Version 1.0 30/10/92
C
C History:
C --------
C Version 0.1 30/10/92
C 
       integer    len_image_defn
       parameter (len_image_defn = len_struct)
       integer*4  image_defn(len_image_defn)

C Graphic control
C ---------------
C
C Graphic control information
       integer      ip_image_graphic
       parameter   (ip_image_graphic = 1)
       integer      len_image_graphic
       parameter   (len_image_graphic = len_graphic)
       integer      image_graphic(len_image_graphic)
       equivalence (image_graphic(1), image_defn(ip_image_graphic))


C General Options affecting many aspects of the plot
C --------------------------------------------------
C
       integer      ip_general_info
       parameter   (ip_general_info = ip_image_graphic + 
     *                                len_image_graphic)
       integer      len_general_info
       parameter   (len_general_info = 20)
       integer      general_info(len_general_info)
       equivalence (general_info(1), image_defn(ip_general_info))

C frame initialised for scaling
       logical      frame_init
       equivalence (frame_init, general_info(1))
C UV range for this frame
       integer      uv_range(4)
       equivalence (uv_range(1), general_info(2))
C main and overlay maps and pointer to current map (1,2)
       integer      imap, imaps(2), imap_overlay, imap_current
       equivalence (imap, imaps(1))
       equivalence (imap_overlay, imaps(2))
       equivalence (imaps, general_info(6))
       equivalence (imap_current, general_info(8))
C flag to indicate a map has been defined
       logical      map_defined
       equivalence (map_defined, general_info(9))
C flag to indicate an overlay map has been defined
       logical      overlay_defined
       equivalence (overlay_defined, general_info(10))
C flag to indicate an overlay option is selected
       logical      overlay_map
       equivalence (overlay_map, general_info(11))
C interpolate image data
       logical      interpolate_opt
       equivalence (interpolate_opt, general_info(12))
C ranges on defined maps
       real*4       data_range(2,2)
       equivalence (data_range(1,1), general_info(13))


C Contour Levels
C --------------
C
C Define sizes of arrays used in theis structure
C   maximum number of contours of all types
       integer      max_contours
       parameter   (max_contours = 200)
C   maximum number of contour styles
       integer      max_contour_styles
       parameter   (max_contour_styles = 5)


C Specify information for contours
       integer      len_contour_info
       parameter   (len_contour_info= 3*max_contours + 3 + 
     *                                len_line_opt*max_contour_styles)
       integer      contour_info(len_contour_info)
       integer      ip_contour_info
       parameter   (ip_contour_info = ip_general_info + 
     *                                len_general_info)
       equivalence (contour_info(1), image_defn(ip_contour_info))


C contours levels
       real*4       contour_list(max_contours)
       equivalence (contour_list(1), contour_info(1))
C array of contour status words
       integer      contour_status(max_contours)
       equivalence (contour_status(1), contour_info(max_contours+1))
C array of contour types 
       integer      contour_type(max_contours)
       equivalence (contour_type(1), contour_info(2*max_contours+1))
C line styles for contours
       integer      contour_styles(len_line_opt,max_contour_styles)
       equivalence (contour_styles(1,1), contour_info(3*max_contours+1))
C default styles for negative, positive and zero-level contours
       integer      negative_style, positive_style, zero_style
       equivalence (negative_style, 
     *  contour_info(3*max_contours+len_line_opt*max_contour_styles+1))
       equivalence (positive_style, 
     *  contour_info(3*max_contours+len_line_opt*max_contour_styles+2))
       equivalence (zero_style, 
     *  contour_info(3*max_contours+len_line_opt*max_contour_styles+3))

C Options for image display 
C -------------------------
C
C 
C Specify options for image
       integer      len_image_info
       parameter   (len_image_info = 10+len_text_opt)
       integer      image_info(len_image_info)
       integer      ip_image_info
       parameter   (ip_image_info = ip_contour_info + 
     *                              len_contour_info)
       equivalence (image_info(1), image_defn(ip_image_info))

C enable image display option
       logical      image_opt
       equivalence (image_opt, image_info(1))
       logical      image_done
       equivalence (image_done, image_info(2))
C data range to map into light to dark on display
       real*4       image_min, image_max
       equivalence (image_min, image_info(3))
       equivalence (image_max, image_info(4))
C scale-bar option
       integer      scale_bar_opt
       equivalence (scale_bar_opt, image_info(5))
C scale-bar text option
       integer      scale_bar_text
       equivalence (scale_bar_text, image_info(6))
C width of the scale bar
       real*4       scale_bar_width
       equivalence (scale_bar_width, image_info(7))
C scale-bar text style
       integer      scale_bar_text_style(len_text_opt)
       equivalence (scale_bar_text_style, image_info(8))



C Vector plot options
C -------------------
C
C Specify information for vectors
       integer      len_vector_info
       parameter   (len_vector_info = 15+len_line_opt)
       integer      vector_info(len_vector_info)
       integer      ip_vector_info
       parameter   (ip_vector_info = ip_image_info + 
     *                              len_image_info)
       equivalence (vector_info(1), image_defn(ip_vector_info))

C enable vector option
       logical      vectors_opt
       equivalence (vectors_opt, vector_info(1))
       logical      vectors_done
       equivalence (vectors_done, vector_info(2))
C type of vector plot
       integer      vec_type
       equivalence (vec_type, vector_info(3))
C scaling factor for vector plot
       real*4       vec_scale
       equivalence (vec_scale, vector_info(4))
C length of constant length vectors
       real*4       vec_length
       equivalence (vec_length, vector_info(5))
C gate for intensity map for vectors
       real*4       vec_gate
       equivalence (vec_gate, vector_info(6))
C position-Angle map
       integer      vec_chi_map
       equivalence (vec_chi_map, vector_info(7))
C intensity map
       integer      vec_int_map
       equivalence (vec_int_map, vector_info(8))
C sampling in U for vector plot
       integer      vec_u_samp
       equivalence (vec_u_samp, vector_info(9))
C sampling in V for vector plot
       integer      vec_v_samp
       equivalence (vec_v_samp, vector_info(10))
C angle through which to rotate vectors on plot
       real*4       vec_rotate
       equivalence (vec_rotate, vector_info(11))
C line style for vectors
       integer      vector_line_style(len_line_opt)
       equivalence (vector_line_style, vector_info(12))


C Symbol plot options
C -------------------
C
C Specify information for symbol plots
       integer      len_symbol_info
       parameter   (len_symbol_info = 10+len_text_opt)
       integer      symbol_info(len_symbol_info)
       integer      ip_symbol_info
       parameter   (ip_symbol_info = ip_vector_info + 
     *                              len_vector_info)
       equivalence (symbol_info(1), image_defn(ip_symbol_info))

C enable symbol option
       logical      symbol_opt
       equivalence (symbol_opt, symbol_info(1))
       logical      symbol_done
       equivalence (symbol_done, symbol_info(2))
C symbol codes for Symbol Map: blank pixel
       integer      symb_blank
       equivalence (symb_blank, symbol_info(3))
C symbol codes for Symbol Map: maximum pixel
       integer      symb_max
       equivalence (symb_max, symbol_info(4))
C symbol codes for Symbol Map: minimum pixel
       integer      symb_min
       equivalence (symb_min, symbol_info(5))
C upper gate value for symbol plot
       real*4       val_symb_max
       equivalence (val_symb_max, symbol_info(6))
C lower gate value for symbol plot
       real*4       val_symb_min
       equivalence (val_symb_min, symbol_info(7))
C text options for symbols
       integer      symbol_text_style(len_text_opt)
       equivalence (symbol_text_style, symbol_info(8))


C Title and anotations
C ---------------------
C
C Specify options for titles and annotations
       integer      len_title_info
       parameter   (len_title_info = 2*len_text_opt + 34 + 64)
       integer      title_info(len_title_info)
       integer      ip_title_info
       parameter   (ip_title_info = ip_symbol_info + 
     *                            len_symbol_info)
       equivalence (title_info(1), image_defn(ip_title_info))

C add title to plot
       logical      title_opt
       equivalence (title_opt, title_info(1))
C title added to frame
       logical      title_done
       equivalence (title_done,  title_info(2)) 
C add contour level anotations
       logical      cont_opt
       equivalence (cont_opt,  title_info(3))
C contour level annotation plotted
       logical      cont_done
       equivalence (cont_done,  title_info(4))
C add date to plot
       logical      date_opt
       equivalence (date_opt,  title_info(5))
C date plotted
       logical      date_done
       equivalence (date_done,  title_info(6))
C title of plot
       character    title_plot*80
       equivalence (title_plot,  title_info(7))
C drop between text lines
       real*4       annotation_drop
       equivalence (annotation_drop,  title_info(27))
C text style for annotations and titles
       integer      annotations_text_style(len_text_opt)
       equivalence (annotations_text_style, title_info(30))
       integer      title_text_style(len_text_opt)
       equivalence (title_text_style, title_info(30+len_text_opt))
C option to specify text has been plotted
       logical      text_done
       equivalence (text_done, title_info(30+2*len_text_opt))
C crosses file
       logical       crosses_done
       integer       crosses_opt
       real*4        crosses_size
       character*256 crosses_file
       equivalence  (crosses_opt, title_info(31+2*len_text_opt))
       equivalence  (crosses_size, title_info(32+2*len_text_opt))
       equivalence  (crosses_done, title_info(33+2*len_text_opt))
       equivalence  (crosses_file, title_info(34+2*len_text_opt))

C Frame Options and Style
C -----------------------
C
       integer      len_frame_info
       parameter   (len_frame_info = 34+len_line_opt+2*len_text_opt )
       integer      frame_info(len_frame_info)
       integer      ip_frame_info
       parameter   (ip_frame_info = ip_title_info + 
     *                             len_title_info)
       equivalence (frame_info(1), image_defn(ip_frame_info))

C plot frame
       logical      pframe_opt
       equivalence (pframe_opt, frame_info(1))
C frame plotted
       logical      pframe_done
       equivalence (pframe_done, frame_info(2))
C add pips to plot
       logical      pips_opt
       equivalence (pips_opt,  frame_info(3))
C add grid to plot
       logical      grid_opt
       equivalence (grid_opt,  frame_info(4))
C plot UV-pips
       logical      uvpip_opt
       equivalence (uvpip_opt,  frame_info(5))
C plot UV-grid
       logical      uvgrid_opt
       equivalence (uvgrid_opt,  frame_info(6))
 
C frame space -- space occupied by frame in view port (-1 default)
       real*4       frame_space
       equivalence (frame_space, frame_info(7))
C offset of frame in view port (NDC)
       real*4       frame_offset(2)
       equivalence (frame_offset,  frame_info(8))
C   pixel size
       real*4       mm_grid_point_x, mm_grid_point_y
       equivalence (mm_grid_point_x, frame_info(10))
       equivalence (mm_grid_point_y, frame_info(11))

C   flags to determine which sides of plot have pips
       logical      plot_pips(4)
       equivalence (plot_pips(1), frame_info(12))
C   Frame view-port
       real*4       frame_vp(4)
       equivalence (frame_vp(1), frame_info(16))
C   Frame window
       real*4       frame_win(4)
       equivalence (frame_win(1), frame_info(20))

C line style for frame
       integer      frame_line_style(len_line_opt)
       equivalence (frame_line_style(1), frame_info(24))
C character style for frame markers
       integer      frame_text_style(len_text_opt)
       equivalence (frame_text_style(1), frame_info(24+len_line_opt))

C grid options
       integer      grid_options(5+len_line_opt)
       equivalence (grid_options(1), 
     *              frame_info(24+len_line_opt+len_text_opt))
C   grid type
       integer      grid_type
       equivalence (grid_type, grid_options(1))
C   grid spacing RA/DEC
       real*4       grid_ra, grid_dec
       equivalence (grid_ra, grid_options(2))
       equivalence (grid_dec, grid_options(3))
C   grid spacing u/v
       real*4       grid_u, grid_v
       equivalence (grid_u, grid_options(4))
       equivalence (grid_v, grid_options(5))
C   grid line style
       integer      grid_line_style(len_line_opt)
       equivalence (grid_line_style(1), grid_options(6))
C pip size and control
       real*4       pip_size, pip_off_x, pip_off_y
       integer      ip_pip_size
       parameter   (ip_pip_size = 31+len_line_opt+2*len_text_opt)
       equivalence (pip_size, frame_info(ip_pip_size))
       equivalence (pip_off_x, frame_info(ip_pip_size+1))
       equivalence (pip_off_y, frame_info(ip_pip_size+2))

       integer      len_image_defn_used
       parameter   (len_image_defn_used=ip_frame_info+len_frame_info)

C annotations
       integer      len_annot_data 
       parameter   (len_annot_data = 
     *              (len_struct - len_image_defn_used) )
       integer      annot_data(len_annot_data)
       equivalence (annot_data, image_defn(len_image_defn_used+1))

       common /image_defn_common/ image_defn





