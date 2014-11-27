* Scratch structure definition : plt_scratch_defn.inc
* ---------------------------------------------------
*

C define structure
       integer      len_scratch_defn
       parameter   (len_scratch_defn = len_struct)
       integer      scratch_defn(len_scratch_defn)

C define a graphic structure for the scratch structure
       integer      scratch_graphic(len_graphic)
       equivalence (scratch_graphic, scratch_defn(1))

C define a coordinate system for this draw structure
       real*4       scratch_coords(4)
       integer      scratch_coord_opt
       equivalence (scratch_coords, scratch_defn(1+len_graphic))
       equivalence (scratch_coord_opt, scratch_defn(5+len_graphic))

C define view-port for scratch window
       real*4       scratch_view_port(4)
       integer      scratch_view_port_opt
       equivalence (scratch_view_port, scratch_defn(6+len_graphic))
       equivalence (scratch_view_port_opt, 
     *              scratch_defn(10+len_graphic))

C define frame style
       integer      scratch_line_opt(len_line_opt)
       integer      scratch_text_opt(len_text_opt)
       integer      scratch_frame_opt(len_frame_opt)
       integer      scratch_symbol
       integer      ip_sf
       parameter   (ip_sf = 21+len_graphic)
       equivalence (scratch_line_opt,scratch_defn(ip_sf))
       equivalence (scratch_text_opt,scratch_defn(ip_sf+len_line_opt))
       equivalence (scratch_frame_opt,
     *              scratch_defn(ip_sf+len_line_opt+len_text_opt))
       equivalence (scratch_symbol,
     *    scratch_defn(ip_sf+len_line_opt+len_text_opt+len_frame_opt))

C titles
       integer      ip_st
       parameter   (ip_st = 
     *              ip_sf+len_line_opt+len_text_opt+len_frame_opt+1)
       character    main_title*80, x_title*80, y_title*80
       equivalence (main_title, scratch_defn(ip_st))
       equivalence (x_title, scratch_defn(ip_st+20))
       equivalence (y_title, scratch_defn(ip_st+40))

C command to generate plot
       character*256 scratch_command
       equivalence (scratch_command, scratch_defn(ip_st+60))
       integer      len_scratch_defn_used
       parameter   (len_scratch_defn_used = ip_st+60+64)
       integer      len_annot_data 
       parameter   (len_annot_data = 
     *              (len_struct - len_scratch_defn_used) )
       integer      annot_data(len_annot_data)
       equivalence (annot_data, scratch_defn(len_scratch_defn_used+1))
       common /scratch_defn_common/ scratch_defn
