* Drawing structure definition : plt_drawing_defn.inc
* ---------------------------------------------------
*

C define structre
       integer    len_draw_defn
       parameter (len_draw_defn = len_struct)
       integer    draw_defn(len_draw_defn)

C define a graphic structure for the draw structure
       integer    draw_graphic(len_graphic)

C define a coordinate system for this draw structure
       real*4     draw_coords(5)
       integer    draw_coord_opt
       equivalence (draw_coord_opt, draw_coords(5))

C define local options
       real*4     arrow_head_angle, arrow_head_vent
       integer    arrow_fill_style

C define and calculate number of objects and labels
       integer    max_draw_object
       parameter (max_draw_object = 40)
       integer    len_draw_text 
       parameter (len_draw_text = 40)
       integer    max_draw_text 
       parameter (max_draw_text = 
     *            ((len_draw_defn - len_graphic - 8
     *                              - max_draw_object*len_object)/
     *            (1 + len_draw_text/4)) )

C define objects
       integer    draw_object(len_object,max_draw_object)

C define lables
       character*(len_draw_text)  draw_text(max_draw_text)

C define label indices
       integer    draw_itext(max_draw_text)

C add these to the structure
       equivalence (draw_graphic, draw_defn(1))
       integer      ip_draw_coords
       parameter   (ip_draw_coords = 1+len_graphic)
       equivalence  (draw_coords, draw_defn(ip_draw_coords))
       equivalence  (arrow_head_angle, draw_defn(ip_draw_coords+5))
       equivalence  (arrow_head_vent, draw_defn(ip_draw_coords+6))
       equivalence  (arrow_fill_style, draw_defn(ip_draw_coords+7))
       integer      ip_draw_object
       parameter   (ip_draw_object = 8 + ip_draw_coords)
       equivalence (draw_object, draw_defn(ip_draw_object))
       integer      ip_draw_itext
       parameter   (ip_draw_itext = (ip_draw_object + 
     *                               max_draw_object*len_object))
       equivalence (draw_itext,   draw_defn(ip_draw_itext))
       integer      ip_draw_text
       parameter   (ip_draw_text = (ip_draw_itext+max_draw_text) )
       equivalence (draw_text,    draw_defn(ip_draw_text))

       common /draw_defn_common/ draw_defn

