* Annotation structure definitions : plt_annot_defn.inc
* -----------------------------------------------------
*

C structure definition
       integer      annot(len_struct)

C control information
       integer      annot_control(4)
       equivalence (annot_control, annot(1))

C define local options
       real*4       arrow_head_angle, arrow_head_vent
       integer      arrow_fill_style
       equivalence (arrow_head_angle, annot(5))
       equivalence (arrow_head_vent, annot(6))
       equivalence (arrow_fill_style, annot(7))
       logical      annot_immediate, annot_cursor
       equivalence (annot_immediate, annot(8))
       equivalence (annot_cursor, annot(9))
       integer      annot_line_opts(len_line_opt)
       integer      annot_text_opts(len_text_opt)
       integer      annot_arrow_opts(len_arrow_opt)
       equivalence (annot_line_opts, annot(10))
       equivalence (annot_text_opts, annot(10+len_line_opt))
       equivalence (annot_arrow_opts, 
     *              annot(10+len_line_opt+len_text_opt))
       integer      ipa1
       parameter   (ipa1 = 11+len_text_opt+len_line_opt+len_arrow_opt)

C define maximum number of objects and labels
       integer    len_annot_text 
       parameter (len_annot_text = 40)
       integer    max_annot_object
       parameter (max_annot_object = (len_struct-ipa1+1)/
     *                    (len_object+1+len_annot_text/4) )
       integer    max_annot_text 
       parameter (max_annot_text = max_annot_object)

C define objects
       integer       annot_object(len_object,max_annot_object)
       equivalence  (annot_object, annot(ipa1))
       integer       ipa2
       parameter    (ipa2 = ipa1 + len_object*max_annot_object)

C define lables
       character*(len_annot_text)  annot_text(max_annot_text)
       equivalence  (annot_text, annot(ipa2))
       integer       ipa3
       parameter    (ipa3 = ipa2 + len_annot_text*max_annot_text/4)

C define label indices
       integer    annot_itext(max_annot_text)
       equivalence  (annot_itext, annot(ipa3))
       integer       ipa4
       parameter    (ipa4 = ipa3 + max_annot_text)

       common /annot_defn_common/ annot


