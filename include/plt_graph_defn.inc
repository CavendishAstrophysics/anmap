C ANMAP graph-style definition: plt_graph_defn.inc
C -----------------------------
C
C This file defines a graph-style definition for Anmap, it
C assumed the following have been defined:
C
C   plt_basic_defn    ---   basic definitions of record types
C   plt_grline_defn   ---   defn of line options to graphs
C
       integer       len_graph
       parameter    (len_graph = len_struct)
       integer       graph(len_graph)

C graphic definition
       integer       graph_graphic(len_graphic)
       equivalence  (graph_graphic, graph(1))
C frame
       integer       graph_frame_status
       integer       graph_frame(len_frame_opt)
       equivalence  (graph_frame_status, graph(1+len_graphic))
       equivalence  (graph_frame, graph(2+len_graphic))
C lines for graph
       integer       graph_max_lines
       parameter    (graph_max_lines = 10)
       integer       graph_grline(len_grline,graph_max_lines)
       equivalence  (graph_grline, graph(2+len_graphic+len_frame_opt))
       integer       ipgraph_1
       parameter    (ipgraph_1 = 1 + len_graphic+len_frame_opt +
     *                               len_grline*graph_max_lines)
C titles
       integer       graph_x_title, graph_y_title, graph_title
       equivalence  (graph_x_title, graph(1+ipgraph_1))
       equivalence  (graph_y_title, graph(2+ipgraph_1))
       equivalence  (graph_title,   graph(3+ipgraph_1))
C text-style for titles
       integer       graph_title_opt(len_text_opt)
       equivalence  (graph_title_opt,graph(4+ipgraph_1))
C control options for positioning titles
       integer       graph_opt_xt
       real*4        graph_posn_xt(3)
       integer       graph_opt_yt
       real*4        graph_posn_yt(3)
       integer       graph_opt_t
       real*4        graph_posn_t(3)
       equivalence  (graph_opt_xt,  graph(4+ipgraph_1+len_text_opt))
       equivalence  (graph_opt_yt,  graph(5+ipgraph_1+len_text_opt))
       equivalence  (graph_opt_t,   graph(6+ipgraph_1+len_text_opt))
       equivalence  (graph_posn_xt, graph(7+ipgraph_1+len_text_opt))
       equivalence  (graph_posn_yt, graph(10+ipgraph_1+len_text_opt))
       equivalence  (graph_posn_t,  graph(13+ipgraph_1+len_text_opt))
C view-port
       integer       graph_view_port_opt
       real*4        graph_view_port(4)
       equivalence  (graph_view_port_opt, 
     *               graph(16+ipgraph_1+len_text_opt))
       equivalence  (graph_view_port, graph(17+ipgraph_1+len_text_opt))
C data range
       integer       graph_xcoord_opt, graph_ycoord_opt 
       equivalence  (graph_xcoord_opt, 
     *               graph(21+ipgraph_1+len_text_opt))
       equivalence  (graph_ycoord_opt, 
     *               graph(22+ipgraph_1+len_text_opt))
       real*4        graph_xrange(2), graph_yrange(2),
     *               graph_coords(4)
       equivalence  (graph_xrange, graph(23+ipgraph_1+len_text_opt))
       equivalence  (graph_yrange, graph(25+ipgraph_1+len_text_opt))
       equivalence  (graph_coords, graph(23+ipgraph_1+len_text_opt))
       integer       ipgraph_2
       parameter    (ipgraph_2 = 30+ipgraph_1+len_text_opt)

C key options
       integer       graph_key_status
       real*4        graph_key_opts(4)
       integer       graph_key_text(len_text_opt)
       equivalence  (graph_key_status, graph(ipgraph_2+1))
       equivalence  (graph_key_opts, graph(ipgraph_2+2))
       equivalence  (graph_key_text, graph(ipgraph_2+6))
       integer       ipgraph_3
       parameter    (ipgraph_3 = ipgraph_2 + 5 + len_text_opt)

C make available text buffers
       integer           graph_ntb
       parameter        (graph_ntb = 4*graph_max_lines)
       integer           graph_itext(graph_ntb)
       character*(ltb)   graph_text(graph_ntb)
       equivalence      (graph_itext, graph(1+ipgraph_3))
       equivalence      (graph_text,  graph(1+ipgraph_3+graph_ntb))
       integer           ipgraph_4
       parameter        (ipgraph_4 = ipgraph_3 + graph_ntb + 
     *                               (ltb*graph_ntb)/4      )

C make available space for annotations
       integer           len_annot_data
       parameter        (len_annot_data = len_graph - ipgraph_4)
       integer           annot_data(len_annot_data)
       equivalence      (annot_data, graph(ipgraph_4+1))

C make the graph definition global
       common /graph_defn_common/ graph

