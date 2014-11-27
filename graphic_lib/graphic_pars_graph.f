*+ graphic_pars_grline

       subroutine graphic_pars_graph( opt, s )
C      ----------------------------------------
C
C Define command-language parameters for graph-option structure
C
C Given:
C    structure
       integer    opt
C
C Updated:
C    error status
       integer    s
C
C-
       include '../include/plt_buffer_defn.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'
       include '../include/plt_graph_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

       character  text*80, name*12
       integer    lt, n

C check status on entry
       if (s.ne.0) return

C setup parameters
       call chr_chitoc( graph_frame_status, text, lt )
       call cmd_setlocal('%graph-frame-status',text(1:lt), s )
       call chr_chitoc( graph_max_lines, text, lt )
       call cmd_setlocal('%graph-max-lines',text(1:lt), s )
       call chr_chitoc( graph_opt_xt, text, lt )
       call cmd_setlocal('%graph-opt-xt',text(1:lt), s )
       call chr_chitoc( graph_opt_yt, text, lt )
       call cmd_setlocal('%graph-opt-yt',text(1:lt), s )
       call chr_chitoc( graph_opt_t, text, lt )
       call cmd_setlocal('%graph-opt-t',text(1:lt), s )
       call chr_chitoc( graph_view_port_opt, text, lt )
       call cmd_setlocal('%graph-opt-vp',text(1:lt), s )
       call cmd_defparam(.false.,'%graph-vp','integer',4,s)
       do n=1,4
         write(name,'(''%graph-vp{'',i1,''}'')') n
         call chr_chitoc( graph_view_port(n), text, lt )
         call cmd_setlocal(name,text(1:lt), s )
       enddo
       call cmd_defparam(.false.,'%graph-xr','real',2,s)
       do n=1,4
         write(name,'(''%graph-xr{'',i1,''}'')') n
         call chr_chrtoc( graph_xrange(n), text, lt )
         call cmd_setlocal(name,text(1:lt), s )
       enddo
       call cmd_defparam(.false.,'%graph-yr','real',2,s)
       do n=1,4
         write(name,'(''%graph-yr{'',i1,''}'')') n
         call chr_chrtoc( graph_yrange(n), text, lt )
         call cmd_setlocal(name,text(1:lt), s )
       enddo

       call cmd_err(s,'graph_pars_graph',' ')
       end
