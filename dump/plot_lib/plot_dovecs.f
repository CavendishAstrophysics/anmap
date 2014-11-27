C
C
*+ plot_dovecs

       subroutine plot_dovecs(chi_map,int_map,status)
C      ----------------------------------------------
C
C Produce a grey scale plot (on most devices overwriting the screen)
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '/mrao/include/constants.inc'

C data arrays
       real*4    chi_map(*), int_map(*)
C error flag and pointers
       integer   status
C transformation array
       real*4    trans(6), length
C UV-range and size of plot
       integer   uv_full(4), ius, ivs, i1, i2, j1, j2
C blank value on CHI map
       real*4    blank_value


C check status on entry
       if (status.ne.0) return

C check for vectors option
       if (.not.vectors_opt) return

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


C do the plotting
       status = 0
       length = vec_scale
       if (vec_type.ne.0) length = vec_length
       call ennull( blank_value, status )
       call graphic_set_line_opt(vector_line_style,status)
       vec_rotate = vec_rotate*const_d2r
       call pgvectors(ius,ivs,i1,i2,j1,j2,chi_map,int_map,
     *                vec_type,length,vec_u_samp,vec_v_samp,
     *                trans, vec_rotate,
     *                vec_gate, blank_value)
       status = 0

C return to immediate update state
       call pgebuf

       call cmd_err(status,'PLOT_DOVECS',' ')

       end
