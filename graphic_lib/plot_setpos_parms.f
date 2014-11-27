C
C
*+ plot_setpos_parms

       subroutine plot_setpos_parms( uv, s )
C      -------------------------------------
C
C Set the standard map-position parameters given a map-position
C
C Given:
C   map-position
       real*4    uv(2)
C
C Updated:
C   error status
       integer   s
C
C-
       include '/mrao/include/chrlib_functions.inc'

       real*4       pos_u, pos_v
       character    value*20, rnumber*20
       integer      i1, i2, len_v

       if (s.ne.0) return

       pos_u = uv(1)
       pos_v = uv(2)

       call chr_chitoc(nint(pos_u),rnumber,i2)
       i1 = chr_intlc(rnumber)
       call cmd_setlocal('%u',rnumber(i1:i2),s)
       value = rnumber(i1:i2)
       call chr_chitoc(nint(pos_v),rnumber,i2)
       i1 = chr_intlc(rnumber)
       call cmd_setlocal('%v',rnumber(i1:i2),s)
       len_v = chr_lenb(value)
       value = value(1:len_v)//','//rnumber(i1:i2)
       len_v = chr_lenb(value)
       call cmd_setlocal( '%uv-pos', value(1:len_v), s )

       call chr_chrtoc(pos_u,rnumber,i2)
       i1 = chr_intlc(rnumber)
       call cmd_setlocal('%map-u',rnumber(i1:i2),s)
       value = rnumber(i1:i2)
       call chr_chrtoc(pos_v,rnumber,i2)
       i1 = chr_intlc(rnumber)
       call cmd_setlocal('%map-v',rnumber(i1:i2),s)
       len_v = chr_lenb(value)
       value = value(1:len_v)//','//rnumber(i1:i2)
       len_v = chr_lenb(value)
       call cmd_setlocal( '%map-pos', value(1:len_v), s )

       call cmd_err(s,'plot_setpos_parms',' ')

       end
