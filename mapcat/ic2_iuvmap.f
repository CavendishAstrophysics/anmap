

*+ic2_iuvmap2

       integer function ic2_iuvmap( uv_map, uv_pix )
C      ---------------------------------------------
C
C  Finds the index of a U,V point within a map.
C
C  Given:
C    U, V window of map.
       integer   uv_map(4)
C    U, V coordinate of a pixel
       integer   uv_pix(2)
C
C  Returns the index of the point UV_PIX within the map represented
C  by the given abbreviated U,V window.  If the point UV_PIX lies
C  outside the map, the returned value is zero.
C
C
*-
       integer      iuvmap2
       ic2_iuvmap = iuvmap2( uv_map, uv_pix )
       end
