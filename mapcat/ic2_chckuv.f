
*+ic2_chckuv

       subroutine ic2_chckuv (uv_map, iuv, status)
C      -------------------------------------------
C
C  Checks U,V window for validity.
C
C  Given:
C   map U,V coordinate window
       integer   uv_map(4)
C   U,V coordinate window
       integer   iuv(4)
C
C  Updated:
C   status value
       integer   status
C
C  Checks a given U,V coordinate window IUV(IU1,IU2,IV1,IV2) for validity,
C  with respect to the given map uv range.
C
C  The STATUS value should be zero on entry.  Possible error values are:
C
C      - invalid U,V window (ILL_UVWIND)
C      - window outside map (UV_OUTMAP)
C
C
*-
       call chckuv2 (uv_map, iuv, status)
       end
