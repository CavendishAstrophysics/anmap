
*+ic2_ruvduv

       subroutine ic2_ruvduv( redtape, data, uv, pduv, s )
C      ---------------------------------------------------
C
C     Returns the map partial derivatives at a given, real u, v point.
C
C     Given:
C         Abbreviated map redtape - see (library)maplib-minirt:incl
              INTEGER     REDTAPE(*)
C         Map data
              REAL        DATA(*)
C         U, V position to find partial derivatives at.
              REAL*8      UV(2)

C     Returned:
C         Returned values - two C*8 if an aperture,  two R*4's if a map.
              REAL        PDUV(*)
C         Status - must be zero on entry.
              INTEGER     S
C
C     Degrids the map partial derivatives for a given, non-integral,
C     uv point in a map or aperture.  At present, the degridding is
C     done using a tabulated gaussian-sinc degridding function. The
C     first zero of the function is at a radius of 1 and the standard
C     deviation of the gaussian is sqrt(0.5/0.275). The tabulation is
C     oversampled by a factor of 128 (so the ideal of a continuous map
C     is approximated by a map with a grid size 128  times finer than
C     the real map) and has a halfwidth of 3 pixels. U, V values too
C     near the edge of the map return an error of UV_OUTMAP.
C
C     N.B. Does not use current map redtape.
C
C
*-
      call ruvduv2( redtape, data, uv, pduv, s )
      end
