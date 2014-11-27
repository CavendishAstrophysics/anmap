

*+ic2_ruvval

       subroutine ic2_ruvval(redtape, data, uv, degrid_type, result, s)
C      ----------------------------------------------------------------
C
C     Returns the map value at a given, real u, v point.
C
C     Given:
C         Abbreviated map redtape - see (library)maplib-minirt:incl
              INTEGER     REDTAPE(*)
C         Map data
              REAL        DATA(*)
C         U, V position to find value of.
              REAL*8      UV(2)
C         Function to use for degridding.
              INTEGER     DEGRID_TYPE
C
C     Returned:
C         Returned value - complex if an aperture, real if a map.
              REAL        RESULT(*)
C         Status - must be zero on entry.
              INTEGER     S
C
C     Degrids the map data to a given, non-integral uv point in a map
C     or aperture.  At present, there are two types of degridding
C     function available :
C
C     degrid_type 1 - linear degridding from the four nearest pixels.
C     degrid_type 2 - degridding using a tabulated gaussian-sinc
C                     degridding function. The first zero of the
C                     function is at a radius of 1 and the standard
C                     deviation of the gaussian is sqrt(0.5/0.275). The
C                     tabulation is oversampled by a factor of 100 and
C                     has a halfwidth of 3 pixels. U, V near the edge
C                     of the map are interpolated linearly and any
C                     undefined pixels are ignored.
C
C
*-
      call ruvval2(redtape, data, uv, degrid_type, result, s)
      end
