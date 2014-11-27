

*+ic2_ruvmax

       subroutine ic2_ruvmax( redtape, data, iuv, ruv, max, s )
C      --------------------------------------------------------
C
C     Returns the real valued u and v of a local map maximum.
C
C     Given:
C         Abbreviated map redtape - see (library)maplib-minirt:incl
              INTEGER     REDTAPE(*)
C         Map data
              REAL        DATA(*)
C         U-V coordinates of local pixel maximum
              INTEGER     IUV(2)

C     Returned:
C         Position of map maximum.
              REAL*8      RUV(2)
C         Value of map at this position
              REAL        MAX
C         Status - must be zero on entry.
              INTEGER     S
C
C
*-
      call ruvmax2( redtape, data, iuv, ruv, max, s )
      end
