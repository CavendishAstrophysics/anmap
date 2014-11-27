
C+PGSCT     -- set colour tables

      SUBROUTINE PGSCT (FUNC)
C-----------------------------------------------------------------------
C Sets colour tables, for devices supporting colour graphics.
C
C
C For devices that support colour, this routine will set colour indices
C from minimum (usually 16, avoiding the standard PGPLOT colours) to
C maximum (usually 255), according to the specified external function.
C Various standard colour tables may be defined using the available
C functions PGCTxx.
C
C Argument:
C  FUNC (input,external) : Function defining the red, green, blue
C                          intensities for a given colour index.
C
C The external function must have the argument string:
C     FUNCTION PGCTxx (RGB,CI,CIMIN,CIMAX)
C and should be written to provide the red, green or blue intensity
C (RGB=1,2,3) as a real number in the range 0.0 to 1.0, for a given
C colour index CI in the range CIMIN to CIMAX.
C
C 29-May-1989 - new routine [PA,DJT].
C-----------------------------------------------------------------------

      REAL      FUNC
      CHARACTER TYPE*16
      INTEGER   ICOL,ICMIN,ICMAX,LTYPE
      INTEGER   R,G,B
      PARAMETER (R=1,G=2,B=3)
C
      CALL PGQINF( 'HARDCOPY', TYPE, LTYPE )
      IF (TYPE(1:LTYPE).EQ.'NO') THEN
        CALL PGQCOL( ICMIN, ICMAX )
        ICMIN=MAX(16,ICMIN)
        DO ICOL=ICMIN,ICMAX
            CALL PGSCR(ICOL,FUNC(R,ICOL,ICMIN,ICMAX),
     :                      FUNC(G,ICOL,ICMIN,ICMAX),
     :                      FUNC(B,ICOL,ICMIN,ICMAX))
        ENDDO
      ENDIF
C
      CALL PGUPDT
C
      END
