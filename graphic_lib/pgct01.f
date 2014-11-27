
C+PGCT01    -- function defining standard colour table

      REAL FUNCTION PGCT01 (RGB, CI, CIMIN, CIMAX)
      INTEGER RGB,CI,CIMIN,CIMAX
C-----------------------------------------------------------------------
C Function defining standard colour table 01.  This function can be used
C as an argument to routine PGSCT.
C
C Returns:
C  PGCT01   : The red, green or blue intensity for a given colour index,
C             as a real number in the range 0.0 to 1.0, for the colour
C             table defined by:
C
C                red   = sin(3x)**2,
C                green = sin(5x)**2,
C                blue  = sin(7x)**2,   x = (ci-cimin)/(cimax-cimin)
C
C Arguments:
C  RGB (input, integer) : colour (red=1, green=2, blue=3)
C  CI (input, integer) : colour index in the range [CIMIN,CIMAX]
C  CIMIN (input, integer) : minimum colour index
C  CIMAX (input, integer) : maximum colour index
C
C 29-May-1989 - [DJT].
C-----------------------------------------------------------------------
      REAL X
      INTEGER R,G,B
      PARAMETER (R=1,G=2,B=3)
C
      X = FLOAT(CI-CIMIN)/FLOAT(CIMAX-CIMIN)
      IF (RGB.EQ.R) THEN
        PGCT01 = SIN(X*3.)**2
      ELSEIF (RGB.EQ.G) THEN
        PGCT01 = SIN(X*5.)**2
      ELSEIF (RGB.EQ.B) THEN
        PGCT01 = SIN(X*7.)**2
      ENDIF
C
      END
