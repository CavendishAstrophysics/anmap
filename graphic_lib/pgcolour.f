C$Attribute setting:

C+PGCOLOUR  -- set standard colour tables

      SUBROUTINE PGCOLOUR
C-----------------------------------------------------------------------
C Sets standard colour tables, for devices supporting colour graphics.
C
C 16-Dec-1988 - new routine for Lexidata image processor [DJT].
C-----------------------------------------------------------------------

      EXTERNAL PGCT01
C
      CALL PGSCT(PGCT01)
C
      END
