C
C+PGQCUR    -- inquire cursor availability

      SUBROUTINE PGQCUR (FLAG)
      LOGICAL FLAG
C-----------------------------------------------------------------------
C Query the availability of cursor input for the currently selected
C output device.
C
C Argument:
C  FLAG   (output)   : .TRUE. if device supports cursor input.
C
C 15-Oct-1987 - new routine [DJT].
C 18-Feb-1988 - superceded by the PGQINF routine [DJT].
C-----------------------------------------------------------------------

      CHARACTER  TYPE*16
      INTEGER    LTYPE
C
      CALL PGQINF( 'CURSOR', TYPE, LTYPE )
      FLAG = TYPE(1:LTYPE).EQ.'YES'
      END
