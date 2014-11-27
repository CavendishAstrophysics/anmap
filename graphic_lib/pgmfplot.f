
C$Metafile output:

C+PGMFPLOT  -- plot a graphics metafile on an output device

      SUBROUTINE PGMFPLOT (FILE,DEVICE)
      CHARACTER*(*) FILE,DEVICE
C-----------------------------------------------------------------------
C Reads the specified graphics metafile and plots the output on the
C specified output device.  See Chapter 8 of the PGPLOT manual for
C further details.  Calls the GRPCKG routine GMFPLOT.
C
C Arguments:
C  FILE (input) : name of graphics metafile.
C  DEVICE (input) : name of a valid PGPLOT device.
C
C 18-Apr-1989 - new routine [DJT].
C-----------------------------------------------------------------------
       print *,'.. function (METAFILE) is not implemented'
*      CALL GMFPLOT(FILE,DEVICE)
      END
