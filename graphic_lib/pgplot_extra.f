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
      LOGICAL   INTER
      INTEGER   ICOL,ICUR,ICMIN,ICMAX,LTYPE
      INTEGER   R,G,B
      PARAMETER (R=1,G=2,B=3)
C
      CALL PGQINF( 'HARDCOPY', TYPE, LTYPE )
      IF (TYPE(1:LTYPE).EQ.'NO') THEN
        ICUR=128
        CALL PGQCOL( ICMIN, ICMAX )
        ICMIN=MAX(16,ICMIN)
        DO ICOL=ICMIN,ICMAX
C  Lookup table entry 128 reserved for cursor colour index
          IF (ICOL.NE.ICUR) THEN
            CALL PGSCR(ICOL,FUNC(R,ICOL,ICMIN,ICMAX),
     :                      FUNC(G,ICOL,ICMIN,ICMAX),
     :                      FUNC(B,ICOL,ICMIN,ICMAX))
          ENDIF
        ENDDO
      ENDIF
C
      CALL PGUPDT
C
      END

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

C+PGVECTORS -- plot a vector field diagram
       SUBROUTINE PGVECTORS( NI, NJ, I1, I2, J1, J2, CHI, AMP,
     *                       OPTION, SCALE, ISAMP, JSAMP, TRANS,
     *                       ROTATE, GATE, BLANK                )
C-----------------------------------------------------------------------
C
C Plot vectors on an existing frame
C
C Given:
C   size of input data array
       INTEGER    NI, NJ
C   region of input array to display
       INTEGER    I1, I2, J1, J2
C   data array of position angles measured North through East
       REAL*4     CHI(NI,*)
C   data array of amplitude
       REAL*4     AMP(NI,*)
C   options
       INTEGER    OPTION
C   scale factor for scaling vectors
       REAL*4     SCALE
C   sampling factors
       INTEGER    ISAMP, JSAMP
C   transformation array
       REAL*4     TRANS(6)
C   angle (radians) through which to rotate vectors (N through E)
       REAL*4     ROTATE
C   gate value on intensity image
       REAL*4     GATE
C   blank value on input CHI image (no data plotted)
       REAL*4     BLANK
C
C The transformation array has the same definition as for other PGPLOT
C routines EXCEPT THAT TRANSFORMATIONS INVOLVING SHEAR ARE NOT
C SUPPORTED IN THIS ROUTINE.
C
C        x = trans(1) + i*trans(2)
C        y = trans(4) + j*trans(6)
C
C This restriction is thought necessary as such transformations are
C ambiguous when considering the definition of position angle.
C
C Scale specifies the scaling of the vectors as determined for OPTION.
C   If OPTION=0 :  The AMP data array is used and the length of vectors
C                  is given as SCALE*AMP(I,J)
C   If OPTION=1 :  The AMP data array is used and the length of vectors
C                  is scaled such that the longest vector has a length
C                  SCALE
C   If OPTION=2 :  The AMP array is not used, all vectors have a length
C                  of SCALE
C
C The parameters ISAMP, JSAMP specify a sampling of the vector plot:
C vectors will only be plotted at every ISAMP'th, JSAMP'th pixel.
C
C P. Alexander, MRAO, 2/04/90.
C
C-----------------------------------------------------------------------
C Define local variables
       integer     i, j
       real*4      x, y, x1, y1, x2, y2, length, max_amp, local_gate

C test input options for consistency
       if (option.lt.0 .or. option.gt.2) then
         print *,'***(PGPLOT_VECTORS) Unkown option ',option
         return
       end if
       if (isamp.gt.(i2-i1+1) .or. jsamp.gt.(j2-j1+1)) then
         print *,'***(PGPLOT_VECTORS) Illegal sampling ',
     *           isamp,jsamp
         return
       end if
       if (gate.le.0.0) then
         local_gate = -1.0
       else
         local_gate = gate
       end if

C determine maximum on amplitude array if the option is equal to 1
       if (option.eq.1) then
         max_amp = 0.0
         do j=j1,j2
           do i=i1,i2
             if (abs(amp(i,j)).gt.max_amp) max_amp = abs(amp(i,j))
           end do
         end do
         if (max_amp.eq.0.0) then
           print *,'***(PGPLOT_VECTORS) Intensity map all zero ',
     *             'unable to scale vectors'
           return
         end if
       end if

C move through the data array as requested
       do j=j1,j2,jsamp
         do i=i1,i2,isamp

           if (chi(i,j).ne.blank .and. abs(amp(i,j)).gt.local_gate) then
C .. determine central location using the transformation array
             x = trans(1) + trans(2)*float(i)
             y = trans(4) + trans(6)*float(j)
C .. determine vector length depending on OPTION
             if (option.eq.0) then
               length = amp(i,j)*scale
             else if (option.eq.1) then
               length = amp(i,j)*scale/max_amp
             else if (option.eq.2) then
               length = scale
             end if
C .. move and display this vector
             x1 = x - 0.5*length*sin(chi(i,j)+rotate)
             y1 = y + 0.5*length*cos(chi(i,j)+rotate)
             x2 = x + 0.5*length*sin(chi(i,j)+rotate)
             y2 = y - 0.5*length*cos(chi(i,j)+rotate)
             call pgmove( x1, y1 )
             call pgdraw( x2, y2 )
           end if

         end do
       end do

       end

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

*+

       SUBROUTINE PLT_MOVTO(X,Y)
C      -------------------------
C
C Move pen to X,Y
C
*-
       REAL*4  X, Y, X_P, Y_P
       COMMON /PLT_COM1/ X_P,Y_P
       X_P = X
       Y_P = Y
       CALL PGMOVE(X_P,Y_P)
       END
C
C
*+

       SUBROUTINE PLT_MOVBY(X,Y)
C      -------------------------
C
C Move pen by X,Y
C
*-
       REAL*4  X, Y, X_P, Y_P
       COMMON /PLT_COM1/ X_P,Y_P
       X_P = X_P + X
       Y_P = Y_P + Y
       CALL PGMOVE(X_P,Y_P)
       END
C
C
*+

       SUBROUTINE PLT_DRAWTO(X,Y)
C      --------------------------
C
C Draw to X,Y
C
*-
       REAL*4  X, Y, X_P, Y_P
       COMMON /PLT_COM1/ X_P,Y_P
       X_P = X
       Y_P = Y
       CALL PGDRAW(X_P,Y_P)
       END
C
C
*+

       SUBROUTINE PLT_DRAWBY(X,Y)
C      --------------------------
C
C Draw by X,Y
C
*-
       REAL*4  X, Y, X_P, Y_P
       COMMON /PLT_COM1/ X_P,Y_P
       X_P = X_P + X
       Y_P = Y_P + Y
       CALL PGDRAW(X_P,Y_P)
       END
C
C
*+

       SUBROUTINE PLT_TEXT(TEXT)
C      -------------------------
C
C Plot text at current position
C
*-
       CHARACTER*(*)    TEXT

       REAL*4  X_P, Y_P
       COMMON /PLT_COM1/ X_P,Y_P

       CALL PGTEXT(X_P,Y_P,TEXT)
       END
