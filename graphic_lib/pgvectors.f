
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
