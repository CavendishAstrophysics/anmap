
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
C
C+PGSCT     -- set colour tables

      SUBROUTINE PGSCT (FUNC)
C-----------------------------------------------------------------------
C Sets colour tables, for devices supporting colour graphics.
C
C
C-----------------------------------------------------------------------

      REAL      FUNC
      INTEGER   ICOL,ICMIN,ICMAX
      INTEGER   R,G,B
      PARAMETER (R=1,G=2,B=3)
C
      CALL PGQCIR( ICMIN, ICMAX )
      DO ICOL=ICMIN,ICMAX
            CALL PGSCR(ICOL,FUNC(R,ICOL,ICMIN,ICMAX),
     :                      FUNC(G,ICOL,ICMIN,ICMAX),
     :                      FUNC(B,ICOL,ICMIN,ICMAX))
      ENDDO
C
      CALL PGUPDT
C
      END
C
C
       real*4 function ct_enh_col( rgb, ci, cimin, cimax )
C      ---------------------------------------------------
C
C Function defining an enhanced colour-scale.
C
C defining:
C             x = (ci-cimin)/(cimax-cimin)
C then:
C   ct_enh_col = ((x-start_colour)/(upper_bound-lower_bound))**power_law
C
C for:
C  lower_bound,start_colour  <=  x  <=  upper_bound
C
C otherwise:
C   ct_enh_col = 0.0 or 1.0
C
C Where the parameters upper_bound, lower_bound and power_law are passed
C in common and depend on the colour index
*-

       include 'pg_cttools.inc'

       integer        rgb, ci, cimin, cimax
       real*4         x

       x  = (float(ci-cimin)/float(cimax-cimin))
       if (x.ge.lower_bound .and. x.le.upper_bound) then
         if ((x-lower_bound).ge.col_starts(rgb)) then
           ct_enh_col = (       (x-col_starts(rgb))
     *                     /(upper_bound-col_starts(rgb))
     *                  )**col_power_law(rgb)
         else
           ct_enh_col = 0.0
         end if
       else if (x.lt.lower_bound) then
         ct_enh_col = 0.0
       else
         ct_enh_col = 1.0
       end if
       end
C
       real*4 function ct_enh_grey( rgb, ci, cimin, cimax )
C      ----------------------------------------------------
C
C Function defining an enhanced grey-scale.
C
C defining:
C             x = (ci-cimin)/(cimax-cimin)
C then:
C   ct_enh_grey = ((x-lower_bound)/(upper_bound-lower_bound))**power_law
C
C for:
C  lower_bound  <=  x  <=  upper_bound
C
C otherwise:
C   ct_enh_grey = 0.0
C
C Where the parameters upper_bound, lower_bound and power_law are passed
C in common.
*-

       include 'pg_cttools.inc'

       integer        rgb, ci, cimin, cimax
       real*4         x
       x  = (float(ci-cimin)/float(cimax-cimin))
       if (x.ge.lower_bound .and. x.le.upper_bound) then
         ct_enh_grey = ((x-lower_bound)/(upper_bound-lower_bound))**
     *                 power_law
       else if (x.lt.lower_bound) then
         ct_enh_grey = 0.0
       else
         ct_enh_grey = 1.0
       end if
       end
C
       real*4 function ct_enh_inv_grey( rgb, ci, cimin, cimax )
C      --------------------------------------------------------
C
C Function defining an enhanced grey-scale.
C
C defining:
C             x = (cimax-ci)/(cimax-cimin)
C then:
C   ct_enh_inv_grey = ((x-lower_bound)/(upper_bound-lower_bound))**power
C
C for:
C  lower_bound  <=  x  <=  upper_bound
C
C otherwise:
C   ct_enh_inv_grey = 0.0
C
C Where the parameters upper_bound, lower_bound and power_law are passed
C in common.
*-

       include 'pg_cttools.inc'

       integer        rgb, ci, cimin, cimax
       real*4         x
       x  = (float(ci-cimin)/float(cimax-cimin))
       if (x.ge.lower_bound .and. x.le.upper_bound) then
         ct_enh_inv_grey = ((upper_bound-x)/(upper_bound-lower_bound))**
     *                 power_law
       else if (x.lt.lower_bound) then
         ct_enh_inv_grey = 1.0
       else
         ct_enh_inv_grey = 0.0
       end if
       end
C
C
       real*4 function ct_lin_grey( rgb, ci, cimin, cimax )
C      ----------------------------------------------------
C
C Function defining a standard linear grey-scale.
C
C    ct_lin_grey = (ci-cimin)/(cimax-cimin)
C
*-

       integer        rgb, ci, cimin, cimax
       ct_lin_grey  = (float(ci-cimin)/float(cimax-cimin))
       end
C
       real*4 function ct_lin_inv_grey( rgb, ci, cimin, cimax )
C      --------------------------------------------------------
C
C Function defining a standard linear inverted grey-scale.
C
C    ct_lin_inv_grey = (cimax-ci)/(cimax-cimin)
C
*-

       integer        rgb, ci, cimin, cimax
       ct_lin_inv_grey  = (float(cimax-ci)/float(cimax-cimin))
       end
C
C
       real*4 function ct_standard( rgb, ci, cimin, cimax )
C      ----------------------------------------------------
C
C Function defining an standard colour-scale.
C
C defining:
C             x = (ci-cimin)/(cimax-cimin)
C then:
C                red   = sin(power(red)*x)**2,
C                green = sin(power(green)*x)**2,
C                blue  = sin(power(blue)*x)**2
*-

       include 'pg_cttools.inc'

       integer        rgb, ci, cimin, cimax
       real*4         x

       x  = (float(ci-cimin)/float(cimax-cimin))
       ct_standard = (sin( col_index(rgb)*x ))**2
       end
C
C

       subroutine pgplotctmod(type,rvals)
C      ----------------------------------
C
C Modification of the colour lookup table
C
C Given:
C   type
       integer   type
C   control values
       real*4    rvals(*)
C
C-
       include 'pg_cttools.inc'

       real*4         pgct01, ct_lin_grey, ct_enh_grey, ct_enh_col
       real*4         ct_lin_inv_grey, ct_enh_inv_grey
       external       pgct01, ct_standard
       external       ct_lin_grey, ct_enh_grey
       external       ct_enh_col, ct_lin_inv_grey, ct_enh_inv_grey

       integer        n

       do n=1,18
          ct_vals(n) = rvals(n)
       enddo
       if (type.eq.1) then
         call pgsct(pgct01)
       elseif (type.eq.2) then
         call pgsct(ct_standard)
       elseif (type.eq.3) then
         call pgsct(ct_lin_grey)
       elseif (type.eq.4) then
         call pgsct(ct_enh_grey)
       elseif (type.eq.5) then
         call pgsct(ct_lin_inv_grey)
       elseif (type.eq.6) then
         call pgsct(ct_enh_inv_grey)
       elseif (type.eq.7) then
         call pgsct(ct_enh_col)
       endif

       end


