

*$ Section 1: Special Functions
*  ----------------------------

*+ fun_gammp

       real function fun_gammp( a, x, istat )
C      --------------------------------------
C
C Returns the incomplete gamma function P(a,x) by argument
C
C Given:
C   parameters for the function
       real*4       a, x
C Updated:
C   error return
       integer      istat
C
C The incomplete Gamma function P(a,x) is defined by:
C \[
C   P(a,x) = \frac{1}{\Gamma(a)} \int_{0}^{x} e^{-t}t^{a-1} dt
C \]
C for $a > 0$.

C ISTAT should be zero on entry.  The values set by this routine are:
C     ILL_INPUTS         == illegal ranges on input data.
C     ILL_PRECISION      == unable to calculate function to sufficient
C                           precision.  This error can be returned from
C                           one of the internal routines and results from
C                           a to large or not enough iterations within
C                           the respective routine.  This error MUST
C                           be reported.
C
C P. Alexander, MRAO, Cambridge.
C Ref: Numerical Recipes, Press et al., Cambridge. pg. 160.
C-

       include '../include/utilities_errors.inc'

C local variables
       real*4    gamser, gammcf, gln

       if (istat.ne.0) return

       if (x.lt.0.0 .or. a.le.0.0) then
         istat = ill_inputs
         fun_gammp = 0.0
       else
         if (x.lt.a+1.0) then
C .. use series representation
           call fun_gser(gamser,a,x,gln,istat)
           fun_gammp = gamser
         else
C .. use continued fraction
           call fun_gcf(gammcf,a,x,gln,istat)
C .. take complement
           fun_gammp = 1.0 - gammcf
         end if
       end if
       call utl_err( istat,'fun_gammp','Failed' )
       end
