C
C
*+ fun_gammln

       real function fun_gammln( x, istat )
C      ------------------------------------
C
C Return the logarithm of the gamma function by argument
C
C Given:
C   parameter to the functions
       real*4        x
C Updated:
C   error return
       integer       istat
C
C The logarithm of the Gamma function $\ln(\Gamma(x))$ is defined by:
C \[
C   \Gamma(x) = \int_{0}^{\infty} e^{-z}t^{z-1} dt
C \]
C for $z > 0$.

C ISTAT should be zero on entry.  The only value set by this routine
C is to ILL_INPUTS for illegal ranges on input data.
C
C P. Alexander, MRAO, Cambridge.
C Ref: Numerical Recipes, Press et al., Cambridge. pg. 157.
C-

       include '../include/utilities_errors.inc'

C counters
       integer    j
C local variables (double precision arithmetic)
       real*8     cof(6), stp, half, one, fpf, xx, tmp, ser
       data       cof,stp / 76.18009173D+0, -86.50532033D+0,
     *                      24.01409822D+0, -1.231739516D+0,
     *                      0.120858003D-2, -0.536382D-5,
     *                      2.50662827465D+0 /
       data       half,one,fpf / 0.5D+0, 1.0D+0, 5.5D+0 /

       if (istat.ne.0) return
       if (x.lt.0) then
         istat = ill_inputs
       else if (x.eq.0.0) then
         fun_gammln = 1.0
       else
         xx = x-one
         tmp = xx+fpf
         tmp = (xx+half)*log(tmp)-tmp
         ser = one
         do j=1,6
           xx = xx + one
           ser = ser + cof(j)/xx
         end do
         fun_gammln = tmp+log(stp*ser)
       end if
       call utl_err( istat,'fun_gammln','Failed' )
       end
