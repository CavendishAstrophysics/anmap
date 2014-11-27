C
C
       subroutine fun_gser(gamser,a,x,gln,istat)
C      -----------------------------------------
C
C Internal subroutine returning incomplete gamma function from series
C
C Given:
C  parameters of function
       real*4     a,x
C Returned:
C  value of function and log(Gamma(a))
       real*4     gamser,gln
C Updated:
C   error return code
       integer    istat
C
C P. Alexander, MRAO, Cambridge.
C Ref: Numerical recipes, Press et al., Cambridge. pg. 162
C-
       include '../include/utilities_errors.inc'

       integer    itmax, n
       real*4     eps, ap, sum, del
       parameter (itmax=100,eps=3.0E-7)

C functions used
       real*4     fun_gammln

       if (istat.ne.0) return
       gln = fun_gammln( a, istat )
       if (istat.ne.0) goto 999
       if (x.lt.0.0) then
         istat = ill_inputs
       else if (x.eq.0.0) then
         gamser = 0.0
       else
         ap = a
         sum = 1.0/a
         del = sum
         do n=1,itmax
           ap = ap + 1.0
           del = del*x/ap
           sum = sum + del
           if (abs(del).lt.abs(sum)*eps) goto 1
         end do
         istat = ill_precision
         goto 999
 1       gamser = sum*exp(-x+a*log(x)-gln)
       end if
 999   call utl_err( istat,'fun_gser',' ' )
       end
