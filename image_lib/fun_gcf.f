C
C
       subroutine fun_gcf(gammcf,a,x,gln,istat)
C      ----------------------------------------
C
C Internal subroutine returning incomplete gamma function by continued
C fraction
C
C Given:
C  parameters of function
       real*4     a,x
C Returned:
C  value of function and log(Gamma(a))
       real*4     gammcf,gln
C Updated:
C   error return code
       integer    istat
C
C P. Alexander, MRAO, Cambridge.
C Ref: Numerical recipes, Press et al., Cambridge. pg. 162
C-
       include '../include/utilities_errors.inc'

       integer    itmax, n
       real*4     eps, g, gold, a0, a1, b0, b1, an, ana, anf, fac
       parameter (itmax=100,eps=3.0E-7)

C functions used
       real*4     fun_gammln

       if (istat.ne.0) return
       gln = fun_gammln( a, istat )
       if (istat.ne.0) goto 999
       if (x.lt.0.0) then
         istat = ill_inputs
       else if (x.eq.0.0) then
         gammcf = 0.0
       else
         gold = 0.0
         a0 = 1.0
         a1 = x
         b0 = 0.0
         b1 = 1.0
         fac = 1.0
         do n=1,itmax
           an = float(n)
           ana = an - a
           a0 = (a1+a0*ana)*fac
           b0 = (b1+b0*ana)*fac
           anf = an * fac
           a1 = x*a0 + anf*a1
           b1 = x*b0 + anf*b1
           if (a1.ne.0.0) then
             fac = 1.0/a1
             g = b1*fac
             if (abs((g-gold)/g).lt.eps) goto 1
             gold = g
           end if
         end do
         istat = ill_precision
         goto 999
 1       gammcf = g*exp(-x+a*log(x)-gln)
       end if
 999   call utl_err( istat,'fun_gcf',' ' )
       end
