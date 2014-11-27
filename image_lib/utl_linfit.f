

*$ Section 2: Data analysis routines
*  ---------------------------------

*+ utl_linfit

       subroutine utl_linfit (npts,x,y,sigmay,a,sigmaa,b,sigmab,chi2)
C      --------------------------------------------------------------
C
C Fit a straigth-line to the specified data
C
C Given:
C   number of data points
       integer  npts
C   input x, y data
       real*4   x(npts), y(npts)
C   error in Y-data
       real*4   sigmay(npts)
C Returned:
C   results of regression
       real*4   a, b
C   error on regression results
       real*4   sigmaa, sigmab
C   Chi**2
       real*4   chi2
C
C
C      P.R.Bevington (1969) "Data reduction and Error analysis
C                            for the Phyiscal sciences"
C      modified by J.P.Leahy, 18/7/83
C     Purpose:
C       make a least-squares fit to data with a straight line
C          Y = A + B*X
C-
      real sumw, sumx, sumy, sumx2, sumxy, weight, delta
      integer i

      sumw = 0.0
      sumx = 0.0
      sumy = 0.0
      sumx2 = 0.0
      sumxy = 0.0
      chi2 = 0.0
C
C        Accumulate weighted sums.
C
      do 50  i = 1,npts
        if (sigmay(i) .gt. 1e-4) goto 10
          write (6,6099) y,sigmay
 6099       format (10(' ',f9.5))
          chi2 = 100
          a = 0
          b = 0
          sigmaa = 0
          sigmab = 0
          goto 200
 10     weight = 1.0 / sigmay(i)**2
        sumw = sumw + weight
        sumx = sumx + weight * x(i)
        sumy = sumy + weight * y(i)
        sumx2 = sumx2 + weight * x(i)**2
        sumxy = sumxy + weight * x(i)*y(i)
 50   continue
C
C        Calculate coefficients and standard deviations.
C
      delta = sumw*sumx2 - sumx**2
      a = (sumx2*sumy - sumx*sumxy) / delta
      b = (sumxy*sumw - sumx*sumy ) / delta
      sigmaa = sqrt( sumx2 / delta )
      sigmab = sqrt( sumw  / delta )
      do 100  i = 1,npts
        chi2 = chi2 + ((a+b*x(i) - y(i)) / sigmay(i))**2
 100  continue
 200  return
      end
