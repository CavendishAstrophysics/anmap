
*$ Section 3: Map utlitity routines
*  --------------------------------

*+ invtan

       real function invtan(x,y)
C      -------------------------
C
C Do an Inverse Tangent taking care of zeros and quadrants
C
C Input
C   X,Y        -      R*4       -    two values for arctan(X/Y)
C
C Returned
C   INVTAN     -      via routine name - arctan(X/Y)
C
*-
       real*4    x, y
       real*4    eps, half_pi

       eps = 1.0e-05
       half_pi = 1.5707963268e+00
       if (abs(y).gt.eps) then
         invtan = atan2(x,y)
       else
         invtan = sign(half_pi,x)
       end if

       end
