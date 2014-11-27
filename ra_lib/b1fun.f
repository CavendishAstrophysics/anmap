C
C Functions used above....
       double precision function b1fun(x)
       real*8            x,xtt,xmm,dgamma,dgam2
       real*8            ffun
       common /locspn/   xtt,xmm,dgamma,dgam2
       b1fun=ffun(x)*((dsqrt(x)-dsqrt(xtt))**(dgamma-2.0))/dsqrt(x)
       return
       end
