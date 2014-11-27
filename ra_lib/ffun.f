       double precision function ffun(x)
       real*8   x, a(9), x1, x2, xd, x0, y0
       integer  ifail
       data a/-6.9257d+0,-5.3079d+0,-4.8307d+0,-3.2329d+0,
     >        -1.88096d+0,-9.64707d-1,-4.18413d-1,-1.60413d-1,
     >        -8.42820d-2/
       real*8   pi, xval
       integer  kplus1
       data     pi/3.1415927d+0/
       x1=-4.0d+0
       x2=1.691077d+0
       xd=x2-x1
       x0=log10(x)

C Check range
       if (x0.lt.x1) then
         ffun=2.15*(x**(1.0/3.0)) -1.58d+0*x
       else if (x.gt.4.0d+0) then
         ffun=dsqrt(x*pi/2.0d+0)*exp(-x)*
     >        (1.0d+0+(55.0/(72.0*x))-10151.0/(10368.0*x*x))
       else
         xval=((x0-x1)-(x2-x0))/xd
         ifail=1
         kplus1=9
         call e02aef(kplus1,a,xval,y0,ifail)
         if (ifail.ne.0) then
            call io_wrout('***(FFUN) Error calling E02AEF')
            print *,'***(FFUN) Non-zero IFAIL = ',ifail
            call io_wrout(
     *           '***(FFUN) Warning: set ffun=0.0 and continuing')
         end if
         ffun=10.0d+0**y0
       end if
       return
       end
