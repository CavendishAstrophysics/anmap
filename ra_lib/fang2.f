       double precision function fang2(x)
       real*4            xxx
       real*8            x,xtt,xmm,dgamma,dgam2,xx
       real*4            synch_bfun
       integer           status
       common /locspn/   xtt,xmm,dgamma,dgam2
       xx=dsin(x)
       xxx=xtt/xx
       fang2=synch_bfun(xxx,status)*xx*xx
       return
       end
