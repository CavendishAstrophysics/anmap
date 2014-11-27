       double precision function fang(x)
       real*4            xxx
       real*8            x,xtt,xmm,dgamma,dgam2,xx
       real*4            synch_bfun
       integer           status
       common /locspn/   xtt,xmm,dgamma,dgam2
       xx=dsin(x)
       xxx=xtt*xx*xx*xx
       xx=xx**xmm
       fang=xx*synch_bfun(xxx,status)
       return
       end
