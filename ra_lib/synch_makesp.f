C
C
*+ synch_makesp

       subroutine synch_makesp(status)
C      -------------------------------
C
C Routine to produce theoretical spectra
C
C Returned:
C     Status
       integer    status
C
C Construct a theoretical spectrum and store the results in the
C standard catalogue of synchrotron spectra
C
*-

       include '../include/synch_defn.inc'
C
C Real*8 variables for the NAG interface...
       real*8     xtt,xmm,dgamma,dgam2,dalpha,
     >            x1,tmin,pi,errabs,errrel,abserr
       real*8     xl(iars1),bl(iars1),xtb(iars1),bb(iars1)
       real*4     xstep, xunder
       integer    j, jj, k, kk, kplus1, i, ihold

C
C Local common to talk to NAG called routines
       common /locspn/ xtt,xmm,dgamma,dgam2
       data            pi/3.14159265d+0/
       real*8          b1fun,fang,fang2
       external        b1fun,fang,fang2

C check status on entry
       if (status.ne.0) return

C Initialise some bits
       xstep=9.691077
       xunder=iars1+1
       xstep=xstep/xunder
       do 5 j=1,iars1
         xt(j)=10**(-8.0 + float(j)*xstep)
  5    continue
       kpb1=30
       kplus1=9
       isize=iars1

C set injection index values...
       gam2 = (1.0-gamma)/2.0
       alpha=-gam2
       dgamma=gamma
       dgam2=gam2
       dalpha=alpha
       xmm=gamma*2.0

C Set error tolerances for the integrations
       errrel=0.025
       errabs=0.0

C Calculate spectra iniatally without any integration over angle
       do 10 k = 1,isize
         xtt=xt(k)
         call d01bdf(b1fun,xt(k),100.0*xt(isize),errabs,errrel,
     >               x1,abserr)
         b(k)=x1*(xt(k)**gam2)
10     continue

C Parametrize this
       jj=0
       do 20 k=1,isize
         if (b(k).gt.1.0e-30) then
          jj=jj+1
          bl(jj)=log10(b(k))
          xl(jj)=log10(xt(k))
         end if
20     continue
       isize=jj

C Do the fit...
       call chebr(xl,bl,isize,kpb1,bak(1))
       bdata(1)=xl(1)
       bdata(2)=xl(isize)
       bdata(3)=gamma
       bdata(4)=iflag

C If necessary do the integration over angle...
       if (iflag.eq.1.or.iflag.eq.2) then
         do 30 k=3,isize
           kk=k-2
           xtb(kk)=xt(k)
           xtt=xt(k)
           call d01bdf(fang,0.001d+0,pi/2.0d+0,errabs,
     >                 errrel,bb(kk),abserr)
  30     continue
         isize=isize-2
         do 40 k=1,isize
           xt(k)=xtb(k)
           b(k)=bb(k)
  40     continue
       else if (iflag.ge.5) then
         ihold=isize-2
         do 50 i=1,ihold
           xtt=xt(i)
           tmin=dasin(xtt/xt(isize))
           call d01bdf(fang2,tmin,pi/2.0d+0,errabs,
     >                 errrel,b(i),abserr)
  50     continue
         isize=ihold
       else
         isize=isize-2
       end if

C Parametrize...
       jj=0
       do 60 k=1,isize
         if (b(k).gt.1.0e-30) then
          jj=jj+1
          bl(jj)=log10(b(k))
          xl(jj)=log10(xt(k))
         end if
60     continue
       isize=jj

       call chebr(xl,bl,isize,kpb1,bak(1))
       bdata(1)=xl(1)
       bdata(2)=xl(isize)
       bdata(3)=gamma
       bdata(4)=iflag

       return
       end
