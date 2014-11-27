C
C
*+ synch_bfun

       real function synch_bfun(x,status)
C      ----------------------------------
C
C Return the value of the theoretical spectrum
C
C Input:
C    dimensionless frequency
       real*4      x
C Returned:
C    status
       integer     status
C
C returns from a chebychev parametrization the value of the
C current theoretical spectrum.
C
*-
       include '../include/synch_defn.inc'
       include '../include/synch_errors.inc'
C
C Real*8 variales for the NAG routine
       real*8     x1, x2, x0, y0, xval, xx1
C Logical flag indicating use of low-x formula
       logical    low_x
C output unit for error messages
       integer    iout
C error flag used by NAG
       integer    ifail

C check status on entry
       if (status.ne.0 .and. status.ne.1) return

C initialise variables
       low_x = .false.
       x1=bdata(1)
       x2=bdata(2)
       x0=log10(x)

C Check range
       if (x0.gt.x2) then
         if (status.ne.1) then
           call io_enqout(iout)
           call io_wrout('***(SYNCH_BFUN) X out of range')
           call io_wrout('***(SYNCH_BFUN) X : LOG10(X) : LOG(MIN/MAX)')
           write (iout,*)'***(SYNCH_BFUN) ', X,X0,X1,X2
         endif
         synch_bfun=0.0
         status = ill_synlim
         return
       end if
       status = 0

C Get log value using E02AEF
       ifail=1
       if (x0.lt.x1) then
         x0=x1+1.0D+0
         low_x=.true.
       end if
       xval=((x0-x1)-(x2-x0))/(x2-x1)
       call e02aef(kpb1,bak(1),xval,y0,ifail)
       if (ifail.ne.0) then
         call io_enqout(iout)
         call io_wrout('***(SYNCH_BFUN) Error calling e02aef')
         write(iout,*)'***(SYNCH_BFUN) non-zero IFAIL = ',ifail
         status = ill_synNAG
         return
       end if

C Set value
       synch_bfun=10.0**y0
       if (low_x) then
         xx1=10.0**x0
         synch_bfun=synch_bfun*((x/xx1)**((1.0-gamma)/2.0))
       end if
       call iocmd_err(status,'SYNCH_BFUN',' ')
       end

