C
C
*+ synch_2P_make

       subroutine synch_2P_make( f1, f2, status )
C      ------------------------------------------
C
C Construct a two-point lookup table for the current spectrum
C
C Given:
C   frequencies between which to define lookup table
       real*4     f1, f2
C Returned:
C   error status
       integer    status
C
C A lookup-table of spectral-index vs X is constructed for the current
C spectrum.  X is defined to be:
C
C      X   =     sqrt( f1 / f_break )
C
C Where f1 is the lower of f1 and f2 and f_break is the break frequency.
C
C PA, 13/02/92
*-

       include '../include/synch_defn.inc'
       include '../include/synch_errors.inc'

C local variables
       integer   n
       real*4    x1, x2
       real*4    synch_bfun

C check status on entry
       if (status.ne.0) return

C setup frequencies
       af1 = min ( f1, f2 )
       af2 = max ( f1, f2 )
       if (af1.eq.af2 .or. af1.le.0.0 .or. af2.le.0.0) then
         status = ill_syn2PD
         goto 999
       end if

C setup x-axis
       do n = 1,i2pt
         xx(n) = float(n)*1.0/float(i2pt)
       end do

C setup lookup table
       do n=1,i2pt
         x1 = xx(n)*xx(n)
         x2 = af2*x1/af1
         aa(n) = log( synch_bfun(x1,status) / synch_bfun(x2,status) ) /
     *           log(af2/af1)
       end do

999    continue
       call iocmd_err( status,'synch_2P_make','Illegal frequencies')
       end

