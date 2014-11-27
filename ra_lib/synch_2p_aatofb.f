C
C
*+ synch_2P_aatofb

       subroutine synch_2P_aatofb( f1, f2, laa, fb, status )
C      -----------------------------------------------------
C
C Determine break frequency from laa
C
C Given:
C   frequencies defining the spectral index
       real*4     f1, f2
C   spectral index
       real*4     laa
C Returned:
C      break frequency
       real*4     fb
C   error status
       integer    status
C
C The lookup-table of spectral-index vs X is used to determine the break
C frequency from the supplied values of spectral index.
C X is defined to be:
C
C      X   =     sqrt( f1 / fb )
C
C Where f1 is the lower of f1 and f2 and fb is the break frequency.
C
C PA, 13/02/92
*-

       include '../include/synch_defn.inc'
       include '../include/synch_errors.inc'

C local variables
       integer   n, nn
       real*4    lf1, lf2, frac

C check status on entry
       if (status.ne.0) return

C setup frequencies
       if (f1.lt.0.0 .or. f2.lt.0.0) then
         f1 = af1
         f2 = af2
       endif
       lf1 = min( f1, f2 )
       lf2 = max( f1, f2 )
       if (lf1.eq.0.0 .or. lf2.eq.0.0) return
       if (lf1.ne.af1 .or. lf2.ne.af2) then
         call synch_2p_make( lf1, lf2, status )
         if (status.ne.0) goto 999
       end if

C search in lookup table for value of laa
       if (laa.le.aa(1)) then
         nn = 1
       else
         nn = i2pt
         do n=1,i2pt
           if (laa.le.aa(n)) then
             nn = n-1
             goto 1
           end if
         end do
       end if

1      continue
       if (nn.eq.1 .or. nn.eq.i2pt) then
         fb = f1 / (xx(nn)*xx(nn))
       else
         frac = (laa-aa(nn))/(aa(nn+1) - aa(nn))
         fb = f1 / ( (xx(nn)+frac*(xx(nn+1)-xx(nn)))**2 )
       end if

999    continue
       call iocmd_err( status,'synch_2P_aatofb',' ')
       end


