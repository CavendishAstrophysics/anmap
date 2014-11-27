C
C
*+ make_phbm

       subroutine make_phbm(beam,m,n,ibc,jbc,alpha,status)
C      ---------------------------------------------------
C
C modify the beam for a Prussian Hat clean
C
C Input/Returned
C   BEAM      -      R4(array)       -     beam to modify
C
C Input
C   M/N       -      I4              -     Beam size
C   I/JBC     -      R4              -     max on beam
C   ALPHA     -      R4              -     fractional hat height
*-
       integer    status, m, n, ibc, jbc

       real*4     beam(m,n), alpha

       if (status.ne.0) return

       beam(ibc,jbc) = beam(ibc,jbc) + alpha*beam(ibc,jbc)

       end
