C
C
*+ synch_age

       real function synch_age(break_frequency,Bfield)
C      -----------------------------------------------
C
C To calculate age given a break frequency
C Input:
C    break frequency
       real*4    break_frequency
C    Magnetic fied
       real*4    Bfield
C
C The age is in units of 10**7 yrs
*-
C local variables
       real*4    gamma
       integer   iflag
C
       call synch_enqsp(gamma,iflag)
       synch_age=3.35/(sqrt(break_frequency)*sqrt(Bfield**3))
       if (iflag.eq.5) synch_age=synch_age*3.0/2.0

       end
