C
C
*+ iswap

       subroutine iswap(i1,i2)
C      -----------------------
C
C To exchange two integers
C
C Input
C   I1         I4        -      integer to swap with I2
C   I2         I4        -      integer to swap with I1
C
C Returned
C   I1
C   I2
C
C Swap over the integers
*-
       integer   i1, i2, ihold

       ihold=i1
       i1=i2
       i2=ihold
       end
