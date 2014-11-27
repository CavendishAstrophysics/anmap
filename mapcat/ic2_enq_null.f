

*+ic2_enq_null

       subroutine ic2_enq_null( null, status )
C      ---------------------------------------

C     Returns the undefined pixel value from current redtape.
C
C     Given:
C         None.
C
C     Returned:
C         The external value used to represent undefined pixels.
              real*4      null
C         Status value - must be zero on entry - otherwise unchanged
              integer     status
C
C
C-
      call ennull( null, status )
      end
