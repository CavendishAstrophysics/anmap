*$ Routines to construct and use Theoretical Spectra
*  -------------------------------------------------
C
C
*+ synch_enqlim

       subroutine synch_enqlim(dimen_freq_max)
C      ---------------------------------------
C
C Enquire the maximum dimensionless frequency for the spectrum
C
C Returned:
C   maximum dimensionless frequency
       real*8     dimen_freq_max
*-
       include '../include/synch_defn.inc'

       dimen_freq_max = 10.0D+0**bdata(2)

       end
