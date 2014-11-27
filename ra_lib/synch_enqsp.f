C
C
*+ synch_enqsp

       subroutine synch_enqsp(injection_index_E,spectral_type)
C      -------------------------------------------------------
C
C Enquire the current injection index and spectral type
C
C Returned:
C    injection index of electron population == GAMMA
       real*4         injection_index_E
C    spectra type code
       integer        spectral_type
C
C The electron spectrum is assumed to be of the form:
C      N(E)dE = N0 E**-GAMMA dE
C The spectral type is a code as follows:
C      1      ==     KP spectrum aged angle averaged
C      2      ==     KP spectrum aged not angle averaged
C      3      ==     KP spectrum continuous injection angle averaged
C      4      ==     KP spectrum continuous injection not angle averaged
C      5      ==     JP spectrum
C
C The current values are returned
*-

       include '../include/synch_defn.inc'

       spectral_type = iflag
       injection_index_E = gamma

       end
