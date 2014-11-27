c include-file bfield:incl
c
c creates and declares the "bfield" common block

c c_zero      R*4 pre-multipling constant (other units => nT)
c k           R*4 E(heavy ions) = k*E(electrons)
c f           R*4 filling factor
c nu_zero     R*4 centre frequency of observation (GHz)
c nu_lo       R*4 lower integration limit for CR energy (GHz)
c nu_hi       R*4 upper     "         "    "   "    "     "
c alpha       R*4 object spectral index
c z           R*4 redshift

      real*4       c_zero,k,f,nu_zero,nu_lo,nu_hi,alpha,z
      parameter   (c_zero = 5.67)
      common /bfield/ k,f,nu_zero,nu_lo,nu_hi,alpha,z


