c     21/12/93   jps18   ver 6.2
c
c     HEADER.INC
c
c     Declarations for common block containing header information

      integer   ncloud,       ! NUMBER OF CLOUDS
     +          nstar,        ! NUMBER OF STARS
     +          itmax,        ! NUMBER OF TIME STEPS REQUIRED
     +          dump,         ! OUTPUT EVERY dump TIMESTEPS
     +          narm          ! NUMBER OF SPIRAL ARMS
      real      tstep,        ! SIMULATION TIME STEP
     +          a0,           ! TURBULENT GAS VELOCITY
     +          elas,         ! ELASTICITY OF CLOUD-CLOUD INTERATIONS
     +          vmax,         ! MAX GAIN IN SPEED FROM CLOUD-SNR INTERACTIONS
     +          mst,          ! CRITICAL MASS FOR STIMULATED STAR FORMATION
     +          msp,          ! CRITICAL MASS FOR SPONTANEOUS STAR FORMATION
     +          eff,          ! CLOUD DISRUPTION FACTOR
     +          omegap,       ! SPIRAL PATTERN SPEED
     +          p,            ! POWER OF SPIRAL SWITCH-OVER
     +          ang,          ! SPIRAL PITCH ANGLE
     +          amp,          ! SPIRAL AMPLITUDE
     +          r0            ! SPIRAL SWITCH-OVER RADIUS
      character source*12     ! NAME OF SOURCE FILE

      common /blk3/ tstep,a0,elas,vmax,mst,msp,eff,omegap,p,ang,amp,
     +              r0,ncloud,nstar,itmax,narm,dump,source
