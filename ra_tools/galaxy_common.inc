c    30/08/93  jps18   ver 7.0
c
c     COMMON.INC
c
c     Declarations for main data common blocks

      integer   nclmax,            ! MAX NUMBER OF CLOUD PARTICLES
     +          nstmax             ! MAX NUMBER OF STAR PARTICLES
      parameter (nclmax=32750,nstmax=32750)
      integer   clage(nclmax),     ! AGES OF CLOUD PARTICLES
     +          stage(0:nstmax)    ! AGES OF STAR PARTICLES
      real      cloud(7,nclmax),   ! COORDINATES AND MASSES OF CLOUDS
     +          star(6,nstmax)     ! COORDINATES OF STARS

      common /clblk/ cloud,clage
      common /stblk/ star,stage

c     Declarations for circular rotation speed table

      real      vrot(nclmax)

      common /vrotblk/ vrot
