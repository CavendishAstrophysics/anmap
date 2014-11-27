C
C
*+ restor

      subroutine restor(values,posns,ncc,map,nmi,nmj,im1,im2,jm1,jm2,
     +                  beam,nbi,nbj,ib1,ib2,jb1,jb2,bmax,ibc,jbc,
     +                  status)
C     ------------------------------------------------------------
C
C Restore sources to the residual map
C
C Input/Returned
C   MAP      -       R4(array)     -     Redidual/CLEAN map
C   BEAM     -       R4(array)     -     Restoring beam
C
C Input
C   VALUES   -       R4(array)     -     Source-List
C   POSNS    -       I2(2,array)   -     Source-List positions
C   NCC      -       I4            -     Number in source-list
C   NMI/J    -       I4            -     map size
C   NBI/J    -       I4            -     beam size
C   I/JM1/2  -       I4            -     region on map
C   I/JB1/2  -       I4            -     region on beam
C   BMAX     -       R4            -     beam height
C   I/JBC    -       I4            -     beam centre
C
C This routine restores the sources removed in CLEAN to the map using
C the clean beam.
C
*-
      integer     status
      integer     nmi, nmj, nbi, nbj, ncc, ion, jon
      integer     is1, is2, js1, js2, ibc, jbc, ib1, ib2, jb1, jb2,
     *            im1, im2, jm1, jm2
      integer     i, k, l, kk, ll

      real*4      map(nmi,nmj), beam(nbi,nbj)
      real*4      values(*), bmax, xsi
      integer*2   posns(2,*)

      if (status.ne.0) return

C check for entries in source list
      if (ncc .eq. 0) return

C restore
      do i=1,ncc
        ion=posns(1,i)
        jon=posns(2,i)
        is1=ib1-ibc+ion
        is2=ib2-ibc+ion
        js1=jb1-jbc+jon
        js2=jb2-jbc+jon
        if (is1 .lt. im1) is1=im1
        if (is2 .gt. im2) is2=im2
        if (js1 .lt. jm1) js1=jm1
        if (js2 .gt. jm2) js2=jm2
        xsi = values(i) / bmax
        do l=js1,js2
          ll=l+jbc-jon
          do k=is1,is2
            kk=k+ibc-ion
            map(k,l)=map(k,l) + beam(kk,ll)*xsi
          end do
        end do
      end do
      end
