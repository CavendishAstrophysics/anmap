C
C
*+ subtrc

       subroutine subtrc(map,m1,m2,im1,im2,jm1,jm2,beam,n1,n2,
     +                   ib1,ib2,jb1,jb2,xmax,ihold,jhold,bmax,
     +                   ibc,jbc,f1,status)
C      --------------------------------------------------------
C
C perform subtraction of a clean component from the map
C
C Input/Returned
C    MAP      -     R4(array)       -     map array
C
C Input
C    M1/2     -     I4              -     map size
C    BEAM     -     R4(array)       -     Beam array
C    N1/2     -     I4              -     beam size
C    I/JM/B1/2-     I4              -     map/beam regions
C    XMAX     -     R4              -     peak
C    I/JHOLD  -     I4              -     peak position
C    BMAX     -     R4              -     maximum on beam
C    I/JBC    -     I4              -     position of beam max
C    F1       -     R4              -     fraction of peak to remove
C
C This subroutine subtracts a proportion F1 of a source of size XMAX
C at (IHOLD,JHOLD) on MAP using the beam stored in BEAM.
C The beam centre is at (IBC,JBC) and the beam-peak value is BMAX.
C Only the window IM1-IM2, JM1-JM2 of MAP and IB1-IB2, JB1-JB2 of
C BEAM are used.
C
*-
       integer  status, m1, m2, n1, n2,
     *          im1, im2, jm1, jm2, ib1, ib2 ,jb1, jb2,
     *          ibc, jbc, ihold, jhold
       real*4   map(m1,m2), beam(n1,n2)
       real*4   xmax, bmax, f1, f2
       integer  is1, is2, js1, js2, i, j, i1, j1

       if (status.ne.0) return

       f2=f1*xmax/bmax
       is1=ib1+ihold-ibc
       is2=ib2+ihold-ibc
       if (im1 .gt. is1) is1=im1
       if (im2 .lt. is2) is2=im2
       js1=jb1+jhold-jbc
       js2=jb2+jhold-jbc
       if (jm1 .gt. js1) js1=jm1
       if (jm2 .lt. js2) js2=jm2
       do j=js1,js2
         j1=j-jhold+jbc
         do i=is1,is2
           i1=i-ihold+ibc
           map(i,j)=map(i,j) - f2*beam(i1,j1)
         end do
       end do
       end
