*+ maxmin

       subroutine maxmin(map,m,n,i1,i2,j1,j2,xmax,imax,jmax,
     *                   xmin,imin,jmin,status)
C      -----------------------------------------------------
C
C find max and min on input map
C
C Input
C    MAP          -      R*4(array) -    map data
C    M,N          -      I4         -    range of map data
C    I1,I2        -      I4         -    range in I
C    J1,J2        -      I4         -    range in J
C
C Returned
C    XMAX         -      R4         -     maximum map-value in window
C    IMAX,JMAX    -      I4         -     max. position
C    XMIN         -      R4         -     minimum map-value in window
C    IMIN,JMIN    -      R4         -     min. position
C
C MAP(M,N) is examined in window I1-I2, J1-J2.
C XMAX is the maximum map-value in the window and is at IMAX,JMAX.
C XMIN is the minimum map-value in the window and is at IMIN,JMIN.
*-
       integer   status, m, n, i1, i2, j1, j2, imax, jmax, imin, jmin
       integer   i, j
       real*4    xmin, xmax
       real*4    x
       real*4    map(m,n)

       if (status.ne.0) return

       xmax=map(i1,j1)
       imax=i1
       jmax=j1
       xmin=xmax
       imin=imax
       jmin=jmax
       do j=j1,j2
         do i=i1,i2
           x=map(i,j)
           if(x.lt.xmin) then
             xmin=x
             imin=i
             jmin=j
           else if (x.gt.xmax) then
             xmax=x
             imax=i
             jmax=j
           end if
         end do
       end do
       end
C
C
*+ maxmin_map

       subroutine maxmin_map(map,m,n,i1,i2,j1,j2,xmax,imax,jmax,
     *                       xmin,imin,jmin,status)
C      ---------------------------------------------------------
C
C Scan a map and update XMAX and XMIN if appropriate
C
C Input
C    MAP          -      R*4(array) -    map data
C    M,N          -      I4         -    range of map data
C    I1,I2        -      I4         -    range in I
C    J1,J2        -      I4         -    range in J
C
C Returned
C    XMAX         -      R4         -     maximum map-value in window
C    IMAX,JMAX    -      I4         -     max. position
C    XMIN         -      R4         -     minimum map-value in window
C    IMIN,JMIN    -      R4         -     min. position
C
C The map is scanned and each map value tested against XMAX and XMIN.
C XMAX, IMAX, JMAX and XMIN, IMIN, JMIN are updtades if required, the
C routine requires that XMAX and XMIN should be set to either limiting
C values (if say the region is to be tested alone), or already set to
C max, min values for another region.
C
C The calling routine MUST set XMAX, XMIN before a call.
*-
       integer     status, m, n, i1, i2, j1, j2, imin, jmin, imax, jmax
       integer     i, j
       real*4      x, xmin, xmax
       real*4      map(m,n)

       if (status.ne.0) return

       do j=j1,j2
         do i=i1,i2
           x=map(i,j)
           if(x.lt.xmin)then
             xmin=x
             imin=i
             jmin=j
           end if
           if(x.gt.xmax)then
             xmax=x
             imax=i
             jmax=j
           end if
         end do
       end do
       end
C
C
*+ boxmin_map

       subroutine boxmin_map(map,m1,m2,i1,i2,j1,j2,max,imax,jmax,
     *                       min,imin,jmin,status)
C      ----------------------------------------------------------
C
C Scan a map for max/min excludinf "NOT-BOX"
C
C Input
C    MAP          -      R*4(array) -    map data
C    M,N          -      I4         -    range of map data
C    I1,I2        -      I4         -    range in I
C    J1,J2        -      I4         -    range in J
C
C Returned
C    XMAX         -      R4         -     maximum map-value in window
C    IMAX,JMAX    -      I4         -     max. position
C    XMIN         -      R4         -     minimum map-value in window
C    IMIN,JMIN    -      R4         -     min. position
C
C Subroutine to find max and min in window I1-I2;J1-J2
C Excludes portion N1-N2;NN1-NN2
*-
       integer         n1, n2, nn1, nn2
       common /notbox/ n1, n2, nn1, nn2
       integer   status
       integer   m1, m2, i1, i2, j1, j2, imin, jmin, imax, jmax
       integer   i, j

       real*4    map(m1,m2), max, min

       if (status.ne.0) return

C  Loop through the data and update if necessary
       do j=j1,j2
         do 10 i = i1,i2
          if(((i.gt.n1).and.(i.lt.n2)).and.
     *       ((j.gt.nn1).and.(j.lt.nn2))) goto 10
          if (map(i,j).gt.max) then
             max=map(i,j)
             imax=i
             jmax=j
          end if
          if (map(i,j).lt.min) then
             min=map(i,j)
             imin=i
             jmin=j
          end if
  10    continue
       end do
       end
C
C
*+ noise_map

       subroutine noise_map(map,n,m,i1,i2,j1,j2,xmean,xsd,npts,status)
C      ---------------------------------------------------------------
C
C Calculate the mean and noise in specified map window
C
C Input
C    MAP          -      R*4(array) -    map data
C    M,N          -      I4         -    range of map data
C    I1,I2        -      I4         -    range in I
C    J1,J2        -      I4         -    range in J
C
C Returned
C    XMEAN        -      R4         -    mean in window
C    XSD          -      R4         -    standard deviation in window
C    NPTS         -      I4         -    number of points in window
C
C  This routine calculates the mean and noise in the window I1-I2,J1-J2
C  of the input map.  No rejection is used.
*-
       integer    status, m, n, i1, i2, j1, j2, i, j, npts
       real*4     map(m,n), xmean, xsd, x
       real*4     xsum1, xsum2, fn

       xsum1=0.0
       xsum2=0.0
       npts=0
       do j=j1,j2
         do i=i1,i2
           x=map(i,j)
           xsum1=xsum1+x
           xsum2=xsum2+x*x
           npts=npts+1
         end do
       end do

C calculate statistics
       fn=float(npts)
       xmean=xsum1/fn
       xsd=sqrt((xsum2-xmean*xsum1)/(fn-1.0))
       end
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
C
C
*+ make_clbm

      subroutine make_clbm(beam,m,n,cbsig1,cbsig2,a,bmax,ibc,jbc,i1,i2,
     +                     j1,j2,sratio,status)
C     -----------------------------------------------------------------
C
C Calculate the clean beam for the restore process
C
C Input
C   M,N       -        I4       -      size beam
C   CBSIG1/2  -        R4       -      beam size (grid-points)
C   A         -        R4       -      position-angle (radians)
C   BMAX      -        R4       -      beam height
C
C Returned
C   BEAM      -        R4(array)-      beam array
C   I/JBC     -        I4       -      beam centre
C   I1,I2     -        I4       -      region on beam to use (I)
C   J1,J2     -        I4       -                "           (J)
C   SRATIO    -        R4       -      beam normalization factor
C
C This routine calculates the clean beam for the restoring process.
C The beam is made to be gaussian, and circular in the aperture
C projection.  CBSIG1,2 are the values of sigma for this beam, and are
C in units of grid-points.
C
C Also, the routine calculates the factor by which sums of numbers on a CLEAN
C map have to be multiplied in order to get the flux density (SRATIO).
C
*-
      integer     status, m, n, i1, i2, j1, j2, ibc, jbc
      real*4      beam(m,n), a, bmax, cbsig1, cbsig2, sratio
      real*4      cb1, cb2, x1, x2, bsum
      integer     na1, na2, i, j

C calculate geometrical quantities for the gaussian
      ibc = n/2
      jbc = m/2

      cb1=1.0/(cbsig1*cbsig1*2.0)
      cb2=1.0/(cbsig2*cbsig2*2.0)
      na1=6.324*cbsig1+2.5
      na2=6.324*cbsig2+2.5
      i1=ibc-max(na1,na2)
      i2=ibc+max(na1,na2)
      j1=jbc-max(na2,na1)
      j2=jbc+max(na2,na1)
C zero old array
      do j=1,n
        do i=1,m
          beam(i,j)=0.0
        end do
      end do

C Calculate beam
      bsum=0.0
      do j=j1,j2
        do i=i1,i2
          x1 = float(i-ibc)*cos(A) + float(j-jbc)*sin(A)
          x2 = float(j-jbc)*cos(A) - float(i-ibc)*sin(A)
          x1 = x1 * x1 * cb1
          x2 = x2 * x2 * cb2
          beam(i,j) = bmax*exp(-x1-x2)
          bsum=bsum+beam(i,j)
        end do
      end do

C determine normalization factor
      sratio=bmax/bsum

      end
C
C
*+ make_trbm

       subroutine make_trbm(beam,m,n,bmax,ibc,jbc,i1,i2,j1,j2,
     *                       sratio,status)
C      -------------------------------------------------------
C
C Make a restoring beam truncated at first zero.
C
C Input
C   M,N       -        I4       -      size beam
C   CBSIG1/2  -        R4       -      beam size (grid-points)
C   BMAX      -        R4       -      beam height
C   I/JBC     -        I4       -      beam centre
C   I1,I2     -        I4       -      region on beam to use (I)
C   J1,J2     -        I4       -                "           (J)
C
C Input/Returned
C   BEAM      -        R4(array)-      beam array
C
C Returned
C   SRATIO    -        R4       -      beam normalization factor
C
C This routine truncates the old dirty beam at the first zero.
C The beam outside the first zero is set equal to zero.
*-
       integer     status, m, n, i1, i2, j1, j2, ibc, jbc
       real*4      beam(1-ibc:m-ibc,1-jbc:n-jbc)
       real*4      bmax, sratio, bsum

       integer     i, ii, j, imax, imin, jmax, jmin
       integer     ib1, jb1, jb2

       if (status.ne.0) return

C  Assume elliptic beam to first order.
       imax=m-ibc
       imin=1-ibc
       jmax=n-jbc
       jmin=1-jbc

       do 10 j=0,jmax
         do i=0,imax
           if (beam(i,j) .le. 0) then
             do ii=i,imax
               beam(ii,j) = 0.0
               beam(ii,-j)= 0.0
             end do
             do ii= -i,imin,-1
               beam(ii,j) = 0.0
               beam(ii,-j)= 0.0
             end do
             goto 10
           end if
         end do
 10    continue

       bsum = 0.0
       ib1  = imin
       jb1  = jmin
       do j=jmin,jmax
         do i=imin,imax
           if (beam(i,j) .gt. 0.0) then
             bsum = bsum + beam(i,j)
             if(i.gt.ib1) ib1 = i
             if(j.gt.jb1) jb1 = j
           end if
         end do
       end do
       i1 = ibc - ib1
       i2 = ibc + ib1
       j1 = jbc - jb1
       j2 = jbc + jb2
       sratio = bsum / bmax

       end
C
C
*+ make_ccsl

       subroutine make_ccsl(values,posns,list,ncc,status)
C      -------------------------------------------------
C
C Construct the current source list
C
C Input/Updated
C   VALUES  -     R4(array)     -   source-list
C   POSNS   -     I2(2,array)   -   source-list positions
C   LIST    -     I2(array)     -   work space array
C   NCC     -     I4            -   number of sources
C
C
C Returned
C   STATUS  -     I4            -   error return
C
C The source list is compacted and multiple entries for a single
C grid point are summed.
*-
       real*4       values(*), flux
       integer*2    posns(2,*), list(*)
       integer      status, ncc, iin, i, i1, i2, j1, j2,
     *              n1, n2, n3
C
C
C Start by initialising the pointer in SAVE.
       iin=1
       do i=1,ncc
         list(i)=1
       end do

C initialise pointers
       n1=0
       n3=0
C
C Select the next point to be surveyed for flux density.  This forces the
C list to be by (approximate) order of flux density.
C
    1  continue
         n1=n1+1
         if (n1 .gt. ncc) goto 4
         if (list(n1) .lt. 0) goto 1
         flux=values(n1)
         i1=posns(1,n1)
         j1=posns(2,n1)
         list(n1)=-1
         n2=n1
         n3=n3+1
C
C N2 now runs through the rest of the stack VALUES selecting those points
C that have not been removed and have the same position as the start
C point.  The flux sum is incremented and the pointer in LIST is
C altered to show that a point is not to be used again.
    2    continue
           n2=n2+1
           if (n2 .gt. ncc) goto 3
             i2=posns(1,n2)
             if (i2 .ne. i1) goto 2
             j2=posns(2,n2)
             if (j2 .ne. j1) goto 2
             flux=flux+values(n2)
             list(n2)=-1
             goto 2
C
C N2 has run through the entire array.
    3      continue
           posns(1,n3) = i1
           posns(2,n3) = j1
           values(n3)  = flux
         goto 1
C
    4  continue
       ncc = n3
       end
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
C
C
*+ make_phbm

       subroutine make_phbm(beam,m,n,ibc,jbc,alpha,status)
C      ---------------------------------------------------
C
C modify the beam for a Prussian Hat clean
C
C Input/Returned
C   BEAM      -      R4(array)       -     beam to modify
C
C Input
C   M/N       -      I4              -     Beam size
C   I/JBC     -      R4              -     max on beam
C   ALPHA     -      R4              -     fractional hat height
*-
       integer    status, m, n, ibc, jbc

       real*4     beam(m,n), alpha

       if (status.ne.0) return

       beam(ibc,jbc) = beam(ibc,jbc) + alpha*beam(ibc,jbc)

       end
C
C
*+QINDXR1

       SUBROUTINE QINDXR1 (ARRAY, M, INDEX, N)
C      ---------------------------------------
C
C  Indexes a real array, using Quicksort.
C
C  Parameters:
C      ARRAY     real        data array
C      M         integer     length of ARRAY
C      INDEX     integer     index to data array
C      N         integer     length of index <= M
C
C  Sorts the index INDEX to the real array ARRAY so that on termination
C  the list (ARRAY(INDEX(I)), I = 1, N) is in ascending order. Uses the
C  Quicksort algorithm (the array is partitioned into successively smaller
C  subarrays, by moving the first element of each subarray to its correct
C  position, using exchanges).
C  The INDEX array is assumed sorted on entry and of length N <= M
C
C  (NPR, 11 July 1987)
C  (PA, 9 September 1988)
C
*-
       INTEGER N, M, INDEX(N)
       REAL    ARRAY(M),A
       INTEGER ISTACK(100)
       INTEGER I,I1,I2,IS,J,INDX
C
       IF (N.LT.2) RETURN
C
C  Begin with the whole array
C
       I1=1
       I2=N
       IS=1
       ISTACK(IS)=N+1
C
       DO WHILE (IS.GT.0)
         DO WHILE (I1.LT.I2)
C
C    Partition the subarray using its first element
C
           I=I1
           J=I2+1
           INDX=INDEX(I1)
           A=ARRAY(INDX)
    1      J=J-1
           IF (I.EQ.J) GOTO 3
           IF (ARRAY(INDEX(J)).GE.A) GOTO 1
           INDEX(I)=INDEX(J)
    2      I=I+1
           IF (I.EQ.J) GOTO 3
           IF (ARRAY(INDEX(I)).LE.A) GOTO 2
           INDEX(J)=INDEX(I)
           GOTO 1
    3      INDEX(I)=INDX
C
C    Store the partition index
C
           IF (I.LT.I2-1) THEN
             IS=IS+1
             ISTACK(IS)=I
           ELSEIF (IS.EQ.1) THEN
             ISTACK(IS)=I
           ENDIF
C
C    Take the left-hand part as the new sub-array
C
           I2=I-1
         ENDDO
C
C  Now take the most recent right-hand part as the new sub-array
C
         I1=ISTACK(IS)+1
         IS=IS-1
         I2=ISTACK(IS)-1
       ENDDO
C
       END





