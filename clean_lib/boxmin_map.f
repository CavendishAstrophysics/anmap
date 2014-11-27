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
