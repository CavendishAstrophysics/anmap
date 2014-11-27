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
