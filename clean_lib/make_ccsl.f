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
