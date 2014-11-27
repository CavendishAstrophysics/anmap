C
C
*+ sort_index

       subroutine sort_index(map,m,depth,index,len_index)
C      --------------------------------------------------
C
C Sort and reset the index
C
       integer    m, depth, index(1), len_index, ii, i
       real*4     map(1)
       integer    i1, i2, i3

C sort indexed array
       call qindxr1(map,m,index,len_index)

C find depth entries
       if (len_index.le.depth) then
         return
       end if

       i1 = 1
       i2 = len_index
       i3 = 0

       do while (i3.le.depth)

         if (map(index(i1)).gt.abs(map(index(i2))) ) then
           i1 = i1 + 1
         else
           i2 = i2 - 1
         end if
         i3 = i3 + 1

       end do

C re-write array
       ii = len_index + 1
       do i = i1,i3
         ii = ii - 1
         index(i) = index(ii)
       end do

C reset the length of the index array
       len_index = i3

       end
