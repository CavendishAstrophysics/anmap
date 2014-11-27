       subroutine ttt( a, n1, n2 )
       integer n1,n2,i,j
       real*4  a(n1,n2)

       do i=1,n1
         do j=1,n2
            s = s + a(i,j)
         enddo
       enddo
       end
