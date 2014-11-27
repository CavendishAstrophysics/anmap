C
C
*+nearest_pt

      subroutine nearest_pt( x, y, a, n_pts, nearest, status )

C     Finds the nearest point in a given array.

C     Given:
C         Coordinates of given point
              integer     x, y
C         An array of cartesian positions and its size
              integer     n_pts, a(2,n_pts)

C     Returned:
C         Index in array of point nearest to the given one.
              integer     nearest
C     Error flag
              integer     status

C     If n_pts is less than 1 then nearest is returned as zero.
C-

      integer     i, distance, temp

      if (n_pts .le. 0) then
          nearest = 0
      else
          nearest  = 1
          distance = (x-a(1,1))*(x-a(1,1)) + (y-a(2,1))*(y-a(2,1))

          do i = 2, n_pts
              temp = (x-a(1,i))*(x-a(1,i))+(y-a(2,i))*(y-a(2,i))
              if (temp .lt. distance) then
                  distance = temp
                  nearest  = i
              end if
          end do
      end if

      return
      end
