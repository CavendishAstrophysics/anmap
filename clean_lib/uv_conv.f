

c+uv_conv

      subroutine uv_conv( map_uv, window, area )
C     ------------------------------------------
C
C Converts a uv window into a set of x-y coordinates
C
C Given:
C    Map UV limits
              integer     map_uv(4)
C    UV limits of window
              integer     window(4)

C Returned:
C    X-Y coordinates of u-v window
              integer     area(4)

C     NPR     5 July 1988
C-

      area(1) =  window(1) - map_uv(1) + 1
      area(2) =  window(2) - map_uv(1) + 1
      area(3) = -window(3) + map_uv(3) + 1
      area(4) = -window(4) + map_uv(3) + 1
      end
