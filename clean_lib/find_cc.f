

*+find_cc

      subroutine find_cc( map, m, n,
     *                    search_area, n_search, not_box,
     *                    lsort, sort_depth, sort_inner, sort_gate,
     *                    index,
     *                    map_val, x, y, ic, status                )

C     Returns the position to subtract the next clean component.
C
C     Input
C         Map array and bounds
              integer         m, n
              real            map(m,n)
C         Number of search areas and their bounds
              integer         n_search, search_area(4,n_search)
C         Flag set if not-box is being used.
              logical         not_box
C         Sort options
              logical         lsort
              integer         sort_depth, sort_inner
              real*4          sort_gate
C         Work space for sort
              integer         index(1)

C     Updated
C         Inner counter
              integer         ic

C     Returned:
C         Map value and x-y coordinates of next clean-component
              real            map_val
              integer         x, y
C         Error flag
              integer         status
C         Maximum length of index
              integer         max_index

C     NPR  6 July 1988
C     PA  15 September 1988
*-
      real        max, min
      integer     i, i2, k, l, len_index,
     *            x_max, x_min, y_max, y_min

      common /cln_hold_1/ len_index

      if (status.ne.0) return

      if (lsort) then
C .. use sorted list of image maxima for sort_inner iterations

        max_index = 2000
        if (ic.eq.0) then
          len_index = 0
          do i = 1,n_search
            do l = search_area(3,i),search_area(4,i)
              do k = search_area(1,i),search_area(2,i)
                if (abs(map(k,l)).gt.sort_gate) then
                  len_index = len_index + 1
                  index(len_index) = (l-1)*m + k
                end if
                if (len_index.eq.max_index) then
                  call sort_index(map,m*n,sort_depth,index,len_index)
                end if
              end do
            end do
          end do
          call sort_index(map,m*n,sort_depth,index,len_index)
          ic = sort_inner
        end if

        max = -1.0E+30
        do i = 1,len_index
          l  = index(i)/m + 1
          k  = index(i) - (l-1)*m
          if (abs(map(k,l)) .gt. max) then
            max = abs(map(k,l))
            i2 = index(i)
          end if
        end do
        y = i2/m + 1
        x = i2 - (y-1)*m
        map_val = map(x,y)
        ic = ic - 1

      else
C .. normal clean seach for next CC
        max = -1.0E+30
        min =  1.0E+30

        do i = 1, n_search
          if (not_box) then
            call boxmin_map( map, m, n,
     *                       search_area(1,i), search_area(2,i),
     *                       search_area(3,i), search_area(4,i),
     *                       max, x_max, y_max,
     *                       min, x_min, y_min, status           )
          else
            call maxmin_map( map, m, n,
     *                       search_area(1,i), search_area(2,i),
     *                       search_area(3,i), search_area(4,i),
     *                       max, x_max, y_max,
     *                       min, x_min, y_min, status           )
          end if
        end do

        if (abs(max) .ge. abs(min)) then
          map_val = max
          x       = x_max
          y       = y_max
        else
          map_val = min
          x       = x_min
          y       = y_min
        end if

      end if

      end
