C
C Statistics and analysis image manipulation routines
C
C
       subroutine img_stats( defn, def1, din, gates, ist, rst, s )
C      -----------------------------------------------------------
C
C Determine image statistics in the sub-image defined by def1
C
C Given:
C   definition record of image
       integer    defn(*)
C   definition record of sub-image
       integer    def1(*)
C   input data
       real*4     din(*)
C   gates --- gate value and absolute gate
       real*4     gates(4)
C
C Returned:
C   statistics
       integer    ist(11)
       real*4     rst(4)
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       integer  i, j, k, t, v, ni, m
       real*4   b

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = defn(m)
       enddo
       b = blank
       do m=1,len_defn
         image_defn(m) = def1(m)
       enddo
       ist(11) = 0
       rst(1) = 1.0E+30
       rst(2) = -1.0E+30
       rst(3) = 0.0
       rst(4) = 0.0
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( defn, i, j, k, t, v )
                      if (ni.gt.0) then
                         if ( (din(ni).ne.b) .and.
     *                        (din(ni).ge.gates(1)) .and.
     *                        (abs(din(ni)).ge.gates(2)) .and.
     *                        (din(ni).ge.gates(3)) .and.
     *                        (din(ni).le.gates(4)) ) then
                            ist(11) = ist(11) + 1
                            rst(3) = rst(3) + din(ni)
                            rst(4) = rst(4) + din(ni)*din(ni)
                            if (din(ni).lt.rst(1)) then
                               rst(1) = din(ni)
                               ist(1) = i
                               ist(2) = j
                               ist(3) = k
                               ist(4) = t
                               ist(5) = v
                            endif
                            if (din(ni).gt.rst(2)) then
                               rst(2) = din(ni)
                               ist(6) = i
                               ist(7) = j
                               ist(8) = k
                               ist(9) = t
                               ist(10) = v
                            endif
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo
       if (ist(11).gt.0) then
          rst(3) = rst(3)/float(ist(11))
          rst(4) = sqrt(rst(4)/float(ist(11)) - rst(3)*rst(3))
       endif
       end
C
C
C
C
       subroutine img_maxmin( defn, def1, din, ist, rst, s )
C      -----------------------------------------------------
C
C Determine image max/min in the sub-image defined by def1
C
C Given:
C   definition record of image
       integer    defn(*)
C   definition record of sub-image
       integer    def1(*)
C   input data
       real*4     din(*)
C
C Returned:
C   statistics
       integer    ist(11)
       real*4     rst(4)
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       integer  i, j, k, t, v, ni, m
       real*4   b

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = defn(m)
       enddo
       b = blank
       do m=1,len_defn
         image_defn(m) = def1(m)
       enddo
       rst(1) = 1.0E+30
       rst(2) = -1.0E+30
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( defn, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            rst(3) = rst(3) + din(ni)
                            rst(4) = rst(4) + din(ni)*din(ni)
                            if (din(ni).lt.rst(1)) then
                               rst(1) = din(ni)
                               ist(1) = i
                               ist(2) = j
                               ist(3) = k
                               ist(4) = t
                               ist(5) = v
                            endif
                            if (din(ni).gt.rst(2)) then
                               rst(2) = din(ni)
                               ist(6) = i
                               ist(7) = j
                               ist(8) = k
                               ist(9) = t
                               ist(10) = v
                            endif
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo
       end
C
C
       subroutine img_sum( defn, def1, din, gates, rst, s )
C      ----------------------------------------------------
C
C Determine image pixel sum in the sub-image defined by def1
C
C Given:
C   definition record of image
       integer    defn(*)
C   definition record of sub-image
       integer    def1(*)
C   input data
       real*4     din(*)
C   gates --- gate value and absolute gate
       real*4     gates(4)
C
C Returned:
C   sum
       real*4     rst
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       integer  i, j, k, t, v, ni, m
       real*4   b

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = defn(m)
       enddo
       b = blank
       do m=1,len_defn
         image_defn(m) = def1(m)
       enddo
       rst = 0.0
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( defn, i, j, k, t, v )
                      if (ni.gt.0) then
                         if ( (din(ni).ne.b) .and.
     *                        (din(ni).ge.gates(1)) .and.
     *                        (abs(din(ni)).ge.gates(2)) .and.
     *                        (din(ni).ge.gates(3)) .and.
     *                        (din(ni).le.gates(4)) ) then
                            rst = rst + din(ni)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo
       rst = rst * norm
       end
