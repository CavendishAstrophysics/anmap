C
C Basic image manipulation routines for images
C
C
C
       subroutine img_apply( def_in, def_out, din, op, va, s )
C      -------------------------------------------------------
C
C Apply a specified operation to the supplied (sub-)image 
C
C Given:
C   input definition record
       integer    def_in(*)
C   input (sub-)image
       integer    def_out(*)
C   input data
       real*4     din(*)
C   operation to apply
       integer    op
C   control data values
       real*4     va(*)
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       integer  i, j, k, t, v, ni, m, it
       real*4   b, xt

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = def_in(m)
       enddo
       b = blank
       do m=1,len_defn
         image_defn(m) = def_out(m)
       enddo
       if (op.eq.1) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).gt.0.0) then
                            din(ni) = log(din(ni))
                         else
                            din(ni) = blank
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.2) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).gt.0.0) then
                            din(ni) = log10(din(ni))
                         else
                            din(ni) = blank
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.3) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = exp(din(ni))
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.5) then
       if (op.eq.4) va(1) = 10.0
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = va(1)**(din(ni))
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.6) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = din(ni)**va(1)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.7) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = sin(din(ni))
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.8) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = cos(din(ni))
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.9) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = tan(din(ni))
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.10) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = asin(din(ni))
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.11) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = acos(din(ni))
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.12) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = atan(din(ni))
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.13) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = din(ni) * va(1)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.14) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = din(ni) + va(1)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.15) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            if (din(ni).lt.va(1)) then
                               din(ni) = 0.0
                            else
                               din(ni) = 1.0
                            endif
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.16) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            din(ni) = va(1)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.17) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            if (din(ni).lt.va(1) .or.
     *                          din(ni).gt.va(2) ) then
                               din(ni) = va(3)
                            endif
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.18) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         din(ni) = blank
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.19) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).eq.b) then
                            din(ni) = va(1)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.20) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b .and.din(ni).ne.0.0) then
                            din(ni) = 1.0/din(ni)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.21) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b .and.din(ni).ge.0.0) then
                            din(ni) = sqrt(din(ni))
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.22) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            xt = din(ni)
                            if (xt.gt.va(2)) xt = va(2)
                            it = (xt - va(1))/va(3)
                            din(ni) = va(1) + it*va(3)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.30) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            if (din(ni).ge.va(1) .and.
     *                          din(ni).le.va(2) ) then
                               din(ni) = va(3)
                            endif
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo
       elseif (op.eq.31) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            if (din(ni).ge.va(1) .and.
     *                          din(ni).le.va(2) ) then
                               din(ni) = din(ni) + va(3)
                            endif
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo
       elseif (op.eq.32) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            if (din(ni).ge.va(1) .and.
     *                          din(ni).le.va(2) ) then
                               din(ni) = din(ni) - va(3)
                            endif
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo
       elseif (op.eq.33) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            if (din(ni).ge.va(1) .and.
     *                          din(ni).le.va(2) ) then
                               din(ni) = din(ni) * va(3)
                            endif
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo
       elseif (op.eq.34) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         if (din(ni).ne.b) then
                            if (din(ni).ge.va(1) .and.
     *                          din(ni).le.va(2) ) then
                               din(ni) = din(ni) / va(3)
                            endif
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       endif
       end
C
C
       subroutine img_combine( def1, def_out, d1, op, def2, d2, s )
C      ------------------------------------------------------------
C
C Combine the intersection of the images def1/def_out and def2
C
C Given:
C   definition record image 1
       integer    def1(*)
C   subimage definition record
       integer    def_out(*)
C
C Updated:
C   input data image 1 (and output)
       real*4     d1(*)
C
C Given:
C   operator code
       integer    op
C
C   definition record image 2
       integer    def2(*)
C   input data image 2
       real*4     d2(*)
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       integer  i, j, k, t, v, n1, n2, m
       real*4   b1, b2

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = def1(m)
       enddo
       b1 = blank
       do m=1,len_defn
         image_defn(m) = def2(m)
       enddo
       b2 = blank
       do m=1,len_defn
         image_defn(m) = def_out(m)
       enddo
       n2 = 0
       if (op.eq.1) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      n1 = img_index( def1, i, j, k, t, v )
                      n2 = img_index( def2, i, j, k, t, v )
                      if (n1.gt.0 .and. n2.gt.0) then
                         d1(n1) = d2(n2)
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.2) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      n1 = img_index( def1, i, j, k, t, v )
                      n2 = img_index( def2, i, j, k, t, v )
                      if (n1.gt.0 .and. n2.gt.0) then
                         if (d1(n1).ne.b1  .and. d2(n2).ne.b2) then
                            d1(n1) = d1(n1) + d2(n2)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.3) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      n1 = img_index( def1, i, j, k, t, v )
                      n2 = img_index( def2, i, j, k, t, v )
                      if (n1.gt.0 .and. n2.gt.0) then
                         if (d1(n1).ne.b1  .and. d2(n2).ne.b2) then
                            d1(n1) = d1(n1) - d2(n2)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.4) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      n1 = img_index( def1, i, j, k, t, v )
                      n2 = img_index( def2, i, j, k, t, v )
                      if (n1.gt.0 .and. n2.gt.0) then
                         if (d1(n1).ne.b1  .and. d2(n2).ne.b2) then
                            d1(n1) = d1(n1) * d2(n2)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.5) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      n1 = img_index( def1, i, j, k, t, v )
                      n2 = img_index( def2, i, j, k, t, v )
                      if (n1.gt.0 .and. n2.gt.0) then
                         if (d1(n1).ne.b1  .and. d2(n2).ne.b2) then
                            if (d2(n2).ne.0.0) then
                              d1(n1) = d1(n1) / d2(n2)
                            else
                              d1(n1) = blank
                            endif
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       else if (op.eq.-1) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      n1 = img_index( def1, i, j, k, t, v )
                      n2 = n2 + 1
                      if (n1.gt.0 .and. n2.le.def2(2)) then
                         d1(n1) = d2(n2)
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.-2) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      n1 = img_index( def1, i, j, k, t, v )
                      n2 = n2 + 1
                      if (n1.gt.0 .and. n2.le.def2(2)) then
                         if (d1(n1).ne.b1  .and. d2(n2).ne.b2) then
                            d1(n1) = d1(n1) + d2(n2)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.-3) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      n1 = img_index( def1, i, j, k, t, v )
                      n2 = n2 + 1
                      if (n1.gt.0 .and. n2.le.def2(2)) then
                         if (d1(n1).ne.b1  .and. d2(n2).ne.b2) then
                            d1(n1) = d1(n1) - d2(n2)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.-4) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      n1 = img_index( def1, i, j, k, t, v )
                      n2 = n2 + 1
                      if (n1.gt.0 .and. n2.le.def2(2)) then
                         if (d1(n1).ne.b1  .and. d2(n2).ne.b2) then
                            d1(n1) = d1(n1) * d2(n2)
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (op.eq.-5) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      n1 = img_index( def1, i, j, k, t, v )
                      n2 = n2 + 1
                      if (n1.gt.0 .and. n2.le.def2(2)) then
                         if (d1(n1).ne.b1  .and. d2(n2).ne.b2) then
                            if (d2(n2).ne.0.0) then
                              d1(n1) = d1(n1) / d2(n2)
                            else
                              d1(n1) = blank
                            endif
                         endif
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       endif
       end
C
C
       subroutine img_combine2( def1, def_out, d1, op, def2, d2,
     *                          def3, d3, s )
C      ---------------------------------------------------------
C
C Combine the intersection of the images def1/def_out and def2
C
C Given:
C   definition record image 1
       integer    def1(*)
C   subimage definition record
       integer    def_out(*)
C
C Updated:
C   input data image 1 (and output)
       real*4     d1(*)
C
C Given:
C   operator code
       integer    op
C
C   definition record image 2
       integer    def2(*)
C   input data image 2
       real*4     d2(*)
C
C   definition record image 3
       integer    def3(*)
C   input data image 3
       real*4     d3(*)
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       integer  i, j, k, t, v, n1, n2, n3, m
       real*4   b1, b2, b3

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = def1(m)
       enddo
       b1 = blank
       do m=1,len_defn
         image_defn(m) = def2(m)
       enddo
       b2 = blank
       do m=1,len_defn
         image_defn(m) = def3(m)
       enddo
       b3 = blank
       do m=1,len_defn
         image_defn(m) = def_out(m)
       enddo
       n2 = 0
       if (op.eq.1) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      n1 = img_index( def1, i, j, k, t, v )
                      n2 = img_index( def2, i, j, k, t, v )
                      n3 = img_index( def3, i, j, k, t, v )
                      if (n1.gt.0 .and. n2.gt.0 .and. n3.gt.0) then
                        if (d2(n2).ne.b2 .and. d3(n3).ne.b3) then
                           d1(n1) = atan2( d2(n2), d3(n3) )
                        else
                           d1(n1) = blank
                        endif
                      else
                         d1(n1) = blank
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo
       endif
       end
