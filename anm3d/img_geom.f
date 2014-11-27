C
C Image routines to provide geometric transformations
C
C
C
       subroutine img_subim( def_in, din, def_out, dout, s )
C      -----------------------------------------------------
C
C Extract the sub-image defined by def_out from the image def_in
C
C Given:
C   input definition record
       integer    def_in(*)
C   input data
       real*4     din(*)
C   output definition record
       integer    def_out(*)
C
C Returned:
C   output data
       real*4     dout(*)
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       integer  i, j, k, t, v, ni, no, m

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = def_out(m)
       enddo
       no = 0
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      if (ni.gt.0) then
                         no = no + 1
                         dout(no) = din(ni)
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo
       end
C
C
       subroutine img_project( defn, din, axis, dout, s )
C      --------------------------------------------------
C
C Project an image by averaging missing dimensions
C
C Given:
C   definition record of image
       integer    defn(*)
C   input data
       real*4     din(*)
C   axis down which to projected image
       integer    axis
C
C Returned:
C   output data
       real*4     dout(*)
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       integer  i, j, k, t, v, ni, n, m, ib

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = defn(m)
       enddo
       n = 0
       if (axis.eq.5) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1, x2
                   n = n + 1
                   dout(n) = 0.0
                   ib = 0
                   do v = v1,v2
                      ni = img_index( defn, i, j, k, t, v )
                      if (din(ni).eq.blank) ib = 1
                      dout(n) = dout(n) + din(ni)
                   enddo
                   if (ib.eq.1) dout(n) = blank
                enddo
             enddo
          enddo
       enddo

       elseif (axis.eq.4) then
       do k = z1, z2
          do j = y1, y2
             do i = x1, x2
                do v = v1,v2
                   n = n + 1
                   dout(n) = 0.0
                   ib = 0
                   do t = t1,t2
                      ni = img_index( defn, i, j, k, t, v )
                      if (din(ni).eq.blank) ib = 1
                      dout(n) = dout(n) + din(ni)
                   enddo
                   if (ib.eq.1) dout(n) = blank
                enddo
             enddo
          enddo
       enddo

       elseif (axis.eq.3) then
       do t = t1, t2
          do j = y1, y2
             do i = x1, x2
                do v = v1,v2
                   n = n + 1
                   dout(n) = 0.0
                   ib = 0
                   do k = z1,z2
                      ni = img_index( defn, i, j, k, t, v )
                      if (din(ni).eq.blank) ib = 1
                      dout(n) = dout(n) + din(ni)
                   enddo
                   if (ib.eq.1) dout(n) = blank
                enddo
             enddo
          enddo
       enddo

       elseif (axis.eq.2) then 
       do t = t1, t2
          do k = z1, z2
             do i = x1, x2
                do v = v1,v2
                   n = n + 1
                   dout(n) = 0.0
                   ib = 0
                   do j = y1,y2
                      ni = img_index( defn, i, j, k, t, v )
                      if (din(ni).eq.blank) ib = 1
                      dout(n) = dout(n) + din(ni)
                   enddo
                   if (ib.eq.1) dout(n) = blank
                enddo
             enddo
          enddo
       enddo

       elseif (axis.eq.1) then
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do v = v1,v2
                   n = n + 1
                   dout(n) = 0.0
                   ib = 0
                   do i = x1,x2
                      ni = img_index( defn, i, j, k, t, v )
                      if (din(ni).eq.blank) ib = 1
                      dout(n) = dout(n) + din(ni)
                   enddo
                   if (ib.eq.1) dout(n) = blank
                enddo
             enddo
          enddo
       enddo


       endif
       end
C
C
       subroutine img_flip( defn, din, axis, s )
C      -----------------------------------------
C
C Flip an image about a specified axis
C
C Given:
C   definition record of image
       integer    defn(*)
C   input data
       real*4     din(*)
C   axis down which to projected image
       integer    axis
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       real*4   val
       integer  i, j, k, t, v, n1, n2, mm, m

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = defn(m)
       enddo
       if (axis.eq.4) then
       mm = (t1 + t2 - 1)/2
       do k = z1, z2
          do j = y1, y2
             do i = x1, x2
                do v = v1,v2
                   m = t2
                   do t = t1,mm
                      n1 = img_index( defn, i, j, k, t, v )
                      n2 = img_index( defn, i, j, k, m, v )
                      val = din(n1)
                      din(n1) = din(n2)
                      din(n2) = val
                      m = m - 1
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (axis.eq.3) then
       mm = (z1 + z2 - 1)/2
       do t = t1, t2
          do j = y1, y2
             do i = x1, x2
                do v = v1,v2
                   m = z2
                   do k = z1,mm
                      n1 = img_index( defn, i, j, k, t, v )
                      n2 = img_index( defn, i, j, m, t, v )
                      val = din(n1)
                      din(n1) = din(n2)
                      din(n2) = val
                      m = m - 1
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (axis.eq.2) then 
       mm = (y1 + y2 - 1)/2
       do t = t1, t2
          do k = z1, z2
             do i = x1, x2
                do v = v1,v2
                   m = y2
                   do j = y1,mm
                      n1 = img_index( defn, i, j, k, t, v )
                      n2 = img_index( defn, i, m, k, t, v )
                      val = din(n1)
                      din(n1) = din(n2)
                      din(n2) = val
                      m = m - 1
                   enddo
                enddo
             enddo
          enddo
       enddo

       elseif (axis.eq.1) then
       mm = (x1 + x2 - 1)/2
       do t = t1, t2
          do k = z1, z2
             do j = y1, y2
                do v = v1,v2
                   m = x2
                   do i = x1,mm
                      n1 = img_index( defn, i, j, k, t, v )
                      n2 = img_index( defn, m, j, k, t, v )
                      val = din(n1)
                      din(n1) = din(n2)
                      din(n2) = val
                      m = m - 1
                   enddo
                enddo
             enddo
          enddo
       enddo


       endif
       end
C
C
       subroutine img_transpose( defn, din, order, def_out, dout, s )
C      --------------------------------------------------------------
C
C Flip an image about a specified axis
C
C Given:
C   definition record of image
       integer    defn(*)
C   input data
       real*4     din(*)
C   order of axes in transposed image
       integer    order(*)
C
C Returned:
C   output definition record
       integer    def_out(*)
C   output data
       real*4     dout(*)
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       integer  m(4), i1(4), i2(4), v, n, ni
       integer  m1, m2, m3, m4

       if (s.ne.0) return

       do n=1,len_defn
         image_defn(n) = defn(n)
       enddo
       do n=1,4
         if (order(n).lt.1 .or. order(n).gt.4) s = 10
         i1(n) = defn(2*order(n) + 6)
         i2(n) = defn(2*order(n) + 7)
         def_out(n+2) = defn(order(n)+2)
         def_out(2*n+6) = defn(2*order(n)+7)
         def_out(2*n+7) = defn(2*order(n)+8)
       enddo
       def_out(1) = 4
       if (def_out(6).eq.1) then
          def_out(1) = 3
          if (def_out(5).eq.1) then
             def_out(1) = 2
             if (def_out(4).eq.1) then
                def_out(1) = 1
             endif
          endif
       endif
       if (s.ne.0) return

       n = 0
       do m4 = i1(4), i2(4)
          m(order(4)) = m4
          do m3 = i1(3), i2(3)
             m(order(3)) = m3
             do m2 = i1(2), i2(2)
                m(order(2)) = m2
                do m1 = i1(1), i2(1)
                   m(order(1)) = m1
                   do v = v1,v2
                      ni = img_index( defn, m(1), m(2), m(3), m(4), v )
                      n = n + 1
                      if (ni.gt.0) then
                          dout(n) = din(ni)
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo

       end
C
C
       subroutine img_transform( defn, din, vd, vec, cvec,
     *                           mdim, mat, def_out, dout, s )
C      -------------------------------------------------------
C
C Flip an image about a specified axis
C
C Given:
C   definition record of image
       integer    defn(*)
C   input data
       real*4     din(*)
C   vector offset
       integer    vd
       real*4     vec(*), cvec(*)
C   transformation matrix
       integer    mdim
       real*4     mat(*)
C
C Returned:
C   output definition record
       integer    def_out(*)
C   output data
       real*4     dout(*)
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       real*4   img_interp
       integer  n, i, j, k, l, v, i1, j1
       real*4   mt(4,4), vv(4), x, y, z, t, xx, yy, zz, tt

       if (s.ne.0) return

       do n=1,len_defn
         image_defn(n) = def_out(n)
       enddo
       n = 0
       do j=1,4
          do i=1,4
             mt(i,j) = 0.0
          enddo
       enddo
       do i=1,4
          mt(i,i) = 1.0
       enddo
       if (mdim.eq.16) then
          i1 = 4
          j1 = 4
       endif
       if (mdim.eq.9) then
          i1 = 3
          j1 = 3
       endif
       if (mdim.eq.4) then
          i1 = 2
          j1 = 2
       endif
       do j=1,i1
          do i=1,j1
             n = n+1
             mt(i,j) = mat(n)
          enddo
       enddo
       do n=1,4
         vv(n) = vec(n) + cvec(n)
       enddo
       do l = t1, t2
          do k = z1, z2
             do j = y1, y2
                do i = x1,x2
                   do v = v1,v2
                      x = i - cvec(1)
                      y = j - cvec(2)
                      z = k - cvec(3)
                      t = l - cvec(4)
                      xx=vv(1)+mt(1,1)*x+mt(2,1)*y+mt(3,1)*z+mt(4,1)*t
                      yy=vv(2)+mt(1,2)*x+mt(2,2)*y+mt(3,2)*z+mt(4,2)*t
                      zz=vv(3)+mt(1,3)*x+mt(2,3)*y+mt(3,3)*z+mt(4,3)*t
                      tt=vv(4)+mt(1,4)*x+mt(2,4)*y+mt(3,4)*z+mt(4,4)*t
                      n = img_index( def_out, i, j, k, l, v )
                      dout(n) = img_interp( defn, din, xx,yy,zz,tt,v )
                   enddo
                enddo
             enddo
          enddo
       enddo
       end
C
C
       subroutine img_bshift( def_in, din, dout, dv, s )
C      -------------------------------------------------
C
C Perform a barrel shift on the image
C
C Given:
C   input definition record
       integer    def_in(*)
C   input data
       real*4     din(*)
C   output data
       real*4     dout(*)
C   offset vector for the shift
       integer    dv(4)
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  img_index
       integer  i, j, k, t, v, ni, no, m
       integer  ii, jj, kk, tt

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = def_in(m)
       enddo
       no = 0
       do t = t1, t2
          tt = t+dv(4)
          if (tt.gt.t2) tt = tt - t2 + t1 - 1
          if (tt.lt.t1) tt = tt + t2 - t1 + 1
          do k = z1, z2
            kk = k+dv(3)
            if (kk.gt.z2) kk = kk - z2 + z1 - 1
            if (kk.lt.z1) kk = kk + z2 - z1 + 1
             do j = y1, y2
                jj = j + dv(2)
                if (jj.gt.y2) jj = jj - y2 + y1 - 1
                if (jj.lt.y1) jj = jj + y2 - y1 + 1
                do i = x1, x2
                   ii = i + dv(1)
                   if (ii.gt.x2) ii = ii - x2 + x1 - 1
                   if (ii.lt.x1) ii = ii + x2 - x1 + 1
                   do v = v1,v2
                      ni = img_index( def_in, i, j, k, t, v )
                      no = img_index( def_in, ii, jj, kk, tt, v )
                      if (ni.gt.0 .and. no.gt.0) then
                         dout(no) = din(ni)
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo
       end
