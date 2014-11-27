C Utility routines for image manipulation
C
C

       integer function img_index( defn, i, j, k, t, v )
C      -------------------------------------------------
C
C Return index to data specified by defn for the selected point
C
C Given:
C   definition record
       integer    defn(*)
C   vector of specified point
       integer    i, j, k, t, v
C
C-
       include 'ic.inc'
       integer  indx

       indx = (t-defn(14))*(defn(3)*defn(4)*defn(5)*defn(7)) +
     *        (k-defn(12))*(defn(3)*defn(4)*defn(7)) +
     *        (j-defn(10))*(defn(3)*defn(7)) +
     *        (i-defn(8))*(defn(7)) +
     *        v
       if (indx.gt.0 .and. indx.le.defn(2)) then
          img_index = indx
       else
          img_index = -1
       endif
       end
C
C
C
       real*4 function img_interp( defn, d, x, y, z, t, v )
C      ----------------------------------------------------
C
C Return index to data specified by defn for the selected point
C
C Given:
C   definition record
       integer    defn(*)
C   input data
       real*4     d(*)
C   vector of specified point
       real*4     x, y, z, t
       integer    v
C
C-
       include 'ic.inc'

       integer  img_index
       integer  i1, j1, k1, l1
       integer  i2, j2, k2, l2
       real*4   dx, dy, dz, dt, val
       real*4   rx1, ry1, rz1, rt1
       real*4   rx2, ry2, rz2, rt2

C find bounding values
       i1 = x
       j1 = y
       k1 = z
       l1 = t
       i2 = min( i1+1, defn(9) )
       j2 = min( j1+1, defn(11) )
       k2 = min( k1+1, defn(13) )
       l2 = min( l1+1, defn(15) )
       if (i1.lt.defn(8)  .or. i1.gt.defn(9)  .or. 
     *     j1.lt.defn(10) .or. j1.gt.defn(11) .or.
     *     k1.lt.defn(12) .or. k1.gt.defn(13) .or.
     *     l1.lt.defn(14) .or. l1.gt.defn(15)     ) then
         image_defn(19) = defn(19)
         img_interp = blank
         return
       endif
       dx = x - i1
       dy = y - j1
       dz = z - k1
       dt = t - l1
       rx2 = d(img_index(defn,i2,j1,k1,l1,v))
       rx1 = d(img_index(defn,i1,j1,k1,l1,v))
       ry1 = rx1 + dx*(rx2-rx1)
       if (defn(1).eq.1) then
         val = ry1
       else 
         rx2 = d(img_index(defn,i2,j2,k1,l1,v))
         rx1 = d(img_index(defn,i1,j2,k1,l1,v))
         ry2 = rx1 + dx*(rx2-rx1)
         rz1 = ry1 + dy*(ry2-ry1)
         if (defn(1).eq.2) then
            val = rz1
         else
           rx2 = d(img_index(defn,i2,j1,k2,l1,v))
           rx1 = d(img_index(defn,i1,j1,k2,l1,v))
           ry1 = rx1 + dx*(rx2-rx1)
           rx2 = d(img_index(defn,i2,j2,k2,l1,v))
           rx1 = d(img_index(defn,i1,j2,k2,l1,v))
           ry2 = rx1 + dx*(rx2-rx1)
           rz2 = ry1 + dy*(ry2-ry1)
           rt1 = rz1 + dz*(rz2-rz1)
           if (defn(1).eq.3) then
              val = rt1
           else
              rx2 = d(img_index(defn,i2,j1,k1,l2,v))
              rx1 = d(img_index(defn,i1,j1,k1,l2,v))
              ry1 = rx1 + dx*(rx2-rx1)
              rx2 = d(img_index(defn,i2,j2,k1,l2,v))
              rx1 = d(img_index(defn,i1,j2,k1,l2,v))
              ry2 = rx1 + dx*(rx2-rx1)
              rz1 = ry1 + dy*(ry2-ry1)
              rx2 = d(img_index(defn,i2,j1,k2,l2,v))
              rx1 = d(img_index(defn,i1,j1,k2,l2,v))
              ry1 = rx1 + dx*(rx2-rx1)
              rx2 = d(img_index(defn,i2,j2,k2,l2,v))
              rx1 = d(img_index(defn,i1,j2,k2,l2,v))
              ry2 = rx1 + dx*(rx2-rx1)
              rz2 = ry1 + dy*(ry2-ry1)
              rt2 = rz1 + dz*(rz2-rz1)
              val = rt1 + dt*(rt2-rt1)
           endif
         endif
       endif
       img_interp = val
       end
C
C
       subroutine img_getvec1( defn, d, v, vec )
C      ----------------------------------------
C
C Return a 1D vector of values
C
C Given:
C   definition record
       integer    defn(*)
C   data
       real*4     d(*)
C   vector pointer
       integer    v
C
C Returned:
C   vector of data values
       real*4     vec(*)
C
C-
       include 'ic.inc'
       integer  img_index
       integer  i, n, m

       do m=1,len_defn
         image_defn(m) = defn(m)
       enddo
       if (v.ge.v1 .and. v.le.v2) then
         m = 0
         do i=x1,x2
            n = img_index(defn,i,defn(10),defn(12),defn(14),v)
            m = m + 1
            vec(m) = d(n)
         enddo
       endif
       end
C
C
       subroutine img_getslice( defn, d, defn1, nd, vd, s )
C      ---------------------------------------------------
C
C Return an interpolated vector as specified by defn1
C
C Gievn:
C   definition record of image
       integer    defn(*)
C   input data
       real*4     d(*)
C   definition record for slice
       integer    defn1(*)
C   length of slice
       integer    nd
C Returned:
C   output data slice
       real*4     vd(*)
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       real*4   x, y, z, t
       real*4   img_interp
       integer  n, m

       do m=1,len_defn
         image_defn(m) = defn1(m)
       enddo
       do n=1,nd
          x = float(x1) +  float(n-1)*float(x2-x1)/float(nd-1)
          y = float(y1) +  float(n-1)*float(y2-y1)/float(nd-1)
          z = float(z1) +  float(n-1)*float(z2-z1)/float(nd-1)
          t = float(t1) +  float(n-1)*float(t2-t1)/float(nd-1)
          vd(2*n-1) = sqrt((x-x1)**2+(y-y1)**2+(z-z1)**2+(t-t1)**2)
          vd(2*n) = img_interp(defn, d, x, y, z, t, v1)
       enddo
       end
C
C
       subroutine img_getvalue( defn, d, vecf, v )
C      -------------------------------------------
C
C Return an interpolated value from image data
C
C Given:
C   definition record
       integer    defn(*)
C   data
       real*4     d(*)
C   vector of position in data
       integer    vecf(*)
C
C Returned:
C   data value
       real*4     v
C
C-
       include 'ic.inc'
       real*4   img_interp
       integer  m

       do m=1,len_vecf
         image_vecf(m) = vecf(m)
       enddo
       v = img_interp( defn, d, xp, yp, zp, tp, vp )
       end
