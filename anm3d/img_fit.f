C
C Image routines to provide geometric transformations
C
C
C
       subroutine img_fitvel( def_in, din, def_out, dout, v, axis, s )
C      ---------------------------------------------------------------
C
C Perform a pixel-wise fit to the velocity profile of the image
C
C Given:
C   input definition record
       integer    def_in(*)
C   input data
       real*4     din(*)
C   output definition record
       integer    def_out(*)
C   control data
       real*4     v(3)
C   axis along which the spectral dimension is held
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
       if (axis.eq.1) then
          call img_fitvelx( def_in, din, def_out, dout, v, s )
       else if (axis.eq.2) then
          call img_fitvely( def_in, din, def_out, dout, v, s )
       else if (axis.eq.3) then
          call img_fitvelz( def_in, din, def_out, dout, v, s )
       endif
       end
C
C
       subroutine img_fitvelz( def_in, din, def_out, dout, v, s )
C      ----------------------------------------------------------
C
C Perform a pixel-wise fit to the velocity profile of the image
C
C Given:
C   input definition record
       integer    def_in(*)
C   input data
       real*4     din(*)
C   output definition record
       integer    def_out(*)
C   control data
       real*4     v(3)
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
       integer  n, m, m1, m2, xys, m_max
       real*4   fv(1024), fv_max, fv_max2

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = def_in(m)
       enddo
       xys = xdim*ydim
       do n=1,xys
          fv_max = -1.0E30
          dout(n) = 0.0
          dout(n + xys) = 0.0
          dout(n + 2*xys) = 0.0
          do m=1,zdim
               fv(m) = din( n + (m-1)*xys )
               if (fv(m).gt.fv_max) then
                   fv_max = fv(m)
                   m_max = m
               endif
               dout(n) = dout(n) + fv(m)
          enddo
          fv_max2 = fv_max/2.0
          m1 = m_max
          m2 = m_max
          do while ( (m1.le.zdim) .and. (fv(m1).gt.fv_max2) )
             m1 = m1 + 1
          enddo
          do while ( (m2.ge.1) .and. (fv(m2).gt.fv_max2) )
             m2 = m2 - 1
          enddo
          if ( fv_max .ge. v(1) ) then
             dout(n + xys) = m_max
             dout(n + 2*xys) = m1 - m2
          endif
       enddo
       end
C
C
       subroutine img_fitvely( def_in, din, def_out, dout, v, s )
C      ----------------------------------------------------------
C
C Perform a pixel-wise fit to the velocity profile of the image
C
C Given:
C   input definition record
       integer    def_in(*)
C   input data
       real*4     din(*)
C   output definition record
       integer    def_out(*)
C   control data
       real*4     v(3)
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
       integer  nn, nx, nz, n, m, m1, m2, xys, m_max
       real*4   fv(1024), fv_max, fv_max2

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = def_in(m)
       enddo
       xys = xdim*zdim
       n = 0
       do nx=1,xdim
         do nz = 1,zdim
          fv_max = -1.0E30
          dout(n) = 0.0
          dout(n + xys) = 0.0
          dout(n + 2*xys) = 0.0
          do m=1,ydim
               nn = nx + (m-1)*xdim + (nz-1)*xdim*ydim
               fv(m) = din( nn )
               if (fv(m).gt.fv_max) then
                   fv_max = fv(m)
                   m_max = m
               endif
               dout(n) = dout(n) + fv(m)
          enddo
          fv_max2 = fv_max/2.0
          m1 = m_max
          m2 = m_max
          do while ( (m1.le.ydim) .and. (fv(m1).gt.fv_max2) )
             m1 = m1 + 1
          enddo
          do while ( (m2.ge.1) .and. (fv(m2).gt.fv_max2) )
             m2 = m2 - 1
          enddo
          if ( fv_max .ge. v(1) ) then
             dout(n + xys) = m_max
             dout(n + 2*xys) = m1 - m2
          endif
         enddo
       enddo
       end
C
C
       subroutine img_fitvelx( def_in, din, def_out, dout, v, s )
C      ----------------------------------------------------------
C
C Perform a pixel-wise fit to the velocity profile of the image
C
C Given:
C   input definition record
       integer    def_in(*)
C   input data
       real*4     din(*)
C   output definition record
       integer    def_out(*)
C   control data
       real*4     v(3)
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
       integer  nn, n, m, m1, m2, xys, m_max
       real*4   fv(1024), fv_max, fv_max2

       if (s.ne.0) return

       do m=1,len_defn
         image_defn(m) = def_in(m)
       enddo
       xys = ydim*zdim
       nn = 0
       do n=1,xys
          fv_max = -1.0E30
          dout(n) = 0.0
          dout(n + xys) = 0.0
          dout(n + 2*xys) = 0.0
          do m=1,xdim
               nn = nn + 1
               fv(m) = din( nn )
               if (fv(m).gt.fv_max) then
                   fv_max = fv(m)
                   m_max = m
               endif
               dout(n) = dout(n) + fv(m)
          enddo
          fv_max2 = fv_max/2.0
          m1 = m_max
          m2 = m_max
          do while ( (m1.le.xdim) .and. (fv(m1).gt.fv_max2) )
             m1 = m1 + 1
          enddo
          do while ( (m2.ge.1) .and. (fv(m2).gt.fv_max2) )
             m2 = m2 - 1
          enddo
          if ( fv_max .ge. v(1) ) then
             dout(n + xys) = m_max
             dout(n + 2*xys) = m1 - m2
          endif
       enddo
       end
