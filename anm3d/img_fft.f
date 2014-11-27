C
C
       subroutine img_fft( defn_in, din, dout, work, dirn, type, s )
C      -------------------------------------------------------------
C
C Apply an (inverse) FFT image 
C
C Given:
C   input definition record
       integer    defn_in(*)
C   input data
       real*4     din(*)
C   output data
       real*4     dout(*)
C   work space data
       real*4     work(*)
C   direction of the transform
       integer    dirn, type
C
C Updated:
C   status
       integer    s
C
C-

       include 'ic.inc'
       integer  i, m

       if (s.ne.0) return
       do m=1,len_defn
         image_defn(m) = defn_in(m)
       enddo

C sort out input data
       if (dirn.eq.1) then
         if (type.eq.1) then
           do i=1,ndata
             dout(2*i-1) = din(i)
             dout(2*i) = 0.0
           enddo
         else
           do i=1,ndata
             dout(i) = din(i)
           enddo
         endif
       else
         do i=1,ndata
           dout(i) = din(i)
         enddo
         call img_fftfiddle4(dout,xdim,ydim,work,s)
       endif

C do the transform
       call complex_2dfft(dout,xdim,ydim,dirn,s)
       if (s.ne.0) goto 999

       if (dirn.eq.1) then
         call img_fftfiddle4(dout,xdim,ydim,work,s)
       endif

999    continue
       end
C
C
       subroutine img_fftfiddle4( x, m, n, work, s)
C      --------------------------------------------
C
C      Perform a simple data translation
C
       integer    m,n,i,j,k,s
       integer    j1,j2
       real*4     x(2,*), work(2,*)
       do j=1,n/2
         j1 = (j-1)*m
         j2 = (j-1+n/2)*m
         do i=1,m/2
           do k=1,2
             work(k,i+j1) = x(k,i+m/2+j2)
             work(k,i+m/2+j2) = x(k,i+j1)
           enddo
         end do
       end do
       do j=1+n/2,n
         j1 = (j-1)*m
         j2 = (j-1-n/2)*m
         do i=1,m/2
           do k=1,2
             work(k,i+j1) = x(k,i+m/2+j2)
             work(k,i+m/2+j2) = x(k,i+j1)
           enddo
         end do
       end do
       do i=1,m*n
         do k=1,2
           x(k,i) = work(k,i)
         enddo
       end do
       end

