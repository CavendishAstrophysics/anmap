C
C
*+ image_moments

       subroutine image_moments(minirt,data,uv,uv_mom,status)
C      ------------------------------------------------------
C
C find moments of image within the specified region
C
C Given:
C   mini redtape of image
       integer     minirt(8)
C   image data
       real*4      data(*)
C   range on image to analyse
       integer     uv(4)
C Returned:
C   1st and 2nd vector moments of image
       real*4      uv_mom(4)
C   error return code
       integer     status
C
C The map region UV is analysed and the moments of the image in this
C region are returned
C-

C local variables
      real*4       value, mass
      integer      i, iu, iv, uv_point(2)
      equivalence (uv_point(1), iu)
      equivalence (uv_point(2), iv)

C check status on entry
       if (status.ne.0) return
       do i = 1,4
         uv_mom(i) = 0.0
       enddo
       mass = 0.0
       do iv=uv(3),uv(4),-1
         do iu=uv(1),uv(2)
           call iuvval2(minirt,data,uv_point,value,status)
           if (status.eq.0) then
             uv_mom(1) = uv_mom(1) + value*float(iu)
             uv_mom(2) = uv_mom(2) + value*float(iv)
             uv_mom(3) = uv_mom(3) + value*float(iu)*float(iu)
             uv_mom(4) = uv_mom(4) + value*float(iv)*float(iv)
             mass = mass + value
           end if
         end do
       end do
       if (mass.ne.0.0) then
         do i = 1,4
           uv_mom(i) = uv_mom(i) / mass
         enddo
       endif
       call cmd_err(status,'image_moments',' ')
       end
