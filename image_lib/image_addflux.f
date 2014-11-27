*$
** Image Analysis Routines
*  -----------------------
C
C The following is a library of image analysis routines.
C
C Last updated:   P. Alexander, MRAO,    15/08/90
C
C The routines make use of the following conventions and libraries:
C
C    Image data ordered by rows with the "TOP" row first"
C       row numbered from bottow of the image
C    MINIRT array passes information for the image:
C       range in X,(or U), (or I) direction = minirt(1),minirt(2)
C       range in Y,(or V), (or J) direction = minirt(4),minirt(3)
C    Only routines from the following libraries are used unless
C       stated otherwise (NON-standard routine)
C          IOLIB          -- user and file IO
C          CHRLIB         -- character handling
C          CMD-LANGUAGE   -- error handling / user interaction
C
*$ 1) Display and Statistics Routines
*  ----------------------------------
C
C
*+ image_addflux

       subroutine image_addflux(minirt,data,uv,gate,flux,npts,status)
C      --------------------------------------------------------------
C
C find sum of pixel values within a given region
C
C Given:
C   mini redtape of image
       integer     minirt(8)
C   image data
       real*4      data(*)
C   range on image to analyse
       integer     uv(4)
C   gate on image
       real*4      gate
C Returned:
C   flux
       real*4      flux
C   number of points analysed
       integer     npts
C   error return code
       integer     status
C
C The map region UV is summed above a gate GATE. FLUX is the sum of
C pixel values in this region. STATUS should be zero on entry and is not
C changed.
C-

C local variables
      real*4       value
      integer      iu, iv, uv_point(2)
      equivalence (uv_point(1), iu)
      equivalence (uv_point(2), iv)

C check status on entry
       if (status.ne.0) return

C do addition
       npts = 0
       flux=0.0
       do iv=uv(3),uv(4),-1
         do iu=uv(1),uv(2)
           call iuvval2(minirt,data,uv_point,value,status)
           if (abs(value).gt.gate .and.status.eq.0) then
             npts = npts+1
             flux = flux+value
           end if
         end do
       end do
       call cmd_err(status,'image_addflux',' ')
       end
