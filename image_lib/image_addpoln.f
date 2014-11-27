C
C
C
*+ image_addpoln

       subroutine image_addpoln(minirt,data,noise,
     *                          uv,gate,flux,npts,status)
C      --------------------------------------------------------------
C
C find sum of pixel values within a given region
C
C Given:
C   mini redtape of image
       integer     minirt(8)
C   image data
       real*4      data(*)
C   noise on image pixel
       real*4      noise
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
C changed.  A correction is made to account for bias in polarization 
C fluxes at low flux densities assuming a ricean distribution of noise
C and making a correction based on the most likely true value of a pixel
C flux density given the flux density within a pixel and the known noise
C on the polarization image.
C-

C local variables
      real*4       value, image_polcorr
      integer      iu, iv, uv_point(2)
      equivalence (uv_point(1), iu)
      equivalence (uv_point(2), iv)

       real*4    image_polcorr

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
             value = value/noise
             flux = flux+noise*image_polcorr(value,status)
           end if
         end do
       end do
       call cmd_err(status,'image_addpoln',' ')
       end
