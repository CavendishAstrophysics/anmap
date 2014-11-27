C
C
*+ image_gscan

       subroutine image_gscan( minirt,data,uv,gate,
     *                         xbar_in,sigma_in,
     *                         xbar_out,sigma_out, status)
C      ---------------------------------------------------
C
C Find the mean and SD in a region of the map gateing as required
C
C Given:
C   mini redtape for image
        integer    minirt(8)
C   image data
        real*4     data(*)
C   range to scan
        integer    uv(4)
C   gate, only values below gate*sigma_in
        real*4     gate
C   input mean
        real*4     xbar_in
C   input SD
        real*4     sigma_in
C Returned:
C   output mean in map region
        real*4     xbar_out
C   output SD in map region
        real*4     sigma_out
C   error return code
        integer    status
C
C The map is scanned in the specified region for the mean and sd.
C Only those points which are within gate*sigma_in of xbar_in are
C included in the analysis.
*-
C local variables
       integer        npts
       real*4         value
       integer        iu, iv, uv_point(2)
       equivalence    (uv_point(1), iu)
       equivalence    (uv_point(2), iv)

C check status on entry
       if (status.ne.0) return

C initialise counter for total number of points
       npts = 0

C find mean on data
       xbar_out = 0.0
       do iv=uv(3),uv(4),-1
         do iu=uv(1),uv(2)
           call iuvval2(minirt,data,uv_point,value,status)
           if (abs(value-xbar_in).le.gate*sigma_in) then
             npts = npts+1
             xbar_out = xbar_out + value
           end if
         end do
       end do
       xbar_out = xbar_out/float(npts)

C find SD on data
       sigma_out = 0.0
       do iv=uv(3),uv(4),-1
         do iu=uv(1),uv(2)
           call iuvval2(minirt,data,uv_point,value,status)
           if (abs(value-xbar_in).le.gate*sigma_in) then
             sigma_out = sigma_out + (value-xbar_out)**2
           end if
         end do
       end do
       sigma_out = sqrt( sigma_out/float(npts-1) )
       call cmd_err(status,'IMAGE_GSCAN',' ')
       end
