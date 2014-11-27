C
C
*$ 2) Routines Producing New Images
*  --------------------------------
C
C
*+ image_convolve

       subroutine image_convolve(nix,niy,in_data,ncx,ncy,icx,icy,array,
     *                           null,out_data,status                 )
C      ----------------------------------------------------------------
C
C Convolve the input image with the specified array
C
C Given:
C   size of input image
       integer    nix, niy
C   input image
       real*4     in_data(nix,niy)
C   size of convolving array
       integer    ncx, ncy
C   centre of convolving array
       integer    icx, icy
C   convolving array data
       real*4     array(ncx,ncy)
C   null data value on input image
       real*4     null
C
C Returned:
C   output image
       real*4     out_data(nix,niy)
C   error status word
       integer    status
C
C The input image is convolved with the supplied function.  The output
C image is of the same size as the input image.
C
C Values beyond the edge of the input image are assumed to be zero.
*-
C Local variables
       integer    i, j, i1, i2, j1, j2, ii, jj

C check status on entry
       if (status.ne.0) return

C initialise output data
       do j=1,niy
         do i=1,nix
           out_data(i,j) = 0.0
         end do
       end do

C move through input image and convolve to output image
       do j=1,niy
         j1 = max(1,j+icy-ncy)
         j2 = min(niy,j+icy-1)
         do i=1,nix
           i1 = max(1,i-icx+1)
           i2 = min(nix,i+ncx-icx)
           if (in_data(i,j).ne.null) then
             do jj=j1,j2
               do ii=i1,i2
                 out_data(ii,jj) = out_data(ii,jj) +
     *               in_data(i,j)*array(ii-i+icx,jj-j+ncy-icy+1)
               end do
             end do
           end if
         end do
       end do

C report any error
       call cmd_err(status,'image_convolve',' ')
       end
