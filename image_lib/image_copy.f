C
C
*+ image_copy

       subroutine image_copy(minirt,data_in,data_out,status)
C      -----------------------------------------------------
C
C Copy image data
C
C Given:
C   Mini redtape
       integer        minirt(8)
C   Input image array
       real*4         data_in(*)
C Returned:
C   Output image array
       real*4         data_out(*)
C   Error return code
       integer        status
C
C Do a copy of the image between arrays
*-
C local variables
       integer     i

       if (status.ne.0) return
       do i = 1,minirt(5)*minirt(6)
         data_out(i) = data_in(i)
       end do
       call cmd_err(status,'image_copy',' ')

       end
