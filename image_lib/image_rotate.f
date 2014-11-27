C
C
*+ image_rotate
C
       subroutine image_rotate(minirt,data_in,rot,uc,vc,degrid_type,
     *                         data_out,status                      )
C      --------------------------------------------------------------
C
C Rotate an image by the specified angle (degrees) counter-clockwise.
C
C Given:
C   mini redtape
       integer       minirt(8)
C   input image data
       real*4        data_in(*)
C   rotation angle of image on the grid (degrees)
       real*4        rot
C   centre about which to rotate image
       real*4        uc, vc
C   type of interpolation to use
       integer       degrid_type
C Returned:
C   output image
       real*4        data_out(*)
C   error return
       integer       status
C
C
C The input image is rotated by an angle ROT on the output grid,
C the map location U,V now becomes the point 
C   (Ucos(rot)-Vsin(rot)),(Usin(rot)+Vcos(rot)) 
C on the new grid.  Any pixel on the output map which does not correspond 
C to a location on the input map is given the BLANK value.
C
C The type of interpolation to use in the rotation must be specified.
*-

C local variables representing U and V on the input map
       real*8       u, v, duv_point(2), drot
       equivalence (duv_point(1), u)
       equivalence (duv_point(2), v)
C loop counters
       integer      iu, iv, i, uv_point(2), istat
       equivalence (uv_point(1), iu)
       equivalence (uv_point(2), iv)
C functions
       integer      iuvmap2

C check status on entry
       if (status.ne.0) return

C set rotation angle for map
       drot = rot*3.14159265D+0/180.0D+0

C determine values on the output map
       do iv=minirt(3),minirt(4),-1
         do iu = minirt(1),minirt(2)
           istat = 0
           i = iuvmap2(minirt,uv_point)
           u = (float(iu)-uc)*cos(drot) + (float(iv)-vc)*sin(drot) + uc
           v = -(float(iu)-uc)*sin(drot) + (float(iv)-vc)*cos(drot) + vc
           call ruvval2(minirt,data_in,duv_point,degrid_type,
     *                  data_out(i),istat)
         end do
       end do

C report any error
999    call cmd_err(status,'image_rotate',' ')

       end
