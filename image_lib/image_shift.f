C
C
*+ image_shift
C
       subroutine image_shift(minirt,data_in,du,dv,degrid_type,
     *                        data_out,status                 )
C      --------------------------------------------------------
C
C Shift an image by a specified amount on the computing grid
C
C Given:
C   mini redtape
       integer       minirt(8)
C   input image data
       real*4        data_in(*)
C   shift of map on UV grid in U and V
       real*8        du, dv
C   type of interpolation to use
       integer       degrid_type
C Returned:
C   output image
       real*4        data_out(*)
C   error return
       integer       status
C
C
C The input image is shifted by an amount DU, DV on the output grid,
C the map location U,V now becomes the point U+DU, V+DV on the new grid.
C Any pixel on the output map which does not correspond to a location on
C the input map is given the BLANK value.
C
C The type of interpolation to use in the shift must be specified.
*-

C local variables representing U and V on the input map
       real*8       u, v, duv_point(2)
       equivalence (duv_point(1), u)
       equivalence (duv_point(2), v)
C loop counters
       integer      iu, iv, i, uv_point(2)
       equivalence (uv_point(1), iu)
       equivalence (uv_point(2), iv)
C window containing real data in output map
       integer      iu1, iu2, iv1, iv2
C functions
       integer      iuvmap2

C check status on entry
       if (status.ne.0) return

C initialise output map with BLANKs
       call filmap(data_out,minirt,minirt(8),status)

C determine useful window on output map
       iu1 = minirt(1) + du
       iu1 = max(iu1,minirt(1))
       iu2 = minirt(2) + du
       iu2 = min(iu2,minirt(2))
       iv1 = minirt(3) + dv
       iv1 = min(iv1,minirt(3))
       iv2 = minirt(4) + dv
       iv2 = max(iv2,minirt(4))
       if (status.ne.0) goto 999

C determine values on the output map
       do iv=iv1,iv2,-1
         v = float(iv) - dv
         do iu = iu1,iu2
           i = iuvmap2(minirt,uv_point)
           u = float(iu) - du
           call ruvval2(minirt,data_in,duv_point,degrid_type,
     *                  data_out(i),status)
         end do
       end do

C report any error
999    call cmd_err(status,'image_shift',' ')

       end
