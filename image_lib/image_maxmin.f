C
C
*+ image_maxmin

       subroutine image_maxmin(minirt,data,nwindows,windows,iopt,
     *                         maxmin,positions,status          )
C      ----------------------------------------------------------
C
C Scan an image find max min and their positions
C
C Given:
C   miniredtape of image
       integer     minirt(8)
C   data array
       real*4      data(*)
C   number of windows to scan
       integer     nwindows
C   list of windows to scan
       integer     windows(4,nwindows)
C   flag indicating how the scan is performed
       integer     iopt
C
C Returned:
C   max, min and absolute max of these
       real*4      maxmin(3)
C   positions of max,min and absolute max of these
       integer     positions(2,3)
C   status
       integer     status
C
C The input image is scanned in the supplied windows for max and min.
C If IOPT is equal to 0 then the routine re-initialises the maxmin values
C before the scan is performed.  If IOPT is not zero then the comparison
C is made against the current values in the maxmin array -- maxmin and
C positions will then NOT be updated unless the values found in this
C routine are larger/smaller than those already in the maxmin array.
C
C PA, 22/09/90
C-

C local variables (counters)
       integer    iu1, iu2, iv1, iv2, iu, iv, irow, i, nw

C check status on entry
       if (status.ne.0) return

C re-initialise maxmin arrays is required
       if (iopt.eq.0) then
         maxmin(1) = -1.0E+60
         maxmin(2) =  1.0E+60
         maxmin(3) = -1.0E+60
       end if

C loop for each of the suplied windows
       do nw=1,nwindows

C .. for each window scan the regions of the image
         iu1=max(1,windows(1,nw)-minirt(1)+1)
         iu2=min(minirt(5),windows(2,nw)-minirt(1)+1)
         iv1=max(1,minirt(3)-windows(3,nw)+1)
         iv2=min(minirt(6),minirt(3)-windows(4,nw)+1)
         do iv=iv1,iv2
           irow = (iv-1)*minirt(5)
           do iu=iu1,iu2
             i = irow + iu
             if (data(i).gt.maxmin(1)) then
               maxmin(1) = data(i)
               positions(1,1) = iu - 1 + minirt(1)
               positions(2,1) = minirt(3) - iv + 1
             end if
             if (data(i).lt.maxmin(2)) then
               maxmin(2) = data(i)
               positions(1,2) = iu - 1 + minirt(1)
               positions(2,2) = minirt(3) - iv + 1
             end if
           end do
         end do
       end do

       if (maxmin(1).gt.abs(maxmin(2))) then
         maxmin(3) = maxmin(1)
         positions(1,3) = positions(1,1)
         positions(2,3) = positions(2,1)
       else
         maxmin(3) = -maxmin(2)
         positions(1,3) = positions(1,2)
         positions(2,3) = positions(2,2)
       end if

       if (status.ne.0) call io_wrerr(status,'in image_maxmin')

       end
