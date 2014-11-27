C
C
*+ plot_ctclear

       subroutine plot_ctclear(itype,status)
C      -------------------------------------
C
C Clear currently defined contour levels
C
C Given:
C   contour type to clear
       integer    itype
C Updated:
C   error status
       integer    status
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C local variables
       integer     n

C check status on entry
       if (status.ne.0) return

C clear counters of contours defined and plotted and the contour tables
       do n=1,max_contours
         if (itype.eq.0 .or. itype.eq.contour_type(n)) then
           contour_status(n) = 0
         endif
       enddo

       end
