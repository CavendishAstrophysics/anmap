C
C
*+ plot_ctreset

       subroutine plot_ctreset(itype,status)
C      -------------------------------------
C
C Reset contour levels ready to be re-plotted
C
C Given:
C   contour type to reset
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
           contour_status(n) = abs(contour_status(n))
         endif
       enddo

       end
