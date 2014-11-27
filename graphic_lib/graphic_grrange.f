*+ graphic_grrange

       subroutine graphic_grrange( gopt, ndata, x, y, rxy, s )
C      -------------------------------------------------------
C
C Determine a suitable range for supplied data in x and y
C
C Given:
C   graphic line structure
       integer    gopt(*)
C   number of data points
       integer    ndata
C   x, y, data:
       real*4     x(*), y(*)
C Updated:
C   data ranges
       real*4     rxy(4)
C   error status
       integer    s
C
C A suitable range is found for the supplied data -- if rxy is already
C "populated" with values these are used.  Appropriate offsets and
C scalings are applied.
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'

C local variables
       integer         n
       real*4          xx, yy, xmax, xmin, ymax, ymin

C check status on entry
       if (s.ne.0) return

C copy structure
       call graphic_copy_grline(gopt,grline,s)
C do data scaling
       do n=1,ndata
         xx = x(n)*grline_scale_x + grline_offset_x
         yy = y(n)*grline_scale_y + grline_offset_y
         if (n.eq.1) then
           xmax = xx
           xmin = xx
           ymax = yy
           ymin = yy
         endif
         xmax = max(xx,xmax)
         xmin = min(xx,xmin)
         ymax = max(yy,ymax)
         ymin = min(yy,ymin)
       enddo

C sort out range
       if (rxy(1).eq.rxy(2)) then
         if (grline_x_log) then
           call pgrnge(log10(max(xmin,1.0E-30)),
     *                  log10(max(xmax,1.0E-30)),
     *                  rxy(1),rxy(2))
         else
           call pgrnge(xmin,xmax,rxy(1),rxy(2))
         endif
       endif
       if (rxy(3).eq.rxy(4)) then
         if (grline_y_log) then
           call pgrnge(log10(max(ymin,1.0E-30)),
     *                  log10(max(ymax,1.0E-30)),
     *                  rxy(3),rxy(4))
         else
           call pgrnge(ymin,ymax,rxy(3),rxy(4))
         endif
       endif
C check status on exit
       call cmd_err(s,'graphic_grrange',' ')

       end
