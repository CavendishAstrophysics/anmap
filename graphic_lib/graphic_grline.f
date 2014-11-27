*+ graphic_grline

       subroutine graphic_grline( gopt, ndata, x, y, ex, ey,
     *                            do_auto, dtmax, s )
C      -----------------------------------------------------
C
C Plot a line using options specified in the supplied grline struture
C
C Given:
C   graphic line structure
       integer    gopt(*)
C   number of data points
       integer    ndata
C   x, y, error-x, error-y data:
       real*4     x(*), y(*), ex(*), ey(*)
C   flag to request auto-scaling be applied if required
       logical    do_auto
C   data maximum in x and y
       real*4     dtmax(2)
C Updated:
C   error status
       integer    s
C
C A graphic line is draw in the current corrdinate system; the style of
C the line is controlled by the options in the supplies graphic line
C structure.
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'

C local variables
       integer         n, nd, nn
       real*4          d, dx, offset_x, scale_y, x1, x2, y1, y2
       real*4          e1, e2

C check status on entry
       if (s.ne.0) return

C copy structure
       call graphic_copy_grline(gopt,grline,s)

C check for auto-scale
       if (grline_auto_x) then
         d = y(1)
         do n=1,ndata
           if (y(n).gt.d) then
             d = y(n)
             dx = x(n)
           endif
         enddo
         if (do_auto) then
           offset_x = dtmax(1) - dx
         else
           offset_x = grline_offset_x
           dtmax(1) = dx*grline_scale_x + offset_x
         endif
       else
         offset_x = grline_offset_x
       endif
       if (grline_auto_y) then
         d = y(1)
         do n=1,ndata
           d = max(d,y(n))
         enddo
         if (do_auto) then
           scale_y = dtmax(2)/d
         else
           scale_y = grline_scale_y
           dtmax(2) = d*scale_y
         endif
       else
         scale_y = grline_scale_y
       endif

C do data scaling
       do n=1,ndata
         x(n) = x(n)*grline_scale_x + offset_x
         y(n) = y(n)*scale_y + grline_offset_y
         if (grline_x_log) then
           if (x(n).gt.0.0) then
             x(n) = log10(x(n))
           else
             x(n) = -1.0E-30
           endif
         endif
         if (grline_y_log) then
           if (y(n).gt.0.0) then
             y(n) = log10(y(n))
           else
             y(n) = -1.0E-30
           endif
         endif
       enddo
       if (grline_ex_opt.eq.1) then
         nd = ndata
       elseif (grline_ex_opt.eq.2) then
         nd = 2*ndata
       endif
       do n=1,nd
         if (grline_ex_opt.gt.0) then
           ex(n) = ex(n)*grline_scale_x
         endif
         if (grline_ey_opt.gt.0) then
           ey(n) = ey(n)*scale_y
         endif
       enddo


C plot line if required
       if (grline_type.eq.1) then
         call graphic_set_line_opt(grline_line_opt,s)
         call pgline(ndata,x,y)
       elseif (grline_type.eq.2) then
         call graphic_set_line_opt(grline_line_opt,s)
         call pgbin(ndata,x,y,.true.)
       endif

C plot symbols if required
       if (grline_symbol.gt.0) then
         call graphic_set_text_opt(grline_text_opt,s)
         call pgpoint(ndata,x,y,grline_symbol)
       endif

C plot error bars if required
       nn = 0
       if (grline_ex_opt.ne.0) then
         call graphic_set_line_opt(grline_error_opt,s)
         do n=1,ndata
           if (grline_ex_opt.eq.1) then
             e1 = ex(n)
             e2 = ex(n)
           else
             nn = nn + 1
             e1 = ex(nn)
             nn = nn + 1
             e2 = ex(nn)
           endif
           if (grline_x_log) then
             x1 = log10(10.0**x(n) + e1)
             x2 = log10(10.0**x(n) - e2)
           else
             x1 = x(n) + e1
             x2 = x(n) - e2
           endif
           if (e1.ne.0.0 .and. e2.ne.0.0) then
             call pgerrx(1,x1,x2,y(n),2.0*grline_ex_top)
           else
             call pgsch(grline_ex_top)
             call pgsah(2,35.0,1.0)
             if (e1.ne.0.0) then
               call pgarro(x(n),y(n),x1,y(n))
             else
               call pgarro(x(n),y(n),x2,y(n))
             endif
           endif
         enddo
       endif
       if (grline_ey_opt.ne.0) then
         call graphic_set_line_opt(grline_error_opt,s)
         do n=1,ndata
           if (grline_ey_opt.eq.1) then
             e1 = ey(n)
             e2 = ey(n)
           else
             nn = nn + 1
             e1 = ey(nn)
             nn = nn + 1
             e2 = ey(nn)
           endif
           if (grline_y_log) then
             y1 = log10(10.0**y(n) + e1)
             y2 = log10(10.0**y(n) - e2)
           else
             y1 = y(n) + e1
             y2 = y(n) - e2
           endif
           if (e1.ne.0.0 .and. e2.ne.0.0) then
             call pgerry(1,x(n),y1,y2,2.0*grline_ey_top)
           else
             call pgsch(grline_ey_top)
             call pgsah(2,35.0,1.0)
             if (e1.ne.0.0) then
               call pgarro(x(n),y(n),x(n),y1)
             else
               call pgarro(x(n),y(n),x(n),y2)
             endif
           endif
         enddo
       endif

C update status flags
       grline_status = -1
       call graphic_copy_grline(grline,gopt,s)

C check status on exit
       call cmd_err(s,'graphic_grline',' ')

       end


