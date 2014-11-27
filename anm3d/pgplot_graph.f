C
C pgGraph routines
C ----------------
C
C    Data structures used by these routines:
C
C    gscale()    1 = xscale     2 = xoffset
C                3 = yscale     4 = yoffset
C                5 = xauto      6 = yauto
C
C    gopt()      1 = xlog       2 = ylog
C                3 = xauto      4 = yauto
C                5 = xscaled    6 = yscaled
C
*+ pgGraphInit

       subroutine pgGraphInit( gscale, gopt, rxy )
C      -------------------------------------------
C
C Initialise graph variables
C
C Updated:
C   scales and offsets
       real*4     gscale(*)
C   options
       integer    gopt(*)
C   ranges
       real*4     rxy(4)
C
C-
       gscale(1) = 1.0
       gscale(2) = 0.0
       gscale(3) = 1.0
       gscale(4) = 0.0
       gscale(5) = 0.0
       gscale(6) = 1.0
       gopt(1) = 0
       gopt(2) = 0
       gopt(3) = 0
       gopt(4) = 0
       gopt(5) = 0
       gopt(6) = 0
       rxy(1) = 0.0
       rxy(2) = 0.0
       rxy(3) = 0.0
       rxy(4) = 0.0
       end
C
C
*+ pgGraphRange

       subroutine pgGraphRange( gscale, gopt, ndata, x, y, rxy )
C      ---------------------------------------------------------
C
C Determine a suitable range for supplied data in x and y
C
C Given:
C   scales and offsets
       real*4     gscale(*)
C   options
       integer    gopt(*)
C   number of data points
       integer    ndata
C   x, y, data:
       real*4     x(*), y(*)
C Updated:
C   data ranges
       real*4     rxy(4)
C
C A suitable range is found for the supplied data -- if rxy is already
C "populated" with values these are used.  Appropriate offsets and
C scalings are applied.
C-

C local variables
       integer         n
       real*4          xx, yy, xmax, xmin, ymax, ymin

C do data scaling
       do n=1,ndata
         xx = x(n)*gscale(1) + gscale(2)
         yy = y(n)*gscale(3) + gscale(4)
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
       if (gopt(1).eq.1) then
           call pgrnge(log10(max(xmin,1.0E-30)),
     *                  log10(max(xmax,1.0E-30)),
     *                  rxy(1),rxy(2))
         else
           call pgrnge(xmin,xmax,rxy(1),rxy(2))
       endif
       if (gopt(2).eq.1) then
           call pgrnge(log10(max(ymin,1.0E-30)),
     *                  log10(max(ymax,1.0E-30)),
     *                  rxy(3),rxy(4))
         else
           call pgrnge(ymin,ymax,rxy(3),rxy(4))
       endif
       end
C
C
*+ pgGraphScale

       subroutine pgGraphScale( gscale, gopt, ndata, 
     *                          x, y, ex1, ex2, ey1, ey2 )
C      ---------------------------------------------------
C
C Scale data for plot as appropriate
C
C Given:
C   scales and offsets
       real*4     gscale(*)
C   options
       integer    gopt(*)
C   number of data points
       integer    ndata
C Updated:
C   x, y, data and errors
       real*4     x(*), y(*), ex1(*), ey1(*), ex2(*), ey2(*)
C
C Appropriate scaling and auto-scaling is applied to the data
C
C local variables
       integer         n
       real*4          d, dx, xoffset, yscale

C check for auto-scale
       if (gopt(3).eq.1) then
         d = y(1)
         do n=1,ndata
           if (y(n).gt.d) then
             d = y(n)
             dx = x(n)
           endif
         enddo
         if (gopt(5).eq.1) then
           xoffset = gscale(5) - dx
         else
           xoffset = gscale(2)
           gscale(5) = dx*gscale(1) + gscale(2)
           gopt(5) = 1
         endif
       else
         xoffset = gscale(2)
       endif
       if (gopt(4).eq.1) then
         d = y(1)
         do n=1,ndata
           d = max(d,y(n))
         enddo
         if (gopt(6).eq.1) then
           yscale = gscale(6)/d
         else
           yscale = gscale(3)
           gscale(6) = d*yscale
           gopt(6) = 1
         endif
       else
         yscale = gscale(3)
       endif

C do data scaling
       do n=1,ndata
         x(n) = x(n)*gscale(1) + xoffset
         y(n) = y(n)*yscale + gscale(4)
         if (gopt(1).eq.1) then
           if (x(n).gt.0.0) then
             x(n) = log10(x(n))
           else
             x(n) = -1.0E-30
           endif
         endif
         if (gopt(2).eq.1) then
           if (y(n).gt.0.0) then
             y(n) = log10(y(n))
           else
             y(n) = -1.0E-30
           endif
         endif
       enddo
       do n=1,ndata
         ex1(n) = ex1(n)*gscale(1)
         ex2(n) = ex2(n)*gscale(1)
         ey1(n) = ey1(n)*gscale(3)
         ey2(n) = ey2(n)*gscale(3)
       enddo
       end
C
C
*+ pgGraphError

       subroutine pgGraphError( gscale, gopt, ndata, errx, erry, 
     *                          etop, x, y, ex1, ex2, ey1, ey2  )
C      ---------------------------------------------------------
C
C Plot Error bars for a graph
C
C Given:
C   scales and offsets
       real*4     gscale(*)
C   options
       integer    gopt(*)
C   number of data points
       integer    ndata
C   error bar options in x and y
       integer    errx, erry
C   error bar size
       real*4     etop
C   x, y, data and errors
       real*4     x(*), y(*), ex1(*), ey1(*), ex2(*), ey2(*)
C
C 
C plot error bars if required
C
C-
C local variables
       real*4     x1, x2, y1, y2
       integer    n

       if ( errx.eq.1 ) then
         do n=1,ndata
           if (gopt(1).eq.1) then
             x1 = log10(10.0**x(n) + ex1(n))
             x2 = log10(10.0**x(n) - ex2(n))
           else
             x1 = x(n) + ex1(n)
             x2 = x(n) - ex2(n)
           endif
           if (ex1(n).ne.0.0 .and. ex2(n).ne.0.0) then
             call pgerrx(1,x1,x2,y(n),2.0*etop)
           else
             call pgsah(2,35.0,1.0)
             if (ex1(n).ne.0.0) then
               call pgarro(x(n),y(n),x1,y(n))
             else
               call pgarro(x(n),y(n),x2,y(n))
             endif
           endif
         enddo
       endif
       if ( erry.eq.1) then
         do n=1,ndata
           if (gopt(2).eq.1) then
             y1 = log10(10.0**y(n) + ey1(n))
             y2 = log10(10.0**y(n) - ey2(n))
           else
             y1 = y(n) + ey1(n)
             y2 = y(n) - ey2(n)
           endif
           if (ey1(n).ne.0.0 .and. ey2(n).ne.0.0) then
             call pgerry(1,x(n),y1,y2,2.0*etop)
           else
             call pgsah(2,35.0,1.0)
             if (ey1(n).ne.0.0) then
               call pgarro(x(n),y(n),x(n),y1)
             else
               call pgarro(x(n),y(n),x(n),y2)
             endif
           endif
         enddo
       endif
       end
C
C
*+ pgPlotError

       subroutine pgPlotError( ndata, errx, erry, 
     *                          etop, x, y, ex1, ex2, ey1, ey2  )
C      ---------------------------------------------------------
C
C Plot Error bars for a curve
C
C Given:
C   number of data points
       integer    ndata
C   error bar options in x and y
       integer    errx, erry
C   error bar size
       real*4     etop
C   x, y, data and errors
       real*4     x(*), y(*), ex1(*), ey1(*), ex2(*), ey2(*)
C
C 
C plot error bars if required
C
C-
C local variables
       real*4     x1, x2, y1, y2
       integer    n

       if ( errx.eq.1 ) then
         do n=1,ndata
           x1 = x(n) + ex1(n)
           x2 = x(n) - ex2(n)
           if (ex1(n).ne.0.0 .and. ex2(n).ne.0.0) then
             call pgerrx(1,x1,x2,y(n),2.0*etop)
           else
             call pgsah(2,35.0,1.0)
             if (ex1(n).ne.0.0) then
               call pgarro(x(n),y(n),x1,y(n))
             else
               call pgarro(x(n),y(n),x2,y(n))
             endif
           endif
         enddo
       endif
       if ( erry.eq.1) then
         do n=1,ndata
           y1 = y(n) + ey1(n)
           y2 = y(n) - ey2(n)
           if (ey1(n).ne.0.0 .and. ey2(n).ne.0.0) then
             call pgerry(1,x(n),y1,y2,2.0*etop)
           else
             call pgsah(2,35.0,1.0)
             if (ey1(n).ne.0.0) then
               call pgarro(x(n),y(n),x(n),y1)
             else
               call pgarro(x(n),y(n),x(n),y2)
             endif
           endif
         enddo
       endif
       end
