C
C
*+ pgskey

       subroutine pgskey( xp, yp, drop, fr )
C      -------------------------------------
C
C Plot a key on a 
C
C Given:
C    top-left corner of the key
       real*4      xp, yp
C    character drop
       real*4      drop
C    option to determine plotting of a frame
       integer     fr
C
C
C A key is added to the plot using the currently defined options.
C This routine initialises a new key.
C
C-

C local variables
C   current window
       real*4         pgkey_vp(4)
C   real-valued options
       real*4         dr, x1, x2, x3, y1, y2, 
     *                xmax, ymax, xpos, ypos, scale
C   integer-valued options
       integer        frame
C Save the option definitions
       common /pgkey_save/ pgkey_vp, dr, x1, x2, x3, y1, y2, 
     *                     xmax, ymax, frame, xpos, ypos, scale


C find current view-port/window, then reset
       call pgqwin( pgkey_vp(1), pgkey_vp(2), pgkey_vp(3), pgkey_vp(4) )
       call pgwindow( 0.0,1.0,0.0,1.0 )
       call pgqch( scale )
       call pgsave

C set coordinate locations
       dr = drop*scale*1.5/40.0
       frame = fr
       xmax = 0.0
       xpos = xp
       ypos = yp
       y1 = ypos - 0.5*dr - 0.025*scale
       y2 = ypos - dr - 0.025*scale
       x1 = xpos + 0.025*scale
       x2 = xpos + 0.15*scale
       x3 = xpos + 0.175*scale
       end

C
C
*+ pgakey

       subroutine pgakey( lls, llw, lci, sty, ssi, slw, sci, text)
C      -----------------------------------------------------------
C
C Plot a key on an existing key setup
C
C Given:
C   line attributes
       integer        lls, llw, lci
C   symbol attributes
       integer        sty, slw, sci
       real*4         ssi
C   supplied text
       character*128  text
C
C Another item is added to an existing key
C-

C local variables
       real*4         x, y
C   current window
       real*4         pgkey_vp(4)
C   real-valued options
       real*4         dr, x1, x2, x3, y1, y2, 
     *                xmax, ymax, xpos, ypos, scale
C   integer-valued options
       integer        frame
C Save the option definitions
       common /pgkey_save/ pgkey_vp, dr, x1, x2, x3, y1, y2, 
     *                     xmax, ymax, frame, xpos, ypos, scale

       integer        chr_lenb

       if (lls.gt.0) then
          call pgsci( lci )
          call pgsls( lls )
          call pgslw( llw )
          call pgmove(x1,y1)
          call pgdraw(x2,y1)
       endif
       if (sty.gt.0) then
          call pgsci( sci )
          call pgsls( 1 )
          call pgslw( slw )
          call pgsch( ssi )
          call pgpoint(1,0.5*(x1+x2),y1,sty)
       endif
       call pgunsa
       call pgtext(x3,y2,text(1:chr_lenb(text)))
       call pgsave
       call pgqpos(x,y)
       xmax = max(xmax,x)
       y1 = y1 - dr
       y2 = y2 - dr

       end
C
C
C
       subroutine pgekey
C      -----------------
C
C End the key building environment
C
C-
C local variables
C   current window
       real*4         pgkey_vp(4)
C   real-valued options
       real*4         dr, x1, x2, x3, y1, y2, 
     *                xmax, ymax, xpos, ypos, scale
C   integer-valued options
       integer        frame
C Save the option definitions
       common /pgkey_save/ pgkey_vp, dr, x1, x2, x3, y1, y2, 
     *                     xmax, ymax, frame, xpos, ypos, scale

C if required draw a frame around the key
       call pgunsa
       if (frame.eq.1) then
         xmax = xmax + 0.025*scale
         call pgmove(xpos,ypos)
         call pgdraw(xpos,y1)
         call pgdraw(xmax,y1)
         call pgdraw(xmax,ypos)
         call pgdraw(xpos,ypos)
       endif
       call pgswin( pgkey_vp(1), pgkey_vp(2), pgkey_vp(3), pgkey_vp(4) )
       call pgebuf
       end
