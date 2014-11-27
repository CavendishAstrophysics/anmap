C
C
C Support routines for PGPLOT tcl binding
C
C+ pgInitStream
C
       subroutine pgInitStream( device, psize, paspect, nx, ny, id, s )
C      ----------------------------------------------------------------
C
C Select and open a new stream for graphical output
C
C Given:
C   device specification
       character         device*128
C   device size and aspect ratio
       real*4            psize, paspect
C   number os sub-divisions of the frame
       integer           nx, ny
C   stream ID
       integer           id
C
C Updated:
C   error status
       integer           s, si, pgindev
C
C-
       integer           l, chr_lenb
       s = 0
       l = chr_lenb(device)
       si = pgindev( 1, device(1:l), nx, ny, id )
       if (si.eq.1) then
         if (psize .lt. 0.0) then
           psize = 0.0
         endif
         if (paspect.lt.0.05) then
           paspect = 0.75
         endif
         call pgask( .false. )
         call pgpap(psize,paspect)
         call pgadvance
         call pgpap(psize,paspect)
         s = 0
       else
         s = 1
       endif
       end
C 
C
C+ pgClearScreen
C
       subroutine pgClearScreen
C      ------------------------
C
C Clear the current device
C
C-
       real*4     x1,x2,y1,y2,u1,u2,v1,v2
       call pgsave
       call pgqwin(x1,x2,y1,y2)
       call pgqvp(0,u1,u2,v1,v2)
       call pgvport(0.0,1.0,0.0,1.0)
       call pgwindow(0.0,1.0,0.0,1.0)
       call pgsfs( 1 )
       call pgsci( 0 )
       call pgrect( 0.0,1.0,0.0,1.0 )
       call pgunsa
       call pgvport(u1,u2,v1,v2)
       call pgwindow(x1,x2,y1,y2)
       end
C
C
C+ pgClearVport
       subroutine pgClearVport
C      -----------------------
C
C Clear the current viewport
C-
       real*4     x1,x2,y1,y2
       call pgsave
       call pgqwin(x1,x2,y1,y2)
       call pgwindow(0.0,1.0,0.0,1.0)
       call pgsfs( 1 )
       call pgsci( 0 )
       call pgrect( 0.0,1.0,0.0,1.0 )
       call pgunsa
       call pgwindow(x1,x2,y1,y2)
       end
C
C
C+ pgTextString

       subroutine pgTextString( text, x, y, r, j )
C      -------------------------------------------
C
C Draw a text string in world corrdinate space
C
C Given:
C    text
       character*128     text
C    position
       real*4            x, y
C    rotation angle
       real*4            r
C    justification
       real*4            j
C
C-
       integer       chr_lenb
       call pgptext( x, y, r, j, text(1:chr_lenb(text)) )
       end
C
C
C+ pgTextLabel

       subroutine pgTextLabel( side, text, x, y, j )
C      ---------------------------------------------
C
C Draw a text string in world corrdinate space
C
C Given:
C    side specified as a character option
       character*2       side
C    text
       character*128     text
C    displacement and coordinate
       real*4            x, y
C    justification
       real*4            j
C
C-
       integer       l
       integer       chr_lenb
       l = chr_lenb(text)
       call pgmtext( side(1:chr_lenb(side)), 
     *               x, y, j, text(1:chr_lenb(text)) )
       end
C
C
C+ pgDrawBox

       subroutine pgDrawBox( type, xopt, xtick, nx, yopt, ytick, ny )
C      --------------------------------------------------------------
C
C Draw a labelled box
C
C Given:
C    type of box (0=normal ; 1=ra/dec (time))
       integer    type
C    options
       character  xopt*10, yopt*10
C    tick interval
       real*4     xtick, ytick
C    number of ticks
       integer    nx, ny
C
C-
       integer       lx, ly
       integer       chr_lenb
       lx = chr_lenb(xopt)
       ly = chr_lenb(yopt)
       if (type.eq.0) then
         call pgbox( xopt(1:lx),xtick,nx,yopt(1:ly),ytick,ny)
       else
         call pgtbox( xopt(1:lx),xtick,nx,yopt(1:ly),ytick,ny)
       endif
       end
C
C
C+ pgReadCursor

       subroutine pgReadCursor( x, y, c )
C      ----------------------------------
C
C Read the cursor position and character typed
C
C Returned:
C    cursor position
       real*4     x, y
C    character typed
       character  c*1
C
C-
       call pgcurse( x, y, c )
       end
C
C
C+ pgDrawBinned

       subroutine pgDrawBinned ( n, x, y )
C      -----------------------------------
C
C Plot binned data
C
C Given:
C    number of data points
       integer    n
C    data arrays
       real*4     x(*), y(*)
C
C-
       call pgbin( n, x, y, .true. )
       end
C
C
C+ pgOpenSpectrum

       subroutine pgOpenSpectrum( file, id, ndata )
C      --------------------------------------------
C
C Open and parse a standard format spectral file
C
C Given:
C    file name
       character*128     file
C
C Returned:
C    file id
       integer           id
C    number of data points
       integer           ndata
C-

C local variables
       integer    status
C functions
       integer    chr_lenb

C open file
       status = 0
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id, status)
       call spec_hd_enqi( id, 'ndata', ndata, status )
       if (status.ne.0) ndata = 0
       end
C
C
C+ pgReadSpectrum

       subroutine pgReadSpectrum( id, ix, iy, iex, iex2, iey, iey2,
     *                            ndata, x,  y,  ex,  ex2,  ey,  ey2 )
C      ---------------------------------------------------------------
C
C Read a standard format spectral file
C
C Given:
C    file id
       integer           id
C    column flags
       integer           ix, iy, iex, iex2, iey, iey2
C
C Returned:
C    data points
       integer           ndata
C    data arrays
       real*4            x(*), y(*), ex(*), ex2(*), ey(*), ey2(*)
C
C-

C local variables
       integer    status, nc

C open file
       call spec_hd_enqi( id, 'ncols', nc, status )

C check status and continue
       if (status.eq.0 .and. ndata.gt.0) then
          if (ix.le.nc .and. ndata.gt.0) then
             call spec_get_data( id, ix, x, ndata, status)
          else
             ndata = 0
          endif
          if (iy.le.nc .and. ndata.gt.0) then
             call spec_get_data( id, iy, y, ndata, status)
          else
             ndata = 0
          endif
          if (iex.gt.0) then
             if (iex.le.nc .and. ndata.gt.0) then
                call spec_get_data( id, iex, ex, ndata, status)
             else
                ndata = 0
             endif
          endif
          if (iey.gt.0) then
             if (iey.le.nc .and. ndata.gt.0) then
             call spec_get_data( id, iey, ey, ndata, status)
             else
                ndata = 0
             endif
          endif
          if (iex2.gt.0) then
             if (iex2.le.nc .and. ndata.gt.0) then
                call spec_get_data( id, iex2, ex2, ndata, status)
             else
                ndata = 0
             endif
          endif
          if (iey2.gt.0) then
             if (iey2.le.nc .and. ndata.gt.0) then
                call spec_get_data( id, iey2, ey2, ndata, status)
             else
                ndata = 0
             endif
          endif
       else
          ndata = 0
       endif
       call spec_deallocate( id, status )
       if (status.ne.0) ndata = 0
       end
