C NOISE: determine the noise in regions of a spectrum
C
       implicit    NONE
C
C Include information on standard array sizes etc.
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
C
C Local variables
C   file identifiers
       integer    id
C   status return
       integer    status
C   file and directory name(s)
       character  file*100, dir*100
C   data
       real*4     x(max_ndata), y(max_ndata)
C   statistics
       real*4     xsum, ysum, ys, noise
C   number of data/columns and counters
       integer    ndata, n, nn, xc, yc
C   windows to use for data
       integer    max_win, nw
       parameter (max_win = 9)
       real*4     win(2,max_win)
       integer    iflag(max_ndata)
       logical    win_set
       character  string*7
C Functions
       integer    chr_lenb

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'noise',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)

C find input file name and allocate file
       call cmd_items( 'infile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id, status)

C find columns to use in noise analysis and windowing
       call cmd_itemi( 'xc', 1, xc, status )
       call cmd_itemi( 'yc', 2, yc, status )

C enquire number of columns and loop for each
       call spec_get_data( id, xc, x, ndata, status)
       call spec_get_data( id, yc, y, ndata, status)
       do n=1,ndata
         iflag(n) = 1
       enddo
       nw = 1
       write(string,'(A,I1)') 'window',nw
       call cmd_itemset( string , win_set, status )
       if (.not.win_set) nw = 0
       do while (win_set .and. nw.le.9)
         call cmd_itemr( string, 2, win(1,nw), status )
         nw = nw + 1
         write(string,'(A,I1)') 'window',nw
         call cmd_itemset( string , win_set, status )
       enddo
       if (nw.gt.0) then
         do n=1,ndata
           iflag(n) = 0
           do nn=1,nw
             if ( x(n).ge.min(win(1,nn),win(2,nn)) .and.
     *            x(n).le.max(win(1,nn),win(2,nn)) ) then
               iflag(n) = 1
             endif
           enddo
         enddo
       endif

C work out the noise for the specified pixels
       xsum = 0.0
       ysum = 0.0
       do n=1,ndata
         if (iflag(n).eq.1) then
           xsum = xsum + 1
           ysum = ysum + y(n)
         endif
       enddo
       ysum = ysum/xsum
       ys = 0.0
       do n=1,ndata
         if (iflag(n).eq.1) then
           ys = ys + (y(n)-ysum)**2
         endif
       enddo
       noise = sqrt( ys/(xsum-1) )
       print 10,'Noise ',noise,'; Xsum ',xsum,'; Ysum ',ysum
10     format(' ',A,1PE12.4,A,1PE12.4,A,1PE12.4)

C deallocate spectrum
       call spec_deallocate( id, status )

       end

