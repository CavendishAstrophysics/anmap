C MAKE_HISTOGRAM: construct a histogram from selected data
C
       implicit    NONE
C
C
C Multi column data are handled.  A histogram of data values in a
C given column is constructed.  The output file is a two-column file
C of the histogram data -- this can be displayed with the
C sp_display command.
C
C Parameters:
C    outfile     ==  output file name
C    infile      ==  input file name
C    col         ==  column for data
C    bins        ==  number of bins
C    low         ==  lower limit for bin 1
C    high        ==  upper limit for bin "bins"
C    title       ==  new title for output file
C    comment     ==  use defined comment text
C
C Include information on standard array sizes etc.
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
C
C Local variables
C   file identifiers
       integer    id_to, id_from
C   parameters for the histogram
       integer    bins
       real*4     low, high, xmax, xmin
C   status return
       integer    status
C   number of data points
       integer    ndata
C   array to hold data values
       real*4     x(max_ndata), y(max_ndata)
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   column for histogram data
       integer    col
C   counters
       integer    n, ibin
C Functions
       integer    chr_lenb

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'make_histogram',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)
C find input file names and allocate files
       call cmd_items( 'infile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_from, status)
C find output file name and allocate
       call cmd_items( 'outfile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'write', id_to, status)
C copy output descriptors
       call spec_hd_copy( id_from, id_to, status )

C read control data
C .. find column for output
       call cmd_itemi( 'bins', 1, bins, status )
       call cmd_itemi( 'col', 1, col, status )
C .. parameters used to determine range
       call cmd_itemr( 'low', 1, low, status )
       call cmd_itemr( 'high', 1, high, status )
C .. add user text to header
       call cmd_items( 'title', string, status )
       if (chr_lenb(string).gt.0) then
         call spec_hd_set( id_to, 'title',
     *                     string(1:chr_lenb(string)), status )
       end if
       call cmd_items( 'comment', string, status )
       if (chr_lenb(string).gt.0) then
         call spec_hd_set( id_to, 'comment',
     *                     string(1:chr_lenb(string)), status )
       end if
       if (status.ne.0) goto 999

C get data
       call spec_get_data( id_from, col, x, ndata, status)
       xmax = -1.0E+30
       xmin = 1.0E+30
       do n=1,ndata
         xmin = min(xmin,x(n))
         xmax = max(xmax,x(n))
       enddo
       if (low.eq.0.0 .and. high.eq.0.0) then
         low = xmin
         high = xmax
       endif
       if (status.ne.0) goto 999
       do n=1,bins
         y(n) = 0.0
       end do
       do n=1,ndata
         ibin = 1 + float(bins)*(x(n)-low)/(high-low)
         if (ibin.gt.0 .and. ibin.le.bins) then
           y(ibin) = y(ibin) + 1
         endif
       enddo
       do n=1,bins
         x(n) = low + (float(n)-0.5)*(high-low)/float(bins)
       enddo
       call spec_put_data( id_to, 1, x, bins, status)
       call spec_put_data( id_to, 2, y, bins, status)

C add history item
       string = ' '
       write(string,'(A,1PE10.2,A,1PE12.2)')
     *       'MHIST: low   = ',low,
     *       ' high = ',high
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )
       call spec_hd_set( id_to, 'ncols', '2', status )

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_to, status )

       end

