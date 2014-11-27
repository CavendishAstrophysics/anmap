C REBIN: Rebin a spectrum to a lower sampling
C
       implicit    NONE
C
C
C Multi column data are handled.  The binning is applied to all columns.
C
C Parameters:
C    outfile   ==  output file name
C    infile    ==  input file name
C    width     ==  width of new bin in units of existing bin/pixel
C    average   ==  KEY to specify averaging of data by number of
C                  bins
C    XC        ==  X column, only used if AVERAGE is not set.
C                  X-column is always averaged
C    title     ==  new title for output file
C    comment   ==  use defined comment text
C
C Include information on standard array sizes etc.
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
C
C Local variables
C   file identifiers
       integer    id_to, id_from
C   status return
       integer    status
C   number of data points
       integer    ndata, new_ndata
C   array to hold data values
       real*4     x(max_ndata), y(max_ndata), xmin, xmax
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   number of columns
       integer    ncols
C   counters
       integer    n, nc, nn, xc
       integer    ix(max_ndata)
C Functions
       integer    chr_lenb

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'shrink',
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
C .. find number of columns on input
       call spec_hd_enqi( id_from, 'ncols', ncols, status )
C .. X-column and range
       call cmd_itemi( 'xc', 1, xc, status )
       call cmd_itemr( 'xmin', 1, xmin, status )
       call cmd_itemr( 'xmax', 1, xmax, status )
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

C loop for each column in the list and shrink
       if (status.ne.0) goto 999
       call spec_get_data( id_from, xc, x, ndata, status )
       nn = 0
       do n=1,ndata
           if (x(n).ge.xmin .and. x(n).le.xmax) then
              nn = nn + 1
              ix(n) = 1
           else
              ix(n) = 0
           endif
       end do
       new_ndata = nn
       do nc=1,ncols
         call spec_get_data( id_from, nc, x, ndata, status)
         nn = 0
         do n=1,ndata
           if (ix(n).eq.1) then
              nn = nn + 1
              y(nn) = x(n)
           endif
         enddo
         call spec_put_data( id_to, nc, y, new_ndata, status)
       end do

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_to, status )

       end

