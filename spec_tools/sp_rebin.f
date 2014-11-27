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
C   width of binning function
       integer    width
C   status return
       integer    status
C   number of data points
       integer    ndata, new_ndata
C   array to hold data values
       real*4     x(max_ndata), y(max_ndata),
     *            xnew(max_ndata), xold(max_ndata)
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   number of columns
       integer    ncols
C   flag to indicate averaging
       logical    average
C   counters
       integer    n, nc, n1, n2, nn, xc
       real*4     weight
C Key to specify re-binning to a fixed bin width
       logical    fixed_width
       real*4     binsize
C Functions
       integer    chr_lenb

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'rebin',
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
C .. X-column -- needed if AVERAGE is not set
       call cmd_itemi( 'xc', 1, xc, status )
C .. parameters used to rebin
       call cmd_itemi( 'width', 1, width, status )
       call cmd_itemset( 'average', average, status )
       call cmd_itemset( 'binsize', fixed_width, status )
       if (fixed_width) then
         call cmd_itemr( 'binsize', 1, binsize, status )
       end if
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

C loop for each column in the list and rebin
       if (status.ne.0) goto 999
       if (fixed_width) then
         call spec_get_data( id_from, xc, xold, ndata, status )
         new_ndata = (xold(ndata)-xold(1))/binsize
         do n=1,new_ndata
           xnew(n) = xold(1) + float(n-1)*binsize
         end do
         call spec_put_data( id_to, xc, xnew, new_ndata, status )
       end if
       do nc=1,ncols
         call spec_get_data( id_from, nc, x, ndata, status)
         if (status.ne.0) goto 999
         if (.not.fixed_width) then
           new_ndata = 0
           do n=1,ndata,width
             n1 = n
             n2 = n+width-1
             if (n2.le.ndata) then
               new_ndata = new_ndata + 1
               y(new_ndata) = 0.0
               weight = 0.0
               do nn = n1,n2
                 y(new_ndata) = y(new_ndata) + x(nn)
                 weight = weight + 1.0
               end do
               if (average .or. nc.eq.xc) then
                 y(new_ndata) = y(new_ndata)/weight
               end if
             end if
           end do
           call spec_put_data( id_to, nc, y, new_ndata, status)
         else if (nc.ne.xc) then
           n1 = 1
           do n=1,new_ndata-1
             y(n) = 0.0
             do nn=n1,ndata
               if (xold(nn).ge.xnew(n) .and. xold(nn).lt.xnew(n+1))
     *           then
                 y(n) = y(n) + x(nn)
                 n2 = nn
               end if
             end do
             n1 = n2 + 1
           end do
           n = new_ndata
           y(n) = 0.0
           do nn=n2,ndata
             y(n) = y(n) + x(nn)
           end do
           call spec_put_data( id_to, nc, y, new_ndata, status)
         end if
       end do

C add history item
       string = ' '
       write(string,'(A,I4)')
     *       'REBIN: width = ',width
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_to, status )

       end

