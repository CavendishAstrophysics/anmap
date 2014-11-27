C SMOOTH: smooth a spectrum
C
       implicit    NONE
C
C
C Multi column data are handled.  The binning is applied to all columns.
C
C Parameters:
C    outfile     ==  output file name
C    infile      ==  input file name
C    smooth_type ==  type of smoothing function:
C                    := BOXCAR [default]
C    width       ==  width of smoothing function interpreted as:
C                    for BOXCAR   := width of boxcar (cells)
C    XC          ==  X column, which is NOT smoothed
C    COLS        ==  list of columns to apply smoothing to
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
C   width of binning function
       integer    width
C   type of smoothing
       character  stype*20
C   status return
       integer    status
C   number of data points
       integer    ndata
C   array to hold data values
       real*4     x(max_ndata), y(max_ndata)
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   number of columns etc.
       integer    ncols, clist(max_ncols)
C   counters
       integer    n, nc, n1, n2, nn
       real*4     weight
C Functions
       integer    chr_lenb

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'smooth',
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
C .. find list of columns to smooth
       call spec_getcols( id_from, ncols, clist, status )
C .. smooth type
       call cmd_items( 'stype', stype, status )
C .. parameters used to smooth
       call cmd_itemi( 'width', 1, width, status )
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
       do nc=1,ncols
         call spec_get_data( id_from, nc, x, ndata, status)
         if (status.ne.0) goto 999
         if (clist(nc).gt.0) then
           do n=1,ndata
             n1 = max(1,n-width/2)
             n2 = min(ndata,n+width/2)
             y(n) = 0.0
             weight = 0.0
             do nn = n1,n2
               y(n) = y(n) + x(nn)
               weight = weight + 1.0
             end do
             y(n) = y(n)/weight
           end do
           call spec_put_data( id_to, nc, y, ndata, status)
         else
           call spec_put_data( id_to, nc, x, ndata, status)
         end if
       end do

C add history item
*       string = ' '
*       write(string,'(A,A,A,I4)')
*     *       'SMOOTH: type = ',stype(1:chr_lenb(stype)),
*     *       ' width = ',width
*       call spec_hd_set( id_to, 'history',
*     *                   string(1:chr_lenb(string)),
*     *                   status )

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_to, status )

       end

