C LOG: take logarithm of specified axes
C
       implicit    NONE
C
C
C Multi column data are handled.  Selected columns are "logged"
C and an output spectrum written.
C
C Parameters:
C    outfile     ==  output file name
C    infile      ==  input file name
C    cols        ==  list of columns to "log"
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
       integer    n, nc
C Functions
       integer    chr_lenb

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'log',
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
C .. find list of columns to "log"
       call spec_getcols( id_from, ncols, clist, status )
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

C loop for each column in the list and apply scaling/offset
       if (status.ne.0) goto 999
       do nc=1,ncols
         call spec_get_data( id_from, nc, x, ndata, status)
         if (status.ne.0) goto 999
         if (clist(nc).eq.1) then
           do n=1,ndata
             if (x(n).gt.0.0) then
               y(n) = log10(x(n))
             else
               y(n) = 0.0
             end if
           end do
           call spec_put_data( id_to, nc, y, ndata, status)
         else
           call spec_put_data( id_to, nc, x, ndata, status)
         end if
       end do

C add history item
       string = ' '
       write(string,'(A)')
     *       'LOG: log base 10'
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_to, status )

       end
