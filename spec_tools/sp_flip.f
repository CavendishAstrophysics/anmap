C FLIP: flip a spectrum
C
       implicit    NONE
C
C
C Multi column data are handled.  The spectral file is flipped so that the
C pixel order is reversed.  The x-column remains unchanged.
C
C   
C Parameters:
C    outfile     ==  output file name
C    infile      ==  input file name
C    title       ==  new title for output file
C    xc          ==  x-column
C    comment     ==  use defined comment text
C
C Include information on standard array sizes etc.
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
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
C   amount by which to wrap output spectrum
       integer    wrap
C   counters
       integer    n, nn, nc

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'flip',
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
C .. find list of columns to scale
       call spec_getcols( id_from, ncols, clist, status )
       call cmd_itemi( 'wrap', 1, wrap, status )
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

C loop for each column in the list and flip
       if (status.ne.0) goto 999
       do nc=1,ncols
         call spec_get_data( id_from, nc, x, ndata, status)
         if (status.ne.0) goto 999
         if (clist(nc).eq.1) then
           do n=1,ndata
             nn = ndata-n+1+wrap
             if (nn.gt.ndata) nn = nn - ndata
             if (nn.lt.1) nn = ndata + nn
             y(nn) = x(n)
           end do
           call spec_put_data( id_to, nc, y, ndata, status)
         else
           call spec_put_data( id_to, nc, x, ndata, status)
         end if
       end do

C add history item
       call spec_hd_set( id_to, 'history','FLIP:',status )

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_to, status )

       end


