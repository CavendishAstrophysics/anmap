C ZAP: zap a specified range of a spectrum
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
       real*4     x(max_ndata), y(max_ndata), 
     *            d(max_ndata)
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   number of columns etc.
       integer    ncols, clist(max_ncols), xc, yc
C   ranges
       real*4     xr(2,9), yr(2,9), v
       logical    set
C   counters
       integer    n, nn, nc

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'zap',
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
C .. find list of columns to zap
       call spec_getcols( id_from, ncols, clist, status )
C .. find ranges to zap
       do n=1,9
         do nn=1,2
           xr(nn,n) = 0.0
           yr(nn,n) = 0.0
         enddo
       enddo
       call cmd_itemr( 'xr1', 2, xr(1,1), status )
       call cmd_itemr( 'xr2', 2, xr(1,2), status )
       call cmd_itemr( 'xr3', 2, xr(1,3), status )
       call cmd_itemr( 'xr4', 2, xr(1,4), status )
       call cmd_itemr( 'xr5', 2, xr(1,5), status )
       call cmd_itemr( 'xr6', 2, xr(1,6), status )
       call cmd_itemr( 'xr7', 2, xr(1,7), status )
       call cmd_itemr( 'xr8', 2, xr(1,8), status )
       call cmd_itemr( 'xr9', 2, xr(1,9), status )
       call cmd_itemr( 'yr1', 2, yr(1,1), status )
       call cmd_itemr( 'yr2', 2, yr(1,2), status )
       call cmd_itemr( 'yr3', 2, yr(1,3), status )
       call cmd_itemr( 'yr4', 2, yr(1,4), status )
       call cmd_itemr( 'yr5', 2, yr(1,5), status )
       call cmd_itemr( 'yr6', 2, yr(1,6), status )
       call cmd_itemr( 'yr7', 2, yr(1,7), status )
       call cmd_itemr( 'yr8', 2, yr(1,8), status )
       call cmd_itemr( 'yr9', 2, yr(1,9), status )
       call cmd_itemset( 'xr', set, status )
       if (set) then
         call cmd_itemr( 'xr',  2, xr(1,1), status )
       endif
       call cmd_itemset( 'yr', set, status )
       if (set) then
         call cmd_itemr( 'yr',  2, yr(1,1), status )
       endif

C .. get column descriptors
       call cmd_itemi( 'xc', 1, xc, status )
       call cmd_itemi( 'yc', 1, yc, status )
       call cmd_itemr( 'value', 1, v, status )

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
       call spec_get_data( id_from, xc, x, ndata, status )
       call spec_get_data( id_from, yc, y, ndata, status )
       do nc=1,ncols
         call spec_get_data( id_from, nc, d, ndata, status)
         if (status.ne.0) goto 999
         if (clist(nc).eq.1) then
           do nn=1,9
             if (xr(1,nn).ne.xr(2,nn)) then
               do n=1,ndata
                 if ((x(n).ge.xr(1,nn)).and.(x(n).le.xr(2,nn))) then
                   d(n) = v
                 endif
               end do
             endif
             if (yr(1,nn).ne.yr(2,nn)) then
               do n=1,ndata
                 if ((y(n).ge.yr(1,nn)).and.(y(n).le.yr(2,nn))) then
                   d(n) = v
                 endif
               end do
             endif
           enddo
           call spec_put_data( id_to, nc, d, ndata, status)
         else
           call spec_put_data( id_to, nc, x, ndata, status)
         end if
       end do

C add history item
       call spec_hd_set( id_to, 'history','ZAP:',status )

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_to, status )

       end





