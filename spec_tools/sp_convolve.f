C CONVOLVE : convolve a spectrum with a convolving psf
C
       implicit    NONE
C
C
C Parameters:
C    outfile   ==  output file name
C    infile    ==  input file name
C    psf       ==  point-spread function file name
C    title     ==  new title for output file
C    comment   ==  use defined comment text
C
C Include information on standard array sizes etc.
       include '/mrao/include/chrlib_functions.inc'
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
C
C Local variables
C   file identifiers
       integer    id_to, id_from, id_psf
C   status return
       integer    status
C   number of data points
       integer    ndata, ndpsf
C   array to hold data values
       real*4     x(max_ndata), psf(max_ndata), cn(max_ndata)
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   list of columns to operate on
       integer    clist(max_ncols), psf_col, ncols
C   convolve control data
       integer    ipsf
C   search on psf array
       real*4     ymax
C   counters
       integer    n, nc

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'convolve',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)
C find input file names and allocate files
       call cmd_items( 'infile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_from, status)
       call cmd_items( 'psf', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_psf, status)
C find output file name and allocate
       call cmd_items( 'outfile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'write', id_to, status)
C copy output descriptors
       call spec_hd_copy( id_from, id_to, status )

C read control data
C .. get column to clean and to use in the psf
       call spec_getcols( id_from, ncols,  clist, status )
       call cmd_itemi( 'psfcol', 1, psf_col, status)
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

C loop for each column in the list and convolve
       call spec_get_data( id_psf, psf_col, psf, ndpsf, status)
       ymax = -1.0E+30
       do n=1,ndpsf
         if (abs(psf(n)).gt.ymax) then
           ymax = psf(n)
           ipsf = n
         endif
       enddo
       ipsf = ndpsf/2
       if (status.ne.0) goto 999
       do nc=1,ncols
         call spec_get_data( id_from, nc, x, ndata, status)
         if (status.ne.0) goto 999
         if (clist(nc).le.0) then
C .. copy this column
           call spec_put_data( id_to, nc, x, ndata, status)
         else
           call spec_convolve( ndata, x, cn, ndpsf, psf, ipsf, status )
           call spec_put_data( id_to, nc, cn, ndata, status)
         end if
       end do

C add history item
       string = 'CONVOLVE: '
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_psf, status )
       call spec_deallocate( id_to, status )

       end
C
C
C
       subroutine spec_convolve(ndata,x,cn,ndpsf,psf,ipsf,status)
C      ----------------------------------------------------------
C
       integer   ndata, ndpsf, ipsf, status
       real*4    x(ndata), cn(ndata), psf(ndpsf)
       integer   n1, n2, n, i
       do n=1,ndata
         cn(n) = 0.0
       enddo
       do i=1,ndata
         n1 = max(1,1+i-ipsf)
         n2 = min(ndata,ndpsf+i-ipsf)
         do n=n1,n2
           cn(n) = cn(n) + x(i)*psf(n+ipsf-i)
         enddo
       enddo

       end




