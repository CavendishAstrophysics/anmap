C RL : richard Lucy deconvolution algorithm
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
       real*4     x(max_ndata), psf(max_ndata)
       real*4     c(max_ndata), b(max_ndata)
       real*4     cc(max_ndata), bb(max_ndata)
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   list of columns to operate on
       integer    col, psf_col, ncols, niter, iter
C   convolve control data
       integer    ipsf
C   sum on psf array
       real*4     ysum
C   counters
       integer    n, nc

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'rl',
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
       call spec_hd_enqi( id_from, 'ncols', ncols, status)
       call cmd_itemi( 'psfcol', 1, psf_col, status)
       call cmd_itemi( 'niter', 1, niter, status )
       call cmd_itemi( 'col', 1, col, status )
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

C read psf data
       call spec_get_data( id_psf, psf_col, psf, ndpsf, status)
       ipsf = ndpsf/2
       ysum = 0.0
       do n=1,ndpsf
           ysum = ysum + abs(psf(n))
       enddo
       do n=1,ndpsf
           psf(n) = psf(n) / ysum
       enddo
       if (status.ne.0) goto 999

C get data to deconvolve and copy to output
       if (status.ne.0) goto 999
       do nc=1,ncols
         call spec_get_data( id_from, nc, x, ndata, status)
         call spec_put_data( id_to, nc, x, ndata, status)
       enddo
       call spec_get_data( id_from, col, x, ndata, status)
       do n=1,ndata
         b(n) = x(n)
       enddo
       do iter=1,niter
          call spec_cnv1(ndata,b,c,ndpsf,psf,ipsf,status)
          do n=1,ndata
            if (c(n).ne.0.0) then
              cc(n) = x(n)/c(n)
            else
              cc(n) = 1.0
            endif
          enddo
          call spec_cnv2(ndata,cc,bb,ndpsf,psf,ipsf,status)
          do n=1,ndata
            b(n) = bb(n)*b(n)
          enddo
       enddo
       call spec_put_data( id_to, col, b, ndata, status)
 

C add history item
       string = 'RL-Deconvolution: '
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
       subroutine spec_cnv1(ndata,x,cn,ndpsf,psf,ipsf,status)
C      ------------------------------------------------------
C
       integer   ndata, ndpsf, ipsf, status
       real*4    x(ndata), cn(ndata), psf(ndpsf)
       integer   i1, i2, n, i
       do n=1,ndata
         cn(n) = 0.0
         i1 = max(1,n+ipsf-ndpsf)
         i2 = min(ndpsf,n+ipsf-1)
         do i=i1,i2
           cn(n) = cn(n) + x(i)*psf(n+ipsf-i)
         enddo
       enddo
       end
C
C
       subroutine spec_cnv2(ndata,x,cn,ndpsf,psf,ipsf,status)
C      ------------------------------------------------------
C
       integer   ndata, ndpsf, ipsf, status
       real*4    x(ndata), cn(ndata), psf(ndpsf)
       integer   i1, i2, n, i
       do n=1,ndata
         cn(n) = 0.0
         i1 = max(1,n+ipsf+1)
         i2 = min(ndpsf,n+ipsf+ndpsf)
         do i=i1,i2
           cn(n) = cn(n) + x(i)*psf(i-ipsf-n)
         enddo
       enddo
       end




