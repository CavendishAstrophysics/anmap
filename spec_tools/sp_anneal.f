C Anneal : simplified simulated annealing to solution of deconvolution
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
       integer    id_to, id_from, id_psf, id_guess
C   status return
       integer    status
C   number of data points
       integer    ndata, ndpsf
C   array to hold data values
       real*4     x(max_ndata), psf(max_ndata)
       real*4     c(max_ndata), b(max_ndata)
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
       logical       isset
C   list of columns to operate on
       integer    col, psf_col, ncols, niter, iter
C   convolve control data
       integer    ipsf
C   counters
       integer    n, nc, i
C   fraction
       real*4     frac, xf, ymax, ysum
C   variances
       real*4     var, var0
C   functions
       integer    irand
       common /normalize/ ysum

C setup random number generator
       i = irand(0)

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'sa',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)
C find input file names and allocate files
       call cmd_items( 'infile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_from, status)
       call cmd_items( 'psf', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_psf, status)
       call cmd_itemset( 'guess', isset, status )
       if (isset) then
         call cmd_items( 'guess', file, status)
         call spec_allocate( file(1:chr_lenb(file)),
     *                       'read', id_guess, status)
       else
         id_guess = -1
       endif
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
       call cmd_itemr( 'frac', 1, frac, status )
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
       if (status.ne.0) goto 999
       ysum = 0.0
       do n=1,ndpsf
         ysum = ysum + abs(psf(n))
       enddo
       ysum = ysum/(2.0*float(ndpsf))

C get data to deconvolve and copy to output
       if (status.ne.0) goto 999
       do nc=1,ncols
         call spec_get_data( id_from, nc, x, ndata, status)
         call spec_put_data( id_to, nc, x, ndata, status)
       enddo
       call spec_get_data( id_from, col, x, ndata, status)

C find maximum
       ymax = -1.0E+30
       do n=1,ndata
         ymax = max(ymax,abs(x(n)))
         c(n) = 0.0
       enddo
       if (id_guess.gt.0) then
         call spec_get_data( id_guess, col, c, ndata, status)
       endif
       xf = ymax*frac
       call spec_cnv1(ndata,c,b,ndpsf,psf,ipsf,status)
       call spec_getvar( ndata, x, c, b, var0 )
       do iter=1,niter
          i = mod(irand(0),ndata) + 1
          if (mod(irand(0),2).eq.0) then
            n = 1
          else
            n = -1
          endif
*          if (c(i).le.0) then
*            n = 1
*          endif
          c(i) = c(i) + n*xf
          call spec_cnv1(ndata,c,b,ndpsf,psf,ipsf,status)
          call spec_getvar( ndata, x, c, b, var )
          if (var.gt.var0) then
            c(i) =  c(i) - n*xf
          else
            var0 = var
          endif
       enddo
       call spec_put_data( id_to, col, c, ndata, status)
 

C add history item
       string = 'SA-Deconvolution: '
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_psf, status )
       call spec_deallocate( id_to, status )
       if (id_guess.gt.0) then
         call spec_deallocate( id_guess, status )
       endif
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
       subroutine spec_getvar( ndata, x, c, y, var )
C      ---------------------------------------------

       integer      ndata
       integer      n
       real*4       x(ndata), y(ndata), c(ndata), var
       real*4       ysum
       common /normalize/ ysum
       var = 0.0
       do n=1,ndata
         var = var + (x(n) - y(n))**2
       enddo
*       do n=1,ndata
*         if (c(n).gt.0.0) then
*            var = var + c(n)*log(c(n)/ysum)
*         endif
*       enddo
       end


