C CLEAN : clean a spectrum with a supplied convolving function
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
       integer    id_to, id_from, id_psf, id_resid
C   status return
       integer    status
C   number of data points
       integer    ndata, ndpsf
C   array to hold data values
       real*4     x(max_ndata), psf(max_ndata), cl(max_ndata)
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   list of columns to operate on
       integer    col, psf_col, ncols
C   clean control data
       integer    niter, ipsfp, ipsfn, ipsfc
       real*4     dmin, frac
C   search on psf array
       real*4     ymin, ymax
C   counters
       integer    n, nc

C   options
       logical    cs_clean, positive
       integer    zero, nlist, nvar
       common /clean_opts/ cs_clean, positive, zero, nlist, nvar

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'clean',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)
C find input file names and allocate files
       call cmd_items( 'infile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_from, status)
       call cmd_items( 'psf', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_psf, status)
C find output file names and allocate
       call cmd_items( 'outfile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'write', id_to, status)
       call cmd_items( 'residual', file, status)
       if (file(1:4).ne.'NONE') then
         call spec_allocate( file(1:chr_lenb(file)),
     *                       'write', id_resid, status)
       else
         id_resid = 0
       endif
C copy output descriptors
       call spec_hd_copy( id_from, id_to, status )
       if (id_resid.ne.0) then
         call spec_hd_copy( id_from, id_resid, status )
       endif


C read control data
C .. get column to clean and to use in the psf
       call cmd_itemi( 'col', 1, col, status)
       call cmd_itemi( 'psfcol', 1, psf_col, status)
       call spec_hd_enqi( id_from, 'ncols', ncols, status)
C .. get control data for the clean
       call cmd_itemi( 'niter', 1, niter, status)
       call cmd_itemi( 'zero', 1, zero, status)
       call cmd_itemi( 'nlist', 1, nlist, status)
       call cmd_itemi( 'nvar', 1, nvar, status)
       call cmd_itemr( 'dmin', 1, dmin, status)
       call cmd_itemr( 'frac', 1, frac, status)
       call cmd_itemset( 'cs_clean', cs_clean, status )
       call cmd_itemset( 'positive', positive, status )
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

C loop for each column in the list and clean
       call spec_get_data( id_psf, psf_col, psf, ndpsf, status)
       ymin = 1.0E+30
       ymax = -1.0E+30
       do n=1,ndpsf
         if (psf(n).gt.ymax) then
           ymax = psf(n)
           ipsfp = n
         endif
         if (psf(n).lt.ymin) then
           ymin = psf(n)
           ipsfn = n
         endif
       enddo
       if (cs_clean) then
         ipsfc = ndpsf/2
       else
         ipsfc = ipsfp
         ipsfn = ipsfp
       endif
       if (status.ne.0) goto 999
       do nc=1,ncols
         call spec_get_data( id_from, nc, x, ndata, status)
         if (status.ne.0) goto 999
         if (nc.ne.col) then
C .. copy this column
           call spec_put_data( id_to, nc, x, ndata, status)
           if (id_resid.ne.0) then
             call spec_put_data( id_resid, nc, x, ndata, status)
           endif
         else
           call spec_clean( ndata, x, cl, ndpsf, psf, 
     *                      niter, dmin, frac, ipsfp, ipsfn, ipsfc,
     *                      status )
           call spec_put_data( id_to, nc, cl, ndata, status)
           if (id_resid.ne.0) then
             call spec_put_data( id_resid, nc, x, ndata, status)
           endif
         end if
       end do

C add history item
       string = ' '
       write(string,'(A,I6,A,1PE10.2,A,1PE10.2)')
     *       'CLEAN: niter = ',niter,' frac = ',frac,
     *               '  dmin = ',dmin
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )
       string = ' '

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_psf, status )
       call spec_deallocate( id_to, status )
       if (id_resid.ne.0) then
         call spec_deallocate( id_resid, status )
       endif

       end
C
C
C
       subroutine spec_clean( ndata, x, cl, ndpsf, psf, 
     *                        niter, dmin, frac, ipsfp, ipsfn, ipsfc,
     *                        status )
C      --------------------------------------------------------------
C
       integer   ndata, ndpsf, niter, ipsfp, ipsfn, ipsfc, status
       real*4    x(ndata), cl(ndata), psf(ndpsf), dmin, frac
       real*4    ymin, ymax, yamax
       integer   imin, imax, iamax
       integer   ipsf, n1, n2, n, iter

C   options
       logical    cs_clean, positive
       integer    zero, nlist, nvar
       common /clean_opts/ cs_clean, positive, zero, nlist, nvar

       ymin = 1.0E+30
       ymax = -1.0E+30
       do n=1,ndata
         cl(n) = 0.0
         if (x(n).gt.ymax) then
            ymax = x(n)
            imax = n
         endif
         if (x(n).lt.ymin) then
            ymin = x(n)
            imin = n
         endif
       enddo
       if (positive) then
         yamax = ymax
         iamax = imax
       else
         if (ymin.lt.0.0 .and. abs(ymin).gt.ymax) then
            yamax = -ymin
            iamax = imin
         else
            yamax = ymax
            iamax = imax
         endif
       endif
       iter = 1
       do while (iter.le.niter .and. yamax.gt.dmin)
         if (ymin.lt.0.0 .and. abs(ymin).gt.ymax) then
            ipsf = ipsfn
         else
            ipsf = ipsfp
         endif
         n1 = max(1,1+iamax-ipsf)
         n2 = min(ndata,ndpsf+iamax-ipsf)
         cl(iamax) = cl(iamax) + frac*x(iamax)
         do n=n1,n2
           x(n) = x(n) - frac*psf(n+ipsf-iamax)*x(iamax)/psf(ipsf)
         enddo
         if (zero.gt.0) then
           do n=1,zero
             x(n) = 0.0
           enddo
           do n=(ndata-zero+1),ndata
             x(n) = 0.0
           enddo
         endif
         ymin = 1.0E+30
         ymax = -1.0E+30
         do n=1,ndata
           if (x(n).gt.ymax) then
              ymax = x(n)
              imax = n
           endif
           if (x(n).lt.ymin) then
              ymin = x(n)
              imin = n
           endif
         enddo
         if (positive) then
           yamax = ymax
           iamax = imax
         else
           if (ymin.lt.0.0 .and. abs(ymin).gt.ymax) then
              yamax = -ymin
              iamax = imin
           else
              yamax = ymax
              iamax = imax
           endif
         endif
         iter = iter + 1
       enddo

       end









