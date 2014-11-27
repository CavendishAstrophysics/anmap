C FFT: take fft of data
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
C    real        ==  real column
C    image       ==  imaginary column
C    xc          ==  x-column
C    xfftc       ==  x-column to use after FFT
C    fill        ==  factor to zero-fill data (integer) 
C    wrap        ==  wrap output/input about centre of spectrum
C                    for forward/reverse transform
C    new_axis    ==  force constrcution of new x-axis
C    title       ==  new title for output file
C    comment     ==  use defined comment text
C
C Include information on standard array sizes etc.
       include '/mrao/include/chrlib_functions.inc'
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
C
C Local variables
C   file identifiers
       integer    id_to, id_from
C   status return
       integer    status
C   number of data points and columns
       integer    ndata, ncols
C   array to hold data values
       real*4     x(max_ndata), yr(max_ndata), yi(max_ndata),
     *                          ya(max_ndata), yp(max_ndata)
       complex    arr(16384)
       integer    narr
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   number of columns etc.
       integer    ir, im, xc, xfftc, idir, fill
C   counters
       integer    n, nt, ioff, noff
       real*4     del, xfill
C   logical flag to test input parameter settings
       logical    set, new_axis, same_axis, wrap, amp

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'fft',
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
C .. find real and imaginary columns
       narr = 2
       call cmd_itemi( 'real', 1, ir, status )
       call cmd_itemi( 'imag', 1, im, status )
       call cmd_itemi( 'ntrans', 1, narr, status )
       call cmd_itemi( 'xc', 1, xc, status )
       call cmd_itemi( 'xfftc', 1, xfftc, status )
       call cmd_itemi( 'fill', 1, fill, status )
       xfill = fill
       call cmd_itemset( 'new_axis', new_axis, status )
       call cmd_itemset( 'same_axis', same_axis, status )
       call spec_hd_enqi( id_from, 'ncols', ncols, status )
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
       if (status.ne.0) goto 999
       call cmd_itemset( 'forward', set, status )
       idir = 1
       if (set) then
         idir = 1
       else
         call cmd_itemset( 'backward', set, status )
         if (set) then
           idir = -1
         else
           call cmd_itemset( 'inverse', set, status )
           if (set) then
             idir = -1
           endif
         endif
       endif
       call cmd_itemset( 'wrap', wrap, status )
       call cmd_itemset( 'amp', amp, status )

C do FFT
       if (ir.gt.0) then
         call spec_get_data( id_from, ir, yr, ndata, status )
       endif
       if (im.gt.0) then
         call spec_get_data( id_from, im, yi, ndata, status )
       endif
       if (narr.eq.2) then
         do while (narr.lt.ndata)
           narr = narr*2
         enddo
       endif
C .. calaculate "padding" in array
       if (fill.gt.0) then
         nt = narr * (2**fill)
       else
         nt = narr
       endif
       do n=1,nt
         arr(n) = (0.0,0.0)
       enddo
C .. zero real and imaginary data if not defined
       if (ir.le.0) then
         do n=1,narr
           yr(n) = 0.0
         enddo
       endif
       if (im.le.0) then
         do n=1,narr
           yi(n) = 0.0
         enddo
       endif
C .. sort out wrapping of the data before transforming
       if (idir.eq.-1 .and. wrap) then
         ioff = ndata/2
         noff = nt - ndata
       else
         ioff = 0
         noff = 0
       endif
       do n=1,ndata/2
         arr(n) = cmplx(yr(n+ioff),yi(n+ioff))
       enddo
       do n=(1+ndata/2),ndata
         arr(n+noff) = cmplx(yr(n-ioff),yi(n-ioff))
       enddo
C .. do transform
       call complex_fft( arr, nt, idir, status )
       if (status.ne.0) goto 999
C .. sort out wrap after transform
       if (idir.eq.1 .and. wrap) then
         ioff = narr/2
         noff = nt - narr/2
       else
         ioff = 0
         noff = 0
       endif
       do n=1,narr/2
         yr(n) = real(arr(n+ioff))*sqrt(1.0+xfill)
         yi(n) = imag(arr(n+ioff))*sqrt(1.0+xfill)
       enddo
       do n=(1+narr/2),narr
         yr(n) = real(arr(n-noff))*sqrt(1.0+xfill)
         yi(n) = imag(arr(n-noff))*sqrt(1.0+xfill)
       enddo
       if (amp) then
         do n=1,narr
           ya(n) = sqrt(yr(n)**2 + yi(n)**2)
           yp(n) = atan2(yi(n),yr(n))
         enddo
         call spec_put_data(id_to,2,ya,narr,status)
         call spec_put_data(id_to,3,yp,narr,status)
       else
         call spec_put_data(id_to,2,yr,narr,status)
         call spec_put_data(id_to,3,yi,narr,status)
       endif
       del = 1.0
       if (same_axis) then
         call spec_get_data(id_from,xc,x,ndata,status)
         call spec_put_data(id_to,1,x,narr,status)
       else
         do n=1,narr
           x(n) = 0.0
         enddo
         del = 1.0
         if (xc.gt.0 .and. xc.le.ncols) then
           call spec_get_data(id_from,xc,x,ndata,status)
           call spec_put_data(id_to,4,x,narr,status)
           del = x(2) - x(1)
         endif
         if (xfftc.gt.0 .and. xfftc.le. ncols .and. .not.new_axis) then
           call spec_get_data(id_from,xfftc,x,ndata,status)
           call spec_put_data(id_to,1,x,narr,status)
         else
           do n=1,narr
             x(n) = float(n-1)*2.0*3.14159265/(float(narr)*del)
           enddo
           call spec_put_data(id_to,1,x,narr,status)
         endif
       endif

C add history item
       string = ' '
       if (idir.eq.1) then
         write(string,'(A)')
     *        'FFT: Forward'
       else
         write(string,'(A)')
     *        'FFT: Forward'
       endif
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_to, status )

       end






