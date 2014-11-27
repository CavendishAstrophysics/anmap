C BASELINE: remove a quadratic baseline from a data set
C
       implicit    NONE
C
C
C Multi column data are handled.  Selected columns have quadratic
C baselines fitted and removed.
C
C Parameters:
C    outfile     ==  output file name
C    infile      ==  input file name
C    cols        ==  list of columns to "log"
C    xc          ==  x-column to use for windowing etc.
C    title       ==  new title for output file
C    comment     ==  use defined comment text
C    window_n    ==  up to nine windows on the data
C    a0, a1, a2  ==  keys to specify a fit is required
C
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
       real*4     x(max_ndata), y(max_ndata), baseline
       integer    mask(max_ndata)
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   number of columns etc.
       integer    ncols, clist(max_ncols), xc
C   flag to indicator a parameter is set
       logical    set
C   windows and mask
       integer    nmask
       real*4     window(2,9)
C   counters
       integer    n, nc, nm

C Local variables used in NAG fit
       integer    npts, nvals
       real*4     fit_vals(3)
       integer    fit_type(3)
       integer    len_work
       parameter (len_work = 400 + 16*max_ndata)
       real*8     fsumsq, fit(3), work(len_work)
       integer    iwork(100), ifail
       common    /fitdat/ x, y, mask, fit_type, fit_vals, ndata


C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'baseline',
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
       call cmd_itemi( 'xc', 1, xc, status )
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

C setup type of fit
       do n=1,3
         fit_type(n) = 0
         fit_vals(n) = 0.0
       enddo
       call cmd_itemset( 'a0', set, status )
       if (set) fit_type(1) = 1
       call cmd_itemset( 'a1', set, status )
       if (set) fit_type(2) = 1
       call cmd_itemset( 'a2', set, status )
       if (set) fit_type(3) = 1
       if (status.ne.0) goto 999

C setup windows
       set = .true.
       nmask = 1
       do while (set .and. nmask.le.9 .and. status.eq.0 )
         write(string,'(''window'',I1)') nmask
         call cmd_itemset( string(1:7), set, status )
         if (set) then
           call cmd_itemr( string(1:7), 2, window(1,nmask), status )
         endif
         nmask = nmask + 1
       enddo
       call spec_get_data( id_from, xc, x, ndata, status )
       do n=1,ndata
         mask(n) = 1
         do nm=1,nmask
           if ((x(n).ge.window(1,nm)).and.(x(n).le.window(2,nm))) then
             mask(n) = 0
           endif
         enddo
       enddo

C preliminary setup for using NAG
       npts = 0
       do n=1,ndata
         if (mask(n).eq.1) then
           npts = npts + 1
         endif
       enddo
C loop for each column in the list and apply scaling/offset
       if (status.ne.0) goto 999
       do nc=1,ncols
         call spec_get_data( id_from, nc, y, ndata, status)
         if (status.ne.0) goto 999
         if (clist(nc).eq.1 .and. nc.ne.xc) then
C ... do fit using NAG
           ifail = 1
           nvals = 0
           do n = 1,3
             if (fit_type(n).eq.1) then
               nvals = nvals + 1
             endif
             fit(n) = 0.0D+0
           enddo
           if (nvals.gt.0) then
             call e04fdf(npts, nvals, fit, fsumsq,
     *                   iwork, 100, work, len_work, ifail)
             if (ifail.ne.0 .and. ifail.lt.5) then
               print *,'***(BASELINE) Ifail non-zero in NAG = ',ifail
               if (ifail.eq.1) goto 999
             end if
C .... copy fitted parameters
             nvals = 0
             do n=1,3
               if (fit_type(n).eq.1) then
                 nvals = nvals + 1
                 fit_vals(n) = fit(nvals)
               else
                 fit_vals(n) = 0.0
               endif
             enddo
C .... remove the baseline
             do n=1,ndata
               baseline = fit_vals(1) + x(n)*fit_vals(2) +
     *                    fit_vals(3)*(x(n)**2)
               y(n) = y(n) - baseline
             enddo
           endif
           call spec_put_data( id_to, nc, y, ndata, status)
         else
           call spec_put_data( id_to, nc, y, ndata, status)
         end if
       end do

C add history item
       string = ' '
       write(string,'(A)')
     *       'BASELINE: removed '
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_to, status )

       end
C
C routine used by nag : LSFUN1

       subroutine LSFUN1( npts, nvals, fit, f )
C
C Routine required by NAG -- see definition in NAG manual for details
C
       include '../include/spec_global.inc'

       integer    nvals, npts
       real*8     fit(nvals), f(npts), fit_v(10)

       integer    n, nn

       real*4     x(max_ndata), y(max_ndata), fit_vals(3)
       integer    mask(max_ndata), fit_type(3), ndata
       common    /fitdat/ x, y, mask, fit_type, fit_vals, ndata


C .. copy data values
       nn = 0
       do n=1,3
         if (fit_type(n).eq.1) then
           nn = nn + 1
           fit_v(n) = fit(nn)
         else
           fit_v(n) = 0.0
         endif
       enddo
       nn = 0
       do n=1,ndata
         if (mask(n).eq.1) then
           nn = nn + 1
           f(nn) = y(n) - fit_v(1) - fit_v(2)*x(n) -
     *             fit_v(3)*(x(n)**2)
         endif
       enddo
       end

