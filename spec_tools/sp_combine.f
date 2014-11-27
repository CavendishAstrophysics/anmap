C COMBINE: combine to spectra and output a new spectrum
C
       implicit    NONE
C
C The input spectra are combined according to the following rule
C
C   spec_out = (spec_1-zero1)*scale1 + (spec_2-zero2)*scale2
C
C Multi column data are handled.  The combination is only applied to
C columns with column number not equal to XC; the columns to be combined
C are handled by the parameter COLS which will contain a list of
C column numbers to combine with -1 == all columns and default -1.
C Columns not speified in the COLS list are copied to the output from
C infile1.
C
C Parameters:
C    outfile   ==  output file name
C    infile1   ==  input file name
C    scale1    ==  scale factor
C    zero1     ==  zero-level
C    infile2   ==  input file name
C    scale2    ==  scale factor
C    zero2     ==  zero-level
C    xc        ==  X-column
C    cols      ==  column list to combine
C    title     ==  new title for output file
C    comment   ==  use defined comment text
C
C Include information on standard array sizes etc.
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
C
C Local variables
C   file identifiers
       integer    id_to, id_from1, id_from2
C   scale factors etc.
       real*4     scale1, scale2, zero1, zero2
C   status return
       integer    status
C   number of data points
       integer    ndata
C   array to hold data values
       real*4     x(max_ndata), x1(max_ndata), x2(max_ndata)
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80, st1*80, st2*80
C   list of columns to operate on
       integer    clist(max_ncols), ncols
C   counters
       integer    n, nc
C   option code
       character*10  option
C Functions
       integer    chr_lenb
       logical    chr_cmatch

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'combine',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)
C find input file names and allocate files
       call cmd_items( 'infile1', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_from1, status)
       call cmd_items( 'infile2', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_from2, status)
C find output file name and allocate
       call cmd_items( 'outfile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'write', id_to, status)
C copy output descriptors
       call spec_hd_copy( id_from1, id_to, status )

C read control data
C .. standard information on column parameters
       call spec_getcols( id_from1, ncols,  clist, status )
C .. parameters used to combine spectra
       call cmd_itemr( 'scale1', 1, scale1, status )
       call cmd_itemr( 'zero1', 1, zero1, status )
       call cmd_itemr( 'scale2', 1, scale2, status )
       call cmd_itemr( 'zero2', 1, zero2, status )
       call cmd_items( 'option', option, status )
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

C loop for each column in the list and combine
       if (status.ne.0) goto 999
       do nc=1,ncols
         call spec_get_data( id_from1, nc, x1, ndata, status)
         call spec_get_data( id_from2, nc, x2, ndata, status)
         if (status.ne.0) goto 999
         if (clist(nc).le.0) then
C .. copy this column
           call spec_put_data( id_to, nc, x1, ndata, status)
         else
C .. combine columns
           if (chr_cmatch(option(1:chr_lenb(option)),
     *                    'divide')) then
             do n=1,ndata
               if (x2(n).ne.0.0) then
                 x(n) = (scale1*x1(n)) / (scale2*x2(n))
               else
                 x(n) = 0.0
               endif
             enddo

           elseif (chr_cmatch(option(1:chr_lenb(option)),
     *                        'multiply')) then
             do n=1,ndata
               x(n) = scale1* (x1(n)-zero1) * scale2 * (x2(n) -zero2)
             enddo

           elseif (chr_cmatch(option(1:chr_lenb(option)),
     *                        'alpha')) then
             do n=1,ndata
               if ( (x1(n).gt.0.0) .and. (x2(n).gt.0.0) ) then
                 x(n) = log(x1(n)/x2(n))/log(scale2/scale1)
               else
                 x(n) = 0.0
               endif
             enddo

           elseif (chr_cmatch(option(1:chr_lenb(option)),
     *                        'visibility')) then
             do n=1,ndata
               if ((x1(n)+x2(n)).ne.0.0) then
                 x(n) = (x1(n) - x2(n))/(x1(n) + x2(n))
               else
                 x(n) = 0.0
               endif
             enddo

           else
             do n=1,ndata
               x(n) = (x1(n)-zero1)*scale1 + (x2(n)-zero2)*scale2
             end do
           endif
           call spec_put_data( id_to, nc, x, ndata, status)
         end if
       end do

C add history item
       string = ' '
       write(string,'(A,1PE10.2,1PE10.2,A,1PE10.2,1PE10.2)')
     *       'COMBINE: scale1/2 =',scale1,scale2,
     *               '  zero1/2 =',zero1,zero2
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )
       string = ' '
       call spec_hd_enq( id_from1, 'name', st1, status)
       call spec_hd_enq( id_from2, 'name', st2, status)
       write(string,'(A,A,A,A)')
     *       'COMBINE: name1/2 = ',st1(1:chr_lenb(st1)),' ',
     *                             st2(1:chr_lenb(st2))
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )

C deallocate files
999    continue
       call spec_deallocate( id_from1, status )
       call spec_deallocate( id_from2, status )
       call spec_deallocate( id_to, status )

       end




