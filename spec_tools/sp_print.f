C PRINT: print selected columns of data
C
       implicit    NONE
C
C
C Multi column data are handled.  The selected columns are printed
C to the standard output.
C
C Parameters:
C    infile      ==  input file name
C    cols        ==  list of columns to "log"
C
C Include information on standard array sizes etc.
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'

       include '/mrao/include/chrlib_functions.inc'
C
C Local variables
C   file identifiers
       integer    id_from
C   status return
       integer    status
C   number of data points
       integer    ndata
C   array to hold data values
       real*4     rd(max_ncols), row(max_ncols)
C   array to hold range in X to display
       real*4     xr(2)
       integer    xc
C   file and directory name(s) and strings
       character  file*100, dir*100, title*80
C   format control for output
       character  form*40
C   number of columns etc.
       integer    ncols, clist(max_ncols)
C   output unit
       integer    iout
C   counters
       integer    n, nc, ic
C   logical to test setting of header parameters
       logical    set

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'print',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)
C find input file names and allocate files
       call cmd_items( 'infile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_from, status)

C read control data
C .. find list of columns to print
       call spec_getcols( id_from, ncols, clist, status )
C .. find title if any
       call cmd_items( 'title', title, status )
       if (status.ne.0) goto 999
C .. find format
       call cmd_itemset( 'format', set, status )
       if (set) then
         call cmd_items( 'format', form, status )
       else
         form = '(1X,6(1PE12.3))'
       endif
C loop for each row and output the requested columns
       call io_enqout(iout)
       if (chr_lenb(title).gt.0) then
         write(iout,'(1X,A)') title(1:chr_lenb(title))
       endif
       call spec_hd_enqi( id_from, 'ndata', ndata, status )
       call cmd_itemset( 'xrange', set, status )
       if (set) then
         call cmd_itemr( 'xrange', 2, xr, status )
       endif
       do n=1,ndata
         call spec_get_row( id_from, n, rd, ncols, status )
         if (status.ne.0) goto 999
         nc = 0
         do ic=1,ncols
           if (clist(ic).ge.0) then
             nc = nc + 1
             row(nc) = rd(ic)
             if (clist(ic).eq.0) xc = nc
           endif
         enddo
         if (set) then
           if (row(xc).ge.xr(1).and.row(xc).le.xr(2)) then
             write(iout,fmt=form(1:chr_lenb(form)))(row(ic),ic=1,nc)
           endif
         else
           write(iout,fmt=form(1:chr_lenb(form)))(row(ic),ic=1,nc)
         endif
       enddo

C deallocate files
999    continue
       call spec_deallocate( id_from, status )

       end
