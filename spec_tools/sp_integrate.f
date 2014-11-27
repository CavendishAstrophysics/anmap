C INTEGRATE: integrate a spectrum file between limits
C
       implicit    NONE
C
C Include information on standard array sizes etc.
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
C
C Local variables
C   file identifiers
       integer    id
C   status return
       integer    status
C   file and directory name(s)
       character  file*100, dir*100
C   data
       real*4     x(max_ndata), y(max_ndata)
C   statistics
       real*4     x_min, x_max, y_sum, x_del
C   number of data/columns and counters
       integer    ndata, n, xc, yc
C Functions
       integer    chr_lenb

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'integrate',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)

C find input file name and allocate file
       call cmd_items( 'infile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id, status)

C find option string if present
       call cmd_itemi( 'xc', 1, xc, status )
       call cmd_itemi( 'yc', 1, yc, status )
       call cmd_itemr( 'xmin', 1, x_min, status )
       call cmd_itemr( 'xmax', 1, x_max, status )

C .. read data
       call spec_get_data( id, xc, x, ndata, status)
       call spec_get_data( id, yc, y, ndata, status)
       y_sum = 0.0
C .. do the integral
       do n=1,ndata
         if (x(n).ge.x_min .and. x(n).le.x_max) then
           if (n.eq.1) then
             x_del = 0.5 * x(n+1) - x_min
           else if (n.eq.ndata) then
             x_del = x_max - 0.5 * x(n-1)
           else
             x_del = 0.5 * ( (x(n+1)+x(n)) - (x(n)+x(n-1)) )
           end if
           y_sum = y_sum + x_del * y(n)
         end if
       end do

C .. report result
       print *,y_sum

C deallocate spectrum
       call spec_deallocate( id, status )

       end
