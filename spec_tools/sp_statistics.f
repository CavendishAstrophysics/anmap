C STATISTICS: determine statistics for a spectrum
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
C   option string
       character  option*80
C   data
       real*4     x(max_ndata)
C   statistics
       real*4     x_min, x_max, x_sum, x_average, x_del
C   number of data/columns and counters
       integer    ndata, ncols, n, nc, ic
C Functions
       integer    chr_lenb
       logical    chr_cmatch

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'statistics',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)

C find input file name and allocate file
       call cmd_items( 'infile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id, status)

C find option string if present
       call cmd_items( 'option', option, status )
       call cmd_itemi( 'col', 1, ic, status )

C enquire number of columns and loop for each
       call spec_hd_enqi( id, 'ncols', ncols, status )
       do nc=1,ncols
C .. read data
         call spec_get_data( id, nc, x, ndata, status)
         x_max = -1.0E+30
         x_min =  1.0E+30
         x_sum = 0.0
         do n=1,ndata
           x_max = max(x_max,x(n))
           x_min = min(x_min,x(n))
           x_sum = x_sum + x(n)
         end do
         x_del = (x_max - x_min) / float( ndata - 1 )
         x_average = x_sum / float( ndata )
         if (chr_lenb(option).eq.0) then
           print 10,nc,x_min,x_max,x_sum,x_average
   10      format(1X,'.. column ',i1,' min/max = ',1P2E12.3,
     *             '  sum = ',1PE12.3,'  average = ',1PE12.3 )
         else if (nc.eq.ic) then
           if (chr_cmatch(option(1:chr_lenb(option)),'DEL')) then
             print *,x_del
           elseif (chr_cmatch(option(1:chr_lenb(option)),'SUM')) then
             print *,x_sum
           elseif (chr_cmatch(option(1:chr_lenb(option)),'MAX')) then
             print *,x_max
           elseif (chr_cmatch(option(1:chr_lenb(option)),'MIN')) then
             print *,x_min
           elseif (chr_cmatch(option(1:chr_lenb(option)),'AVG')) then
             print *,x_average
           end if
         end if
       end do

C deallocate spectrum
       call spec_deallocate( id, status )

       end
