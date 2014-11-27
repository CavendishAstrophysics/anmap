C HEADER: Print the header onthe standard output
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
C Functions
       integer    chr_lenb

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'header',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)

C find input file name and allocate file
       call cmd_items( 'infile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id, status)
C print header on unit 6
       call spec_hd_print( id, 6, file, status )

C deallocate spectrum
       call spec_deallocate( id, status )

       end
