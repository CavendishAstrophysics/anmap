C GETHD : print the value of a single header item to the standard output
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
C   file and directory name(s) and strings used in enquiries
       character  file*100, dir*100, parameter*80, string*80

       include '/mrao/include/chrlib_functions.inc'

C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'gethd',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)

C find input file name and allocate file
       call cmd_items( 'infile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id, status)

C find item to print
       call cmd_items( 'parameter', parameter, status )
C enquire isem
       call spec_hd_enq( id, parameter(1:chr_lenb(parameter)),
     *                       string, status )
       if (status.eq.0) then
          print *,string(1:chr_lenb(string))
       else
          print *,'NOT_FOUND'
       endif

C deallocate spectrum
       call spec_deallocate( id, status )

       end
