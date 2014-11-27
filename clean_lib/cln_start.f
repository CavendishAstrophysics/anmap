C
C
*+ cln_start

       subroutine cln_start(map_array,file_r,file_c,status)
C      ----------------------------------------------------
C
C Start the clean process; create files, spool process or run
C
C Given:
C   map work space
       real*4                map_array(*)
C
C Returned:
C   file names of residual and ccmp file
       character*(*)         file_r, file_c
C   error status
       integer               status
C
C To start the clean process from within the interactive sequence.
C The CCSL file is opened and the current control parameters written.
C The .res file is initialised. A CLEAN process is spooled if the
C ONLINE flag is set to false, otherwise the process is run interactively
C at the terminal.
C
*-
       include '../include/clean_record.inc'
       include '../include/clean_sys_pars.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
C  character strings for file names
       character*60    directory, residual_map, string
C  pointers
       integer         ip_map
C  unit numbers
       integer         iresi, icc
C  length of character strings
       integer         len_r, len_g
C  local status word
       integer         istat

C check status on entry
       if (status.ne.0) return

C create residual map file
       call redt_load(map_cat,status)
       len_g = chr_lenb(generic_name)
       call mapcat_enqdefdir( directory, status )
       call io_makfil( directory(1:chr_lenb(directory)),
     *              generic_name(1:chr_lenb(generic_name)),
     *              'resi', residual_map, len_r       )
       istat = 0
       call io_namfil( residual_map(1:len_r), string, 0, istat )
       if (istat.ne.0) then
         istat = 0
         call map_alloc_in( map_cat, 'DIRECT',
     *                      map_array, ip_map, status )
         call map_end_alloc( map_cat, map_array, status )
         call stxrdt( 1, 1, status )
         call stxrec( clean_text_header, clean_record, status )
         call opemap( iresi, residual_map(1:len_r), 'WRITE',
     *                0, status )
         call wrredt( iresi, 0, status )
         call wrmap( iresi, map_array(ip_map), status )
         close (iresi)
       end if

C write out clean-components control data
       call opemap( icc, map_name(1:chr_lenb(map_name)),
     *              'UPDATE', 0, status )
       call rdredt( icc, 0, status )
       call stxrec( clean_text_header, clean_record, status )
       call wrredt( icc, 0, status )
       close (icc)

C return file name
       file_c = map_name(1:chr_lenb(map_name))
       file_r = residual_map(1:chr_lenb(residual_map))

C error reporting and releasing allocated store
       call cmd_err(status,'clean-start','Clean failed')
       end
