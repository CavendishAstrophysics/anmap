C
C
*+ cln_clear

       subroutine cln_clear(status)
C      ----------------------------
C
C Clear all traces of previous clean runs except the CLEAN map
C
C Returned:
C   error status
       integer               status
C
C The residual map and clean component map are deleted and items in the
C header file restored.  A new Clean components map is then created to
C ensure consistency.
*-
       include '../include/clean_record.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
C  character strings for file names
       character*60    directory, residual_map, string
C  length of character strings
       integer         len_r, len_g, len_m
C  CLEAN components map unit number
       integer         icc
       logical         exist
C   UV-range
       integer         iuv(4)
C   dummy values for the clean components
       real*4          cc_values(1)
       integer*2       cc_posns(2,1)
C  local status word
       integer         istat
C  debug routine and parameters
       logical         cmd_dblev
       integer         iprint

C check status on entry
       if (status.ne.0) return

C delete residual map file
       call redt_load(map_cat,status)
       len_g = chr_lenb(generic_name)
       call mapcat_enqdefdir( directory, status )
       call io_makfil( directory(1:chr_lenb(directory)),
     *              generic_name(1:chr_lenb(generic_name)),
     *              'resi', residual_map, len_r       )
       istat = 0
       call io_namfil( residual_map(1:len_r), string, 0, istat )
       if (istat.eq.0) then
         call io_delfil( residual_map(1:len_r), iprint, status )
       end if

C reset header information
       number_cc = 0
       times_cleaned = 0
       number_it_done = 0
       max_on_resid = 0.0
       how_ended = 0

C check for clean components map
       len_g = chr_lenb(generic_name)
       call mapcat_enqdefdir( string, status )
       call io_makfil( string(1:chr_lenb(string)),
     *                 generic_name(1:len_g),
     *                 'ccmp', map_name, len_m )
       len_m = chr_lenb(map_name)
       inquire (file = map_name(1:len_m), exist=exist)
       iprint = 0

C delete existing file and re-create
       if (exist) then
         call io_delfil( map_name(1:len_m), iprint, status )
         call ldredt( redt_map, status )
         call iuv_load( iuv, status )
         map_defined = .true.
         call cln_setdef( .true., status )
         call stredt( iuv, -3, status )
         number_cc = 0
         times_cleaned = 0
         number_it_done = 0
         max_on_resid = 0.0
         how_ended = 0
         call stxrdt( 1, 1, status )
         call stxrec( clean_text_header, clean_record, status )
         call opemap( icc, map_name(1:len_m), 'WRITE', iprint, status )
         call wrredt( icc, 0, status )
         call wrmapc( icc, cc_posns, cc_values, number_cc, status )
         close (icc)
       end if

C error reporting
       call cmd_err(status,'cln_clear',' ')
       end
