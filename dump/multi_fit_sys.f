C
C
*+ multi_fit_sys

       subroutine multi_fit_sys( status )
C      ----------------------------------
C
C Setup sub-system for fitting of many maps
C
C Returned:
C    error status
       integer       status
C
C A command line driven sub-system for setting up options to add
C large numbers of maps together with varying weights.
C
C-

       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C Local variables
C ---------------
C  dummy real*4 argument to map_end_alloc routine
       real*4             dummy_argument
C  command line, length and initial check
       character*120      command_line
       integer            len_com, iout
       logical            exit_on_completion
C  command line interpretation
       integer            number_commands, icom
       parameter         (number_commands = 10)
       character*80       liscom(number_commands)
C  record containing list of maps; this will be a record in the extra
C  redtape slot of a standard format map.  The first 16 bytes of the
C  record containing list of maps; this will be a record in the extra
C  redtape slot of a standard format map.  The first 16 bytes of the
C  record are therefore reserved words.
       integer            map_list(128)
C    number of maps in list
       integer            nmaps
       equivalence       (map_list(5), nmaps)
C    start record for maps in list
       integer            start
       equivalence       (map_list(6), start)
C    length of each record in list
       integer            len_record
       equivalence       (map_list(7), len_record)
C    type of model fit required
       integer            model
       equivalence       (map_list(8),model)
C    number of constants
       integer            nc
       equivalence       (map_list(9), nc)
C    constants
       real*4             c_data(5)
       equivalence       (map_list(10), c_data(1))
C    number of output maps
       integer            num_out
       equivalence       (map_list(15), num_out)
C  define entry for each map
       integer            map_record(4)
       integer            map_entry
       equivalence       (map_record(1), map_entry)
       integer            map_directory
       equivalence       (map_record(2), map_directory)
       real*4             map_xval
       equivalence       (map_record(3), map_xval)
       real*4             map_gate
       equivalence       (map_record(4), map_gate)
C  counters and pointers
       integer            imap, n, nn, i, iwarn, first_map
C  maximum number of maps to be added to list
       integer            max_maps
C  general string and filename
       character          string*256, source*20, program*8
       character          filename*(iolen_file)
C  maximum number of model types defined
       integer            max_model
       parameter         (max_model = 5)
C
       common /map_anal_local_multifit/ map_list
C
C define commands
       data liscom(1) /
     *  'clear-map-list ............... initialise map list'
     *                /
       data liscom(2) /
     *  'add-map-to-list .............. add a map to the list'
     *                /
       data liscom(3) /
     *  'delete-map-from-list ......... delete a map from the list'
     *                /
       data liscom(4) /
     *  'list-maps .................... list the maps in the list'
     *                /
       data liscom(5) /
     *  'set-fit-type ................. set model to fit data'
     *                /
       data liscom(6) /
     *  'set-constant ................. set one of th 5 constants'
     *                /
       data liscom(7) /
     *  'make ......................... run multi-fit offline'
     *                /
       data liscom(8) /
     *  'go ........................... run multi-fit interactively'
     *                /
       data liscom(9) /
     *  'help ......................... help on multi-fit options'
     *                /


C check status on entry
       if (status.ne.0) return
       nc = 5
       num_out = 2
       start = 18
       len_record = 4
       if (model.le.0 .or. model.gt.max_model) then
         model = 1
         call io_wrout('.. model type set to 1')
       end if
       max_maps = (128-start+1)/len_record

C interogate command line and determine whether to return on completion
C of the current command.
       call io_enqcli( command_line, len_com )
       exit_on_completion = len_com.ne.0

100    continue
       status = 0
       call cmd_scncmd( .false., status )
       call cmd_getcmd( 'Multi-Fit> ',liscom,number_commands,
     *                  icom,status)
       call cmd_scncmd( .true., status )
       if (status.ne.0) then
         call cmd_err(status,'Multi-Fit',' ')
         goto 100
       end if
       if (icom.le.0) return

C decode command
       if (chr_cmatch('clear-map-list',liscom(icom))) then
         nmaps = 0
         do n=start,128
           map_list(n) = 0
         end do

       else if (chr_cmatch('add-map-to-list',liscom(icom))) then
         string = ' '
         if (nmaps.ge.max_maps) then
           call io_wrout('*** Maximum number of maps reached in list')
           goto 100
         end if
         write(string,'(A,I2,A)')
     *         'Map-entry for map number ',nmaps+1,' : '
         call mapcat_getmap(string(1:chr_lenb(string)),
     *                      'Default-Map','READ',
     *                      imap,status)
         if (status.eq.IOLIB_OK) then
           iwarn = 0
           if (nmaps.eq.0) then
             first_map = imap
           else
             iwarn = 2
             first_map = map_list( start )
             call redt_comp(first_map,imap,iwarn,status)
           end if
           if (iwarn.le.5) then
             call io_getr('X/t-value : ','1.0',map_xval,status)
             call io_getr('Gate : ','0.0',map_gate,status)
             if (status.eq.IOLIB_OK) then
               map_entry = imap
               nmaps = nmaps + 1
               do i=1,len_record
                 map_list( i + start-1 + (nmaps-1)*len_record ) =
     *               map_record(i)
               end do
             end if
           else
             call io_wrout('*** Map not added to list')
           end if
         end if
         call cmd_err(status,'multi-fit',' ')

       else if (chr_cmatch('delete-map-from-list',liscom(icom))) then
         n = 1
         do while ( n.le.nmaps .and. status.eq.IOLIB_OK)
           do i=1,len_record
             map_record(i) = map_list( i + start-1 + (n-1)*len_record )
           end do
           call mapcat_enqsr(map_entry,source,program,status)
           string = ' '
           write(string,'(A,A,A2,A,A2,I3,A3)')
     *          'Remove map ',source(1:chr_lenb(source)),
     *          ' (',program(1:chr_lenb(program)),') ',map_entry,' ? '
           if (io_yesno(string(1:chr_lenb(string)),'no',status)) then
             if (status.eq.IOLIB_OK) then
               do nn=n+1,nmaps
                 do i=1,len_record
                   map_list( i + start-1 + (n-1)*len_record ) =
     *                 map_list( i + start-1 + (nn-1)*len_record )
                 end do
               end do
               nmaps = nmaps - 1
             end if
           end if
           n = n + 1
         end do
         if (status.eq.USR_BREAK) status = IOLIB_OK
         call cmd_err(status,'do_multi_fit',' ')

       else if (chr_cmatch('list-maps',liscom(icom))) then
         call io_enqout(iout)
         write(iout,'(A,I2,A,5(1PE10.2))')
     *         ' Model-type =',model,' constants =',
     *         (c_data(n), n=1,5)
         n = 1
         do while ( n.le.nmaps .and. status.eq.IOLIB_OK)
           do i=1,len_record
             map_record(i) = map_list( i + start-1 + (n-1)*len_record )
           end do
           call mapcat_enqsr(map_entry,source,program,status)
           if (status.eq.IOLIB_OK) then
              write(iout,'(A,A,A2,A,A2,I3,A,1PE12.2,A,1PE12.2)' )
     *          ' Map: ',source(1:chr_lenb(source)),
     *          ' (',program(1:chr_lenb(program)),') ',map_entry,
     *          ' Xvalue =',map_xval,'  Gate =',map_gate
           end if
           n = n + 1
         end do
         if (status.eq.USR_BREAK) status = IOLIB_OK
         call cmd_err(status,'multi-fit',' ')

       else if (chr_cmatch('set-fit-type',liscom(icom)) ) then
         call io_geti('Model-fit-type (1-3) : ','*',model,status)
         if (model.le.0 .or. model.gt.max_model) then
           call io_wrout('***Illegal model type: out of range')
           model = 1
         end if

       else if (chr_cmatch('set-constant',liscom(icom)) ) then
         call io_geti('Constant-to-set (1-5) : ','*',n,status)
         if (n.gt.0 .and. n.le.5) then
           call io_getr('Constant-value : ','*',c_data(n),status)
         else
           call io_wrout('***No such constant')
         end if

       else if (chr_cmatch('go',liscom(icom)) .or.
     *          chr_cmatch('make',liscom(icom))  ) then
          call redt_load(first_map,status)
          call redt_setxrdt( status )
          call map_alloc_new(0,0,map_list(16),filename,status)
          call stxrec('MULTIFIT',map_list,status)
          call sttype(0.0,-1,'Rho',' ',status)
          call map_end_alloc(map_list(16),dummy_argument,status)
          call map_alloc_new(0,0,map_list(17),filename,status)
          call stxrec('MULTIFIT',map_list,status)
          call sttype(0.0,-1,'Carr',' ',status)
          call map_end_alloc(map_list(17),dummy_argument,status)
          string = multifit_command//' '//filename
          if (chr_cmatch('make',liscom(icom))) then
            call anm_exec(string(1:chr_lenb(string)),
     *           'MAP-ANALYSIS','batch','exact',
     *           status)
          else
            call anm_exec(string(1:chr_lenb(string)),
     *           'MAP-ANALYSIS','background','exact',
     *           status)
          end if

       else if (chr_cmatch('help',liscom(icom))) then
         call hlp_setfil( multifit_helpfile, status )
         call hlp_system( ' ', ' ', .true., .true., status)
         call cmd_err(status,'HELP (Multi-Fit)',' ')

       end if

       if (.not.exit_on_completion) goto 100

       end

