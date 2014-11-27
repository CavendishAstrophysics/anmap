C
C
*+ clean_sys

       subroutine clean_sys(i_command,map_array,status)
C      ------------------------------------------------
C
C Run the clean sub system
C
C Given:
C   map data work space
       real*4      map_array(*)
C
C Returned:
C   command on exit from CLEAN_SYS
       integer     i_command
C   error return code
       integer     status
C
C The clean sub system provides an interactive environment to setup
C clean runs which may then be spooled or run online. Checking of all
C values before the CLEAN is run is then possible, thus minimizing
C annoying batch crashes, but by running detached the terminal is
C not tied to the task.
C
*-
       include '/mrao/anmap/include/clean_record.inc'
       include '/mrao/anmap/include/anmap_sys_pars.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
C
C Define commands:
       integer         number_commands
       parameter      (number_commands = 19)
       character*60    liscom(number_commands)
       integer         len_cli, i_com_done, icomm

C command line and parameters used to check command input
       character*80    parameters
       logical         needs_map(number_commands), exit_at_end,
     *                 large_map
       integer         i, iout

C local variables
C    fraction, flux-limits, prussian hat (saved values)
       real*4          o_fract, o_fllim, o_prhat
C    beam size (saved values)
       real*4          o_beam_size_u, o_beam_size_v
C    iteration limit (saved values)
       integer         o_itlim
C    loop counter
       integer         ns

       common /save_clean_parameters/ large_map

C check for exit on completion of command
       call io_enqcli(parameters,len_cli)
       exit_at_end = len_cli.ne.0
       i_com_done = 0

c define whether commands need a map to have been defines
       do i=1,number_commands
         needs_map(i) = .false.
       end do

C define commands and optionally the need for a map to have been defined
       liscom(1) =
     *  'set-map .................. define map and name'
       liscom(2) =
     *  'reset-defaults ........... reset all default values'
       liscom(3) =
     *  'set-clean-window ......... set the window to clean'
       needs_map(3) = .true.
       liscom(4) =
     *  'set-beam-window .......... set the window on the beam'
       needs_map(4) = .true.
       liscom(5) =
     *  'set-search-windows ....... set search windows'
       needs_map(5) = .true.
       liscom(6) =
     *  'set-fraction ............. set fraction for removal'
       liscom(7) =
     *  'set-flux-limit ........... set limiting flux density'
       liscom(8) =
     *  'set-iteration-limit ...... set iteration limit'
       liscom(9) =
     *  'set-clean-beam ........... set size for gaussian beam'
       needs_map(9) = .true.
       liscom(10)=
     *  'set-position-angle ....... set position angle for beam'
       needs_map(10) = .true.
       liscom(11)=
     *  'prussian-hat ............. select/deselect PH clean'
       liscom(12)=
     *  'stop-at-zero ............. select option for autocal'
       liscom(13)=
     *  'not-box .................. set not box for search'
       needs_map(13) = .true.
       liscom(14)=
     *  'truncated-beam ........... truncate beam at first zero'
       liscom(15)=
     *  'make ..................... run CLEAN in batch'
       needs_map(15) = .true.
       liscom(16)=
     *  'go-clean ................. run CLEAN on line'
       needs_map(16) = .true.
       liscom(17)=
     *  'display-options .......... display options and values'
       liscom(18)=
     *  'clear-clean-run .......... deletes clean run'
       needs_map(19) = .true.
       liscom(19)=
     *  'sorting-clean ............ sort-clean option'

C enter command mode
1000   continue

C check for exit on completion of command
       if (exit_at_end .and. i_com_done.ne.0) then
         i_command = 100
         return
       end if

c reset error status flag
       status =0
       call cmd_getcmd(prompt_clean,liscom,
     *                  number_commands,icomm,status)

c check error flag
       if (status.ne.0) then
         call cmd_err(status,'CLEAN-SYSTEM','I/O error')
         goto 1000
       else
         i_com_done = i_com_done + 1
       end if

C check accessibility of command if map has not been defined
       if (icomm.gt.0) then

         if (needs_map(icomm) .and. .not.map_defined) then
           call cmd_wrerr('CLEAN',
     >     'Command not available before specifying map: use set-map')
           goto 1000
         end if

       else

C return to calling level on EXIT or call to basic command
         i_command = icomm
         return

       end if

C decode command
       goto (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
     *       icomm

C jump to required command and then return
1      call cln_setmap(large_map,status)
       goto 1000

2      call cln_setdef(.true.,status)
       goto 1000

3      call ldredt(redt_map,status)
       call plot_getuv('Clean-Window : ','*',clean_window,status)
       call cmd_err(status,'set-clean-window','Invalid clean window')
       goto 1000

4      call ldredt(redt_beam,status)
       call plot_getuv('Beam-Window : ','*',beam_window,status)
       call cmd_err(status,'set-beam-window','Invalid beam window')
       goto 1000

5      call ldredt(redt_map,status)
       call io_geti('Number of search windows : ','*',number_search,
     *            status)
       if (number_search.gt.9 .or. number_search.lt.1) then
         call cmd_wrerr('set-search-windows',
     *                   'Invalid number of search windows')
         number_search = 1
         goto 5
       end if
       if (status.eq.0) then
         do ns = 1,number_search
           call plot_getuv('Search-window : ','*',
     *                     search_window(1,ns),status)
         end do
       end if
       call cmd_err(status,'set-search-window',' ')
       goto 1000

6      o_fract = fract
       call io_getr('Fraction for removal : ','*',fract,status)
       call cmd_err(status,'set-fraction',' ')
       if (status.eq.0) then
C .. check range of FRACT
         if (fract.lt.0.0 .or. fract.gt.1.0) then
           call cmd_wrerr('SET-FRACTION',
     *                       'Fraction out of range 0->1')
           fract = o_fract
         end if
       end if
       goto 1000

7      o_fllim = fllim
       call io_getr('Limiting flux : ','*',fllim,status)
       call cmd_err(status,'set-flux-limit',' ')
       if (status.eq.0) then
C .. check range of FLLIM
         if (fllim.lt.0.0) then
           call cmd_wrerr('SET-FLUX-LIMIT','Limiting flux <0.0')
           fllim = o_fllim
         end if
       end if
       goto 1000

8      o_itlim = itlim
       call io_geti('Iteration limit : ','*',itlim,status)
       call cmd_err(status,'set-iteration-limit',' ')
       if (status.eq.0) then
C .. check range of ITLIM
         if (itlim.lt.0 .or. itlim.gt.max_cc_iter) then
           call cmd_wrerr('SET-ITERATION-LIMIT','Limit < 1 or >max')
           itlim = o_itlim
         end if
       end if
       goto 1000

9      call ldredt(redt_map,status)
       o_beam_size_u = beam_size_u
       o_beam_size_v = beam_size_v
       call io_getr('Beam-size u (arcsec) : ','*',beam_size_u,status)
       if (beam_size_u.lt.0.0) then
         call cmd_wrerr('SET-CLEAN-BEAM',
     *                     'Invalid size for beam <0.0')
         beam_size_u = o_beam_size_u
       else
         beam_size_v = beam_size_u
         beam_size_v = beam_size_v/sin(decmap)
         call io_getr('Beam-size v (arcsec) : ','*',beam_size_v,status)
         if (beam_size_v.lt.0.0) then
           call cmd_wrerr('SET-CLEAN-BEAM',
     *                     'Invalid size for beam <0.0')
           beam_size_v = o_beam_size_v
         end if
       end if
       call cmd_err(status,'set-clean-beam',' ')
       goto 1000

10     call io_getr('Position angle for clean-beam : ','*',
     *           beam_pa,status)
       if (beam_pa.lt.0.0 .or. beam_pa.gt.360.0) then
         call cmd_wrerr('SET-POSITION-ANGLE',
     *                   'Invalid PA <0.0 or >360.0')
         beam_pa = 0.0
       end if
       call cmd_err(status,'set-position-angle',' ')
       goto 1000

11     lprhat = io_onoff('Prussian hat on/off : ','off',status)
       if (lprhat) then
         o_prhat = prhat
         call io_getr('Hat-size : ','*',prhat,status)
         if ((prhat.lt.0.0 .or. prhat.gt.1.0) .and. status.eq.0) then
           call cmd_wrerr('PRUSSIAN-HAT',
     *               'Invalid hat size <0.0 or >1.0')
           prhat = o_prhat
         end if
       end if
       call cmd_err(status,'prussian-hat',' ')
       goto 1000

12     lstop0 = io_onoff('Stop at first zer-cc (on/off) : ',
     *                   'off',status)
       call cmd_err(status,'stop-at-zero',' ')
       goto 1000

13     lnotbx = io_onoff('Not-box (on/off) : ','off',status)
       if (lnotbx .and. status.eq.0) then
         call ldredt(redt_map,status)
         call plot_getuv('Not-box : ','*',box_window,status)
         call cmd_err(status,'NOT-BOX','Invalid not-box window')
         if (status.ne.0) lnotbx = .false.
       end if
       call cmd_err(status,'not-box',' ')
       goto 1000

14     ltrbm = io_onoff('Truncated beam on/off : ','off',status)
       call cmd_err(status,'truncated-beam',' ')
       goto 1000

15     call cln_start(map_array,.false.,status)
       goto 1000

16     call cln_start(map_array,.true.,status)
       goto 1000

17     call io_enqout(iout)
       call cln_display(iout,status)
       goto 1000

18     call cln_clear(status)
       goto 1000

19     lsort = io_onoff('Sort-mode on/off : ','off',status)
       if (lsort .and. status.eq.0) then
         call io_geti('Depth for sort (<500) : ',
     *                '100',sort_depth,status)
         if (sort_depth.gt.500) then
           call cmd_wrerr('SORT-MODE','Depth must be <=500')
           call cmd_wrerr('SORT-MODE','Sort-mode disabled')
           lsort = .false.
           goto 19
         end if
         sort_inner = sort_depth
         call io_geti('Iterations between sort : ','*',
     *             sort_inner,status)
         call io_getr('Gate for inclusion in sort : ','0.0',
     *             sort_gate,status)
       end if
       call cmd_err(status,'sort-option',' ')
       goto 1000

       end
C
C
*$ Clean-System Support Routines
*  -----------------------------

*+ cln_setmap

       subroutine cln_setmap(large_map,status)
C      ---------------------------------------
C
C Read in a map from stack or disc.
C
C Returned:
C    flag set if CLEAN is defined as a LARGE clean job
       logical     large_map
C    error return
       integer     status
C
C Prompt for map and beam and initialise the clean components map if
C required.
*-
       include '/mrao/anmap/include/clean_record.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/maplib_redtape.inc'

C local variables
C   general string
       character  string*60, temp*10
C   lengths of character strings
       integer    len_g, len_m
C   UV-range
       integer    iuv(4)
C   unit number for CC file
       integer    icc
C   print control indicator
       integer    iprint
C   dummy values for the clean components
       real*4     cc_values(1), itemp(10)
       integer*2  cc_posns(2,1)
C   logical variable to test for existence of CC-map
       logical    exist, cmd_dblev

C test status on entry
       if (status.ne.0) return

C prompt for map
       large_map = .false.
       call mapcat_getmap('Map-to-CLEAN : ','Default-Map','READ',
     *                    map_cat,status)
       call redt_load(map_cat,status)
       large_map = large_map .or. ixmax*iymax.gt.256*256
       call dpredt(redt_map,status)
       call mapcat_enqsr(map_cat,string,temp,status)
       len_g = chr_ilstc(string,'-') - 1
       generic_name = string(1:len_g)
       if (cmd_dblev(1)) then
         print *,'.. Generic_name = ',
     *           generic_name(1:chr_lenb(generic_name))
       end if
       if (status.ne.0) goto 999

C prompt for beam
       call mapcat_getmap('Beam : ',
     *                    generic_name(1:len_g)//'-BEAM','READ',
     *                    beam_cat,status)
       call redt_load(beam_cat,status)
       call dpredt(redt_beam,status)
       large_map = large_map .or. ixmax*iymax.gt.256*256
       call mapcat_enqrt(beam_cat,beam_name,itemp,status)
       call ldredt(redt_map,status)
       if (cmd_dblev(2)) then
         print *,'.. Beam redtape loaded'
       end if
       if (status.ne.0) goto 999

C check for clean components map
       call mapcat_enqdefdir( string, status )
       call io_makfil( string(1:chr_lenb(string)),
     *                 generic_name(1:len_g),
     *                 'ccmp', map_name, len_m )
       len_m = chr_lenb(map_name)
       inquire (file = map_name(1:len_m), exist=exist)
       iprint = 0
       if (cmd_dblev(1)) then
         print *,'.. CCMP_map = ',map_name(1:len_m)
         iprint = 1
       end if
       if (exist) then
C .. read the source-list file redtape
         if (cmd_dblev(2)) then
           print *,'.. CCMP map exists -- reading redtape'
         end if
         status = 0
         call opemap( icc, map_name(1:len_m), 'READ', 0, status )
         call rdredt( icc, 0, status )
         call enxrec( clean_text_header, clean_record, status )
         close (icc)
         if (cmd_dblev(2)) then
           print *,'.. CCMP map redtape and extra redtape read '
         end if

       else
C .. create a new source list map
         if (cmd_dblev(2)) then
           print *,'.. CCMP map being created'
           iprint = 3
         end if
         call ldredt( redt_map, status )
         call iuv_load( iuv, status )
         map_defined = .true.
         call cln_setdef( .true., status )
         call stredt( iuv, -3, status )
         call stxrdt( 1, 1, status )
         call stxrec( clean_text_header, clean_record, status )
         call opemap( icc, map_name(1:len_m), 'WRITE', iprint, status )
         call wrredt( icc, 0, status )
         call wrmapc( icc, cc_posns, cc_values, number_cc, status )
         close (icc)
         if (cmd_dblev(2)) then
           print *,'.. CCMP map redtape and initial data written'
         end if

       end if

C specify file as defined -- makes certain commands accessible
       map_defined = status.eq.0
999    call cmd_err(status,'SET-MAP','Failed')
       end
C
C
*+ cln_display

       subroutine cln_display(iout,status)
C      -----------------------------------
C
C Display settings for all CLEAN parameters on unit IOUT
C
C Given:
C   output unit number
       integer      iout
C Returned:
C   error status
       integer      status
C
C Display all the information concerning the current CLEAN setup.
C
*-
       include '/mrao/anmap/include/clean_record.inc'
       include '/mrao/include/maplib_redtape.inc'

       integer    n, i

C check status on entry
       if (status.ne.0) return

       call ldredt(redt_map,status)
       write(iout,100)generic_name,beam_name
       write(iout,101)iumap1,iumap2,ivmap1,ivmap2
       write(iout,102)(clean_window(i), i=1,4)
       write(iout,103)(beam_window(i), i=1,4)
       do n=1,number_search
         write(iout,104)n,(search_window(i,n), i=1,4)
       end do
100    FORMAT(1X/1X,' Generic file name  = ',A/
     >           1X,' Beam file name     = ',A/1X)
101    FORMAT(1X,' Range on input map = ',4I6)
102    FORMAT(1X,' Clean window       = ',4I6)
103    FORMAT(1X,' Beam window        = ',4I6)
104    FORMAT(1X,' Search window (',I1,')  = ',4I6)
       write(iout,105)itlim
       write(iout,106)fllim
       write(iout,107)fract
105    FORMAT(1X,' Iteration limit    = ',I6)
106    FORMAT(1X,' Flux limit         = ',1PE12.3)
107    FORMAT(1X,' Fraction removed   = ',F8.3/1X)

C options
       if (lstop0) then
         write(iout,*)' '
         write(iout,*)' Truncation at first zero component enabled'
       end if
       if (lnotbx) then
         write(iout,*)' '
         write(iout,*)' Not box selected:'
         write(iout,108)(box_window(i), i=1,4)
       end if
108    FORMAT(1X,' NOT-box window     = ',4I6/1X)
       if (lprhat) then
         write(iout,*)' '
         write(iout,*)' Prussian Hat clean enabled'
         write(iout,109)prhat
       END IF
109    FORMAT(1X,' Prussian hat height= ',F8.2/1X)
       write(iout,110)beam_size_u,beam_size_v,beam_pa
110    FORMAT(1X/1X,' Gaussian CLEAN beam size = ',2F8.2,' arcsec ',
     >           'pa = ',f8.2,' degrees')
       if (ltrbm) then
         write(iout,*)' '
         write(iout,*)' Truncated dirty-beam option enabled'
       end if
       if (lsort) then
         write(iout,*)' '
         write(iout,*)' SORT option enabled - MRAO go-faster CLEAN'
         write(iout,111)sort_depth, sort_inner, sort_gate
111      FORMAT(1X,' Sort-Depth         = ',I10/
     >             ' Inner-Iterations   = ',I10/
     >             ' Sort-Gate          = ',1PE10.2)
       end if
       write(iout,*)' '
       if (times_cleaned .ne.0) call cln_result(iout,status)

       end
C
C
*+ cln_result

       subroutine cln_result(iout,status)
C      ----------------------------------
C
C Report on the results of previous CLEANs to unit IOUT
C
C Given:
C    output unit number
       integer     iout
C Returned:
C    error status
       integer     status
C
C Report details of the results of completed CLEAN runs.
C
*-
       include '/mrao/anmap/include/clean_record.inc'

C check status on entry
       if (status.ne.0) return

       if (times_cleaned .eq. 0) then
         write(iout,'(1x/1x,''.. clean not run on this map''/1x)')
         return
       end if
       write(iout,120) times_cleaned,number_it_done,number_cc,
     >                 max_on_resid
120    FORMAT(1X/1X,'.. CLEAN completion information:'/1X/
     >           1X,'   Number of times CLEANed     =',I8/
     >           1X,'   Number iterations completed =',I8/
     >           1X,'   Number of CLEAN components  =',I8/
     >           1X,'   Maximum on residual map     =',1PE12.3/1X)
       if (how_ended .lt. 0) then
         WRITE(IOUT,'(1X,''.. CLEAN terminated abnormally STATUS = '',
     >                i4)')how_ended
       else if (how_ended .eq. 1) then
         WRITE(IOUT,'(1X,''.. CLEAN Terminated at iteration limit = '',
     >                I8/'' .. Residual-maximum = '',1PE12.3)')
     >                number_it_done,max_on_resid
       else if (how_ended .eq. 2) then
         WRITE(IOUT,'(1X,''.. CLEAN Terminated at flux limit = '',
     >                1PE12.3/'' .. Number of iterations = '',I8)')
     >                max_on_resid,number_it_done
       else if (how_ended .eq. 3) then
         WRITE(IOUT,'(1X,''.. CLEAN terminated at negative residual'')')
       else if (how_ended .eq. 4) then
         WRITE(IOUT,'(1X,''.. CLEAN terminated on user intervention'')')
       end if
       write(iout,*)' '

       end
C
C
*+ cln_setdef

       subroutine cln_setdef(full,status)
C      ----------------------------------
C
C reset the CLEAN defaults
C Given:
C   logical flag indicating whether a full initialisation is required
       logical       full
C Returned:
C    error status
       integer       status
C
C Initialise the control information for CLEAN.  If full is set then
C the results information is initialised as well.
*-
       include '/mrao/anmap/include/clean_record.inc'
       include '/mrao/include/maplib_redtape.inc'

       integer  n

       if (status.ne.0) return

C reset values for parameters
       lnotbx = .false.
       do n=1,4
         box_window(n) = 0
       end do
       if (full) then
         ltrbm = .false.
         lprhat = .false.
         prhat = 0.0
         lclean = .true.
         lrest = .true.
         fllim = 0.001
         fract = 0.2
         itlim = 100
         lsort = .false.
         sort_gate  = 0.0
         sort_depth = 100
         sort_inner = 100
         number_cc  = 0
         times_cleaned = 0
         number_it_done = 0
         max_on_resid = 0.0
         how_ended = 0
       end if

       do n=1,4
         box_window(n) = 0
       end do

       if (map_defined) then

         call ldredt(redt_map,status)
         call iuv_load(clean_window,status)
         number_search = 1
         do n=1,9
           call iuv_load(search_window(1,n),status)
         end do

         call ldredt(redt_beam,status)
         beam_pa = 0.0
         if (hpfbwu.gt.0.0) then
             beam_size_u = hpfbwu
             beam_size_v = hpfbwv
         else
           if (freq.gt.100.0 .and. freq.lt.40000.0) then
             beam_size_u = 2.0*5000.0/freq
           else
             beam_size_u = 1.0
           end if
           beam_size_v = beam_size_u
           beam_size_v = beam_size_v/sin(decmap)
         end if
         call iuv_load(beam_window,status)

       end if

       end
C
C
*+ cln_start

       subroutine cln_start(map_array,online,status)
C      ---------------------------------------------
C
C Start the clean process; create files, spool process or run
C
C Given:
C   map work space
       real*4                map_array(*)
C   online or offline flag
       logical               online
C
C Returned:
C   error status
       integer               status
C
C To start the clean process from within the interactive sequence.
C The CCSL file is opened and the current control parameters written.
C The .res file is initialised. A CLEAN process is spooled if the
C ONLINE flag is set to false, otherwise the process is run interactively
C at the terminal.
C
C SUNOS Version 1.0 03/04/92
*-
       include '/mrao/anmap/include/clean_record.inc'
       include '/mrao/anmap/include/clean_sys_pars.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
C  character strings for file names
       character*60    directory, residual_map, user_name, string
       character*256   command
C  pointers
       integer         ip_map
C  unit numbers
       integer         iresi, icc
C  length of character strings
       integer         len_r, len_g
C  local status word
       integer         istat
C  character variable for number conversion
       character*5     number
       integer         len_number

C check status on entry
       if (status.ne.0) return

C enquire user name
       call io_namusr(' ',user_name,0,status)

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

       else
         call io_wrout(
     *        '.. residual map exists: restarting clean process')

       end if

C write out clean-components control data
       call opemap( icc, map_name(1:chr_lenb(map_name)),
     *              'UPDATE', 0, status )
       call rdredt( icc, 0, status )
       call stxrec( clean_text_header, clean_record, status )
       call wrredt( icc, 0, status )
       close (icc)

C take action if on/off line
       call chr_chitoc(map_cat,number,len_number)
       call cmd_setparam('%clean-map',number(1:len_number),status)
       call chr_chitoc(beam_cat,number,len_number)
       call cmd_setparam('%clean-beam',number(1:len_number),status)
       call cmd_end(status)
       command = clean_online_command//' '//
     *             residual_map(1:chr_lenb(residual_map))
       if (online) then
         call anm_exec(command,'clean','background','exact',status)
       else
         call anm_exec(command,'clean','batch','exact',status)
       end if

C error reporting and releasing allocated store
       call cmd_err(status,'clean-start','Clean failed')
       end
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
       include '/mrao/anmap/include/clean_record.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
C  character strings for file names
       character*60    directory, residual_map, user_name, string
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

C enquire user name
       call io_namusr(' ',user_name,0,status)
       if (cmd_dblev(1)) iprint = 1

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
       if (cmd_dblev(1)) then
         print *,'.. CCMP_map = ',map_name(1:len_m)
         iprint = 1
       end if
       if (exist) then
C delete existing file and re-create
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


