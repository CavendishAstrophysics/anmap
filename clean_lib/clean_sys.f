C
C
*+ clean_sys

       subroutine clean_sys(interp,cdata,map_array,status)
C      ------------------------------------------------
C
C Run the clean sub system
C
C Updated:
C    command interpreter data structure
       integer       interp(*)
C    command language data structure
       integer       cdata(*)
C   map data work space
       real*4        map_array(*)
C   error return code
       integer       status
C
C The clean sub system provides an interactive environment to setup
C clean runs which may then be spooled or run online. Checking of all
C values before the CLEAN is run is then possible, thus minimizing
C annoying batch crashes, but by running detached the terminal is
C not tied to the task.
C
*-
       include '../include/clean_record.inc'
       include '../include/anmap_sys_pars.inc'
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

C file names of residual and ccmp files
       character*256   file_r, file_c

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
         cdata(1) = 100
         return
       end if

c reset error status flag
       status =0
       call cmd_getcmd('Clean> ',liscom,
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
         cdata(1) = icomm
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

15     continue
16     continue
       call cln_start(map_array,file_r,file_c,status)
       call iocmd_tclsets( interp,file_r,'Clean(resid)',' ',status)
       call iocmd_tclsets( interp,file_c,'Clean(ccmp)',' ',status)
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


