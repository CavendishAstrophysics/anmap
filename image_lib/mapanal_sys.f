*$ Map-Analysis Sub-system
*  -----------------------
C
C Last updated Paul Alexander MRAO, 25/02/93
C
*+ mapanal_sys

       subroutine mapanal_sys(interp,cdata,map_array,status)
C      -----------------------------------------------------
C
C Map analysis sub-system
C
C Updated:
C    command interpreter data structure
       integer       interp(*)
C    command language data structure
       integer       cdata(*)
C   map array work space
       real*4         map_array(*)
C   error return
       integer        status
C
C Sub-system providing commands to operate on maps.
*-
C
       include  '../include/anmap_sys_pars.inc'
C
       integer          number_commands
       parameter       (number_commands = 49)
       character*60     liscom(number_commands)
C
       character*80     command_line
       integer          len_cli

       logical          exit_at_end
       integer          ncomms, icom, i_com_done

       data liscom(1) /
     *  'add-flux ................. find flux on CLEAN map'
     *                /
       data liscom(2) /
     *  'strip-flux ............... calculate flux in strips'
     *                /
       data liscom(3) /
     *  'ring-flux ................ find flux in ellipses'
     *                /
       data liscom(4) /
     *  'alpha-map ................ make spectral index map'
     *                /
       data liscom(5) /
     *  'print-map ................ print values from a map'
     *                /
       data liscom(6) /
     *  'output-map ............... write map as data array'
     *                /
       data liscom(7) /
     *  'input-map ,............... read map as data array'
     *                /
       data liscom(8) /
     *  'map-histogram ............ histogram of map values'
     *                /
       data liscom(9) /
     *  'scatter-plot ............. plot data map-1 vs. map-2'
     *                /
       data liscom(10)/
     *  'plot-slice ............... plot slice through map'
     *                /
       data liscom(11)/
     *  'shrink-map ............... reduce map size in UV'
     *                /
       data liscom(12)/
     *  'expand-map ............... expand map size in UV'
     *                /
       data liscom(13)/
     *  'stretch-map .............. reproject to sky-plane'
     *                /
       data liscom(14)/
     *  'vertical-flip ............ flip image in V direction'
     *                /
       data liscom(15)/
     *  'horizontal-flip .......... flip image in U direction'
     *                /
       data liscom(16) /
     *  'reproject-map ............ reproject onto new grid'
     *                /
       data liscom(17)/
     *  'UV-shift-map ............. shift map in U and/or V'
     *                /
       data liscom(18)/
     *  'UV-rotate-map ............ rotate map on UV grid'
     *                /
       data liscom(19)/
     *  'smooth-map ............... gaussian convolution'
     *                /
       data liscom(20)/
     *  'convolve-map ............. convolve with any function'
     *                /
       data liscom(21)/
     *  'scan-map ................. scan map for max/min/mean'
     *                /
       data liscom(22)/
     *  'noise-map ................ find noise on a map'
     *                /
       data liscom(23)/
     *  'scale-map ................ appply scaling and offset'
     *                /
       data liscom(24)/
     *  'zap-map .................. set map area to zero'
     *                /
       data liscom(25)/
     *  'change-blank-values ...... change blank values'
     *                /
       data liscom(26)/
     *  'gate-map ................. gate a map with itself'
     *                /
       data liscom(27)/
     *  'logarithmic-map .......... take logarithm (ln) map'
     *                /
       data liscom(28)/
     *  'exponentiate-map ......... take exponential of map'
     *                /
       data liscom(29)/
     *  'add-maps ................. add two maps with weight'
     *                /
       data liscom(30)/
     *  'divide-maps .............. divide two maps'
     *                /
       data liscom(31)/
     *  'multiply-maps ............ multiply two maps'
     *                /
       data liscom(32)/
     *  'mask-map ................. mask one map with another'
     *                /
       data liscom(33)/
     *  'CHI-map .................. make position-angle map'
     *                /
       data liscom(34)/
     *  'mI-map ................... make polarization map'
     *                /
       data liscom(35)/
     *  'PERC-map ................. make percentage pol. map'
     *                /
       data liscom(36)/
     *  'makei .................... make I map from I+-Q/Q'
     *                /
       data liscom(37)/
     *  'predict-map .............. extrapolate alpha to f3'
     *                /
       data liscom(38)/
     *  'pb-correction ............ RT/CLFST primary beam'
     *                /
       data liscom(39)/
     *  'pbcor-omt ................ correct OMT Primary-Beam'
     *                /
       data liscom(40)/
     *  'pb-make .................. make a map of RT/CLFST PB'
     *                /
       data liscom(41)/
     *  'convert-position ......... convert RA/DEC to/from UV'
     *                /
       data liscom(42)/
     *  'local-maximum ............ find local maximum on map'
     *                /
       data liscom(43)/
     *  'multi-add-maps ........... addition of many maps'
     *                /
       data liscom(44)/
     *  'angular-flux ............. add flux in angular annuli'
     *                /
       data liscom(45)/
     *  ' '
     *                /
       data liscom(46)/
     *  'extract-slice ............ extract slice through map'
     *                /
       data liscom(47)/
     *  'binary-map ............... perform a 2-level digitisation'
     *                /
       data liscom(48)/
     *  'get ...................... get information from the map'
     *                /
       data liscom(49)/
     *  'reproj2 .................. modified map reprojection'
     *                /
       ncomms = number_commands

C check for exit on completion of command
       call io_enqcli(command_line,len_cli)
       exit_at_end = len_cli.ne.0
       i_com_done = 0

1000   continue

C .. auto-exit
         if (exit_at_end .and. i_com_done.gt.0) then
           cdata(1) = 100
           return
         end if

C .. command line interpretation
         if (.not.exit_at_end) call io_setcli(' ')

C .. reset error return
         status=0

C .. read command
         call cmd_getcmd('Map-Analysis> ',liscom,ncomms,
     *                    icom,status)

C check for error
         if (status.ne.0) then
           call cmd_err(status,'MAP-ANALYSIS',' ')
           goto 1000
         else
           i_com_done = i_com_done + 1
         end if

C .. check icom for a basic command
         if (icom.le.0) then
           cdata(1) = icom
           return
         end if


C jump to required command in command list
         goto (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     *         21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,
     *         38,39,40,41,42,43,44,45,46,47,48,49)
     *       icom

1        call do_add_flux(map_array,status)
         goto 1000
2        call do_strip_flux(map_array,status)
         goto 1000
3        call do_ring_flux(map_array,status)
         goto 1000
4        continue
         goto 1000
5        call do_mapprint(map_array,status)
         goto 1000
6        continue
         goto 1000
7        continue
         goto 1000
8        call plot_histmap(map_array,status)
         goto 1000
9        call plot_scatter(map_array,status)
         goto 1000
10       call plot_slice(map_array,status)
         goto 1000
11       call do_shrink(map_array,status)
         goto 1000
12       call do_expand(map_array,status)
         goto 1000
13       call do_stretch(map_array,status)
         goto 1000
14       call do_vflip(map_array,status)
         goto 1000
15       call do_hflip(map_array,status)
         goto 1000
16       call do_reproject(map_array,status)
         goto 1000
17       call do_shift(map_array,status)
         goto 1000
18       call do_rotate(map_array,status)
         goto 1000
19       call do_smooth(map_array,status)
         goto 1000
20       call do_convolve(map_array,status)
         goto 1000
21       call do_scan_map(map_array,status)
         goto 1000
22       call do_noise_map(map_array,status)
         goto 1000
23       call do_mapscl(map_array,status)
         goto 1000
24       call do_zapmap(map_array,status)
         goto 1000
25       call do_chr_chblnk(map_array,status)
         goto 1000
26       call do_gate_map(map_array,status)
         goto 1000
27       call do_log_map(map_array,status)
         goto 1000
28       call do_exp_map(map_array,status)
         goto 1000
29       call do_add_maps(map_array,status)
         goto 1000
30       call do_divide_maps(map_array,status)
         goto 1000
31       call do_multiply_maps(map_array,status)
         goto 1000
32       call do_mask_map(map_array,status)
         goto 1000
33       call do_chi_map(map_array,status)
         goto 1000
34       continue
         goto 1000
35       continue
         goto 1000
36       continue
         goto 1000
37       continue
         goto 1000
38       continue
         goto 1000
39       continue
         goto 1000
40       continue
         goto 1000
41       call do_conv_pos(map_array,status)
         goto 1000
42       call do_loc_max(map_array,status)
         goto 1000
43       call multi_add_sys(status)
         goto 1000
44       call do_angular_flux(map_array,status)
         goto 1000
45       goto 1000
46       call extract_slice(map_array,status)
         goto 1000
47       call do_binary_map(map_array,status)
         goto 1000
48       call do_get(map_array,status)
         goto 1000
49       call do_reproj2(map_array,status)
         goto 1000

       end

