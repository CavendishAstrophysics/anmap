*+ plot_vectors

       subroutine plot_vectors( status )
C      ---------------------------------
C
C Setup sub-system for plotting of vector maps
C
C Returned:
C    error status
       integer       status
C
C A command line driven sub-system for defining aspects of the plot
C for adding vectors.
C
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
C   command line, length and initial check
       character*120      command_line
       integer            len_com
       logical            exit_on_completion
C   command line interpretation
       integer            number_commands, icom
       parameter         (number_commands = 11)
       character*80       liscom(number_commands)
C
C define commands
       data liscom(1) /
     *  'on ........................... turn plotting of vectors on'
     *                /
       data liscom(2) /
     *  'off .......................... turn plotting of vectors off'
     *                /
       data liscom(3) /
     *  'position-angle-map ........... define map position-angle map'
     *                /
       data liscom(4) /
     *  'intensity-map ................ define intensity map'
     *                /
       data liscom(5) /
     *  'scaled-vectors ............... select scaled vectors'
     *                /
       data liscom(6) /
     *  'constant-length-vectors ...... select constant length vectors'
     *                /
       data liscom(7) /
     *  'sampling ..................... set sampling of vector plot'
     *                /
       data liscom(8) /
     *  'gate ......................... set gate on intensity map'
     *                /
       data liscom(9) /
     *  'rotation-angle ............... set rotation angle for vectors'
     *                /
       data liscom(10)/
     *  'display ...................... display vector options'
     *                /
       data liscom(11)/
     *  'help ......................... help on vector options'
     *                /


C check status on entry
       if (status.ne.0) return

C interogate command line and determine whether to return on completion
C of the current command.
       call io_enqcli( command_line, len_com )
       exit_on_completion = len_com.ne.0

100    continue
       status = 0
       call cmd_scncmd( .false., status )
       call cmd_getcmd( 'Vectors> ',
     *                     liscom,number_commands,icom,status)
       call cmd_scncmd( .true., status )
       if (status.ne.0) then
         call cmd_err(status,'Vector-Plot',' ')
         goto 100
       end if
       if (icom.le.0) return

C decode command
       if (chr_cmatch('on',liscom(icom))) then
         vectors_opt = .true.

       else if (chr_cmatch('off',liscom(icom))) then
         vectors_opt = .false.

       else if (chr_cmatch('position-angle-map',liscom(icom))) then
         call mapcat_getmap('Position-angle-map : ','Default-Map',
     *                      'READ',vec_chi_map,status)

       else if (chr_cmatch('intensity-map',liscom(icom))) then
         if (vec_int_map .eq. 0) vec_int_map = imap
         call mapcat_getmap('Intensity-map : ','Default-Map',
     *                      'READ',vec_int_map,status)

       else if (chr_cmatch('scaled-vectors',liscom(icom))) then
         if (vec_int_map .eq. 0) vec_int_map = imap
         call io_getr('Scale (grid-points/Intensity-Unit) : ',
     *             '*',vec_scale,status)
         vec_type = 0
         if (vec_scale.le.0) then
            vec_type = 1
            vec_length = abs(vec_scale)
         end if

       else if (chr_cmatch('constant-length-vectors',liscom(icom))) then
         call io_getr('Vector-length (grid-points) : ','*',
     *             vec_length,status)
         vec_type = 2

       else if (chr_cmatch('sampling',liscom(icom))) then
         call io_geti('Sampling in U : ','*',vec_u_samp,status)
         call io_geti('Sampling in V : ','*',vec_v_samp,status)

       else if (chr_cmatch('gate',liscom(icom))) then
         call io_getr('Gate-on-intensity-map : ','*',vec_gate,status)

       else if (chr_cmatch('rotation-angle',liscom(icom))) then
         call io_getr('Rotation-angle (degrees) : ','0.0',
     *              vec_rotate,status)

       else if (chr_cmatch('display',liscom(icom))) then
         call plot_shvec(status)

       else if (chr_cmatch('help',liscom(icom))) then
         call hlp_setfil( vectors_helpfile, status )
         call hlp_system( ' ', ' ', .true., .true., status)
         call cmd_err(status,'HELP (Vectors)',' ')

       end if

       if (.not.exit_on_completion) goto 100

       end
C
C
*+ plot_symbols

       subroutine plot_symbols( status )
C      ---------------------------------
C
C Setup sub-system for plotting of symbol maps
C
C Returned:
C    error status
       integer       status
C
C A command line driven sub-system for defining aspects of the plot
C for adding symbols.
C
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C local variables
C   command line, length and initial check
       character*120      command_line
       integer            len_com
       logical            exit_on_completion
       character          ans*1
C   command line interpretation
       integer            number_commands, icom
       parameter         (number_commands = 7)
       character*80       liscom(number_commands)
C
C define commands
       data liscom(1) /
     *  'on ........................... turn plotting of symbols on'
     *                /
       data liscom(2) /
     *  'off .......................... turn plotting of symbols off'
     *                /
       data liscom(3) /
     *  'mark-blank-values ............ select marking of blanks'
     *                /
       data liscom(4) /
     *  'mark-max-pixels .............. select marking of max values'
     *                /
       data liscom(5) /
     *  'mark-min-pixels .............. select marking of min values'
     *                /
       data liscom(6) /
     *  'display ...................... display symbol options'
     *                /
       data liscom(7)/
     *  'help ......................... help on symbol options'
     *                /


C check status on entry
       if (status.ne.0) return

C interogate command line and determine whether to return on completion
C of the current command.
       call io_enqcli( command_line, len_com )
       exit_on_completion = len_com.ne.0

100    continue
       status = 0
       call cmd_scncmd( .false., status )
       call cmd_getcmd( 'Symbols> ',
     *                     liscom,number_commands,icom,status)
       call cmd_scncmd( .true., status )
       if (status.ne.0) then
         call cmd_err(status,'Symbol-Plot',' ')
         goto 100
       end if
       if (icom.le.0) return

C decode command
       if (chr_cmatch('on',liscom(icom))) then
         symbol_opt = .true.

       else if (chr_cmatch('off',liscom(icom))) then
         symbol_opt = .false.

       else if (chr_cmatch('mark-blank-values',liscom(icom))) then
         if (io_yesno('Mark blank pixels : ','yes',status)) then
           call io_getkey('Symbol : ','o','.+-*#o',ans,status)
           if (status.eq.0) then
             symb_blank = ichar(ans)
           else
             symb_blank = -1
           end if
         else
           symb_blank = -1
         end if

       else if (chr_cmatch('mark-max-pixels',liscom(icom))) then
         if (io_yesno('Mark maximum pixels : ','yes',status)) then
           call io_getr('Upper gate map value : ','0.0',
     *               val_symb_max,status)
           call io_getkey('Symbol : ','+','.+-*#o',ans,status)
           if (status.eq.0) then
             symb_max = ichar(ans)
           else
             symb_max = -1
           end if
         else
           symb_max = -1
         end if


       else if (chr_cmatch('mark-min-pixels',liscom(icom))) then
         if (io_yesno('Mark minimum pixels : ','yes',status)) then
           call io_getr('Lower gate map value : ','0.0',
     *               val_symb_min,status)
           call io_getkey('Symbol : ','-','.+-*#o',ans,status)
           if (status.eq.0) then
             symb_min = ichar(ans)
           else
             symb_min = -1
           end if
         else
           symb_min = -1
         end if


       else if (chr_cmatch('display',liscom(icom))) then
         call plot_shsymb(status)

       else if (chr_cmatch('help',liscom(icom))) then
         call hlp_setfil( symbols_helpfile, status )
         call hlp_system( ' ', ' ', .true., .true., status)
         call cmd_err(status,'HELP (Symbols)',' ')

       end if

       if (.not.exit_on_completion) goto 100

       end
