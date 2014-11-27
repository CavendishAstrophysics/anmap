*+ anmap_command

       subroutine anmap_command( interp, map_array, cs, lcs, status )
C      --------------------------------------------------------------
C
C Command dispatcher for Anmap.
       integer         interp(*)
       real*4          map_array(*)
       character*1024  cs
       integer         lcs, status
C
C Mullard Radio Astronomy Observatory
C
C Version 7.4(e) SUNOS Version
C
C Copyringth P. Alexander MRAO, Cambridge
C
C This is the main program to implement the ANMAP map, image and
C data display and analysis system.  This version has been developped
C under SUNOS version 4.1.3 on SUN SPARC stations
C
*-
C=====================================================================
C Initialisation
C --------------
C
       include '/mrao/include/iolib_constants.inc'
       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/cmd_lang_data.inc'
C
C Define work space (map sizes are defined in anmap-sys-pars)
       real*4           map_array( map_size*number_maps +
     *                             buffer_size*number_buffers)
C
C current command line
       character*120    command_line
C number of commands defined in main level and number of sub-options
       integer          number_commands, number_normal_commands
       parameter       (number_commands = 25)
C list of commands defined at this level (basic)
       character*80     liscom(number_commands)
C command-line parameters to pass to commands & current command name
       character*80     command_name
C number of callable sub-systems and a list
       integer          number_sub_systems
       parameter       (number_sub_systems = 15)
       character*20     sub_system_list(0:number_sub_systems)
C exit program
       character*40     exit_program
       integer          len_program
C error code
       integer          status
C calling system (return here on completion)
       integer          i_system
C command number found in parsing routine
       integer          icom
C length of command line
       integer          len_cli, i1
C length of option string, command and prompt
       integer          len_c, len_string
C general string variable
       character*80     string
C output unit and catalogue entry
       integer          iout, imap
C return to i_system on completion if true
       logical          return_system
C
C-----------------------------------------------------------------------
C
C initialise commands
       data sub_system_list(0) /
     *  'NO-INITIALISE       ' /
       data liscom(1) /
     *  'news ..................... DEFUNCT'
     *                /
       data liscom(2) /
     *  'help ..................... DEFUNCT'
     *            /
       data liscom(3) /
     *  'edit-file ................ use the ee-editor'
     *            /
       data liscom(4) /
     *  'list-basic-commands ...... list basic commands'
     *                /
       data liscom(5) /
     *  'display-version .......... display program version No.'
     *                /
       data liscom(6)/
     *  'redtape .................. display redtape for a map'
     *                /
       data liscom(7)/
     *  'file-system .............. enter file system commands'
     *                /
       data liscom(8) /
     *  'catalogue-system ......... enter catalogue-system'
     *                /
       data liscom(9) /
     *  'TV-load .................. load an image into the TV'
     *                /
       data liscom(10)/
     *  'no-initialise ............ start ANMAP without banner'
     *                /
       data liscom(11)/
     *  'read-data-file ........... read a data-file into ANMAP'
     *                /
       data liscom(12)/
     *  'write-data-file .......... write a data-file to disc'
     *                /

       data liscom(13)/
     *  ' '
     *                /

       parameter  (number_normal_commands = 13)

       integer     isys_synch
       parameter  (isys_synch = 14)
       data liscom(isys_synch)/
     *  'synchrotron-analysis ..... enter synchrotron analysis'
     *                /
       data sub_system_list(isys_synch-number_normal_commands) /
     *  'SYNCHROTRON-ANALYSIS ' /

       integer     isys_clean
       parameter  (isys_clean = 15)
       data liscom(isys_clean)/
     *  'clean-system ............. enter clean sub-system'
     *                /
       data sub_system_list(isys_clean-number_normal_commands) /
     *  'CLEAN               ' /

       integer     isys_mapdisplay
       parameter  (isys_mapdisplay = 16)
       data liscom(isys_mapdisplay)/
     *  'map-display .............. maps/images display system'
     *                /
       data sub_system_list(isys_mapdisplay-number_normal_commands) /
     *  'MAP-DISPLAY         ' /

       integer     isys_drawing
       parameter  (isys_drawing = 17)
       data liscom(isys_drawing)/
     *  'drawing-display .......... drawing system'
     *                /
       data sub_system_list(isys_drawing-number_normal_commands) /
     *  'DRAWING-DISPLAY     ' /

       integer     isys_scratch
       parameter  (isys_scratch = 18)
       data liscom(isys_scratch)/
     *  'scratch-display .......... scratch display system'
     *                /
       data sub_system_list(isys_scratch-number_normal_commands) /
     *  'SCRATCH-DISPLAY     ' /

       integer     isys_datadisplay
       parameter  (isys_datadisplay = 19)
       data liscom(isys_datadisplay)/
     *  'data-display ............. data/graphs display system'
     *                /
       data sub_system_list(isys_datadisplay-number_normal_commands) /
     *  'DATA-DISPLAY        ' /

       integer     isys_graphic
       parameter  (isys_graphic = 20)
       data liscom(isys_graphic)/
     *  'graphic-system ........... control of graphic displays'
     *                /
       data sub_system_list(isys_graphic-number_normal_commands) /
     *  'GRAPHIC-SYSTEM      ' /

       integer     isys_analysis
       parameter  (isys_analysis = 21)
       data liscom(isys_analysis)/
     *  'map-analysis ............. image analysis commands'
     *                /
       data sub_system_list(isys_analysis-number_normal_commands) /
     *  'MAP-ANALYSIS        ' /

       integer     isys_spectrum
       parameter  (isys_spectrum = 22)
       data liscom(isys_spectrum)/
     *  'spectrum-analysis ........ analyse spectral-type files'
     *                /
       data sub_system_list(isys_spectrum-number_normal_commands) /
     *  'SPECTRUM-ANALYSIS    ' /

       integer     isys_redtape
       parameter  (isys_redtape = 23)
       data liscom(isys_redtape)/
     *  'edit-redtape ............. redtape editor'
     *                /
       data sub_system_list(isys_redtape-number_normal_commands) /
     *  'EDIT-REDTAPE        ' /

       integer     isys_edit_image
       parameter  (isys_edit_image = 24)
       data liscom(isys_edit_image)/
     *  'edit-image ............... edit image data'
     *                /
       data sub_system_list(isys_edit_image-number_normal_commands) /
     *  'EDIT-IMAGE          ' /

       integer     isys_true_colour
       parameter  (isys_true_colour = 25)
       data liscom(isys_true_colour)/
     *  'true-colour .............. analysis of true-colour images'
     *                /
       data sub_system_list(isys_true_colour-number_normal_commands) /
     *  'TRUE-COLOUR         ' /



C-----------------------------------------------------------------------
C

C Define error flag for exception handling
       status = 0
       if (lcs.eq.0) return
       call io_setcli( ' ' )
       call io_setcli(cs(1:lcs))
       lcs = 0
       cmd_results = ' '

C initialise command-line interpretation control counters
       cmd_data(1) = 0
       i_system  = number_normal_commands
       return_system = .false.

C  Command line interpretation -- Basic commands
C  ---------------------------------------------
1000   continue

C reset error return
         status = 0

C check for auto-return to calling level and return if required
         if (return_system) then
           icom = i_system
           goto 2000
         end if

C scan command line
         call cmd_setparam('%sub-system','ANMAP',status)
         i_system = number_normal_commands
         call cmd_scncmd(.false.)
         call cmd_setscope(1)
         call cmd_getcmd('Anmap> ',liscom,
     *                    number_commands,icom,status)
         call cmd_scncmd(.true.)

C check for error
         if (status.ne.0) then
           call cmd_err(status,'ANMAP',' ')
           goto 1000
         end if

C execute required command
 2000    continue

         if (icom.eq.0) then

C .. command is exit
C .. find exit program if any and execute command
           call cmd_enqparam('%exit-program',exit_program,status)
           call cmd_unsetparam('%exit-program',status)
           len_program = chr_lenb(exit_program)
           call anm_exit(status)
           call pgend
           if (len_program.gt.0) then
             call io_system(exit_program(1:len_program),status)
           end if
           goto 5000

         end if

C .. normal command
         len_c = chr_lenw(liscom(icom))
         command_name = liscom(icom)
         call chr_chucas(command_name)

C .. action on commands
         IF (COMMAND_NAME(1:LEN_C).EQ.'EDIT-FILE') THEN
           call cmd_editor('ANMAP',status)

         ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'LIST-BASIC-COMMANDS')THEN
           call io_prtopt(liscom,number_commands,' ',status)

         ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'DISPLAY-VERSION') THEN
           call io_enqout(iout)
           write(iout,50)sys_version,sys_date
50         format(1x/1x,'ANMAP Version ',a,' of ',a/
     *            1x,'-----'/1x)

         ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'REDTAPE') THEN
           call mapcat_getmap('Map : ','Default-Map',
     *                        'READ',imap,status)
           call redt_load(imap,status)
           call exm_print('ALL',status)

         ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'NO-INITIALISE')THEN
           continue

*         elseif (command_name(1:len_c).eq.'CATALOGUE-SYSTEM') THEN
*            call mapcat_sys(' ',map_array,status)

         elseif (command_name(1:len_c).eq.'FILE-SYSTEM') THEN
            call cmd_filesys(' ','File-System> ',status)

         ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'BATCH-MONITOR') THEN
            call anm_exec( batch_command,
     *          sub_system_list(i_system-number_normal_commands),
     *          'online','append',
     *          status)

         ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'SUGGEST') then
            call anm_exec( suggest_command,
     *          sub_system_list(i_system-number_normal_commands),
     *          'online','append',
     *          status)

         ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'READ-DATA-FILE') THEN
            call do_inmap( map_array, status )

         ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'WRITE-DATA-FILE') THEN
            call do_outmap( map_array, status )

         ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'TV-LOAD') THEN
            call TV_load( status )


         ELSE

C     Sub-System Definitions
C     ----------------------
           IF (COMMAND_NAME(1:LEN_C).EQ.'SYNCHROTRON-ANALYSIS') THEN
             call cmd_setparam(
     *            '%sub-system','SYNCHROTRON-ANALYSIS',status)
             call cmd_setscope(10)
             call synch_sys(interp,cmd_data(1),map_array,status)
             if (cmd_data(1).le.0) i_system = isys_synch


           ELSEIF (COMMAND_NAME(1:LEN_C).EQ.'CLEAN-SYSTEM') THEN
             call cmd_setparam('%sub-system','CLEAN',status)
             call cmd_setscope(11)
             call clean_sys(interp,cmd_data(1),map_array,status)
             if (cmd_data(1).le.0) i_system = isys_clean

           ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'MAP-DISPLAY') THEN
             call cmd_setparam('%sub-system','MAP-DISPLAY',status)
             call cmd_setscope(12)
             call plot_sys(interp,cmd_data(1),map_array,status)
             if (cmd_data(1).le.0) i_system = isys_mapdisplay

           ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'DRAWING-DISPLAY') THEN
             call cmd_setparam('%sub-system','DRAWING-DISPLAY',status)
             call cmd_setscope(13)
             call drawing_sys(interp,cmd_data(1),map_array,status)
             if (cmd_data(1).le.0) i_system = isys_drawing

           ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'SCRATCH-DISPLAY') THEN
             call cmd_setparam('%sub-system','SCRATCH-DISPLAY',status)
             call cmd_setscope(14)
             call scratch_sys(interp,cmd_data(1),map_array,status)
             if (cmd_data(1).le.0) i_system = isys_scratch

           ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'DATA-DISPLAY') THEN
             call cmd_setparam('%sub-system','DATA-DISPLAY',status)
             call cmd_setscope(15)
             call datadisplay_sys(interp,cmd_data(1),map_array,status)
             if (cmd_data(1).le.0) i_system = isys_datadisplay

           ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'GRAPHIC-SYSTEM') THEN
             call cmd_setparam('%sub-system','GRAPHIC-SYSTEM',status)
             call cmd_setscope(16)
             call graphic_sys(interp,cmd_data(1),map_array,status)
             if (cmd_data(1).le.0) i_system = isys_graphic

           ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'MAP-ANALYSIS') THEN
             call cmd_setparam('%sub-system','MAP-ANALYSIS',status)
             call cmd_setscope(17)
             call mapanal_sys(interp,cmd_data(1),map_array,status)
             if (cmd_data(1).le.0) i_system = isys_analysis

           ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'SPECTRUM-ANALYSIS') THEN
             call cmd_setparam('%sub-system','SPECTRUM-ANALYSIS',status)
             call cmd_setscope(18)
             call specanal_sys(interp,cmd_data(1),status)
             if (cmd_data(1).le.0) i_system = isys_spectrum

           ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'EDIT-REDTAPE') THEN
             call cmd_setparam('%sub-system','EDIT-REDTAPE',status)
             call cmd_setscope(19)
             call redtape_sys(interp,cmd_data(1),map_array,status)
             if (cmd_data(1).le.0) i_system = isys_redtape

           ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'EDIT-IMAGE') THEN
             call cmd_setparam('%sub-system','EDIT-IMAGE',status)
             call cmd_setscope(20)
             call image_edit(interp,cmd_data(1),map_array,status)
             if (cmd_data(1).le.0) i_system = isys_edit_image

           ELSE IF (COMMAND_NAME(1:LEN_C).EQ.'TRUE-COLOUR') THEN
             call cmd_setparam('%sub-system','TRUE-COLOUR',status)
             call cmd_setscope(21)
             call truecolour_sys(interp,cmd_data(1),map_array,status)
             if (cmd_data(1).le.0) i_system = isys_true_colour
           end if

C .. exit has been from a sub_system
           if (return_system .and. cmd_data(1).gt.0) then
C ... a sub-system was called with the form 'sub-sys command'
C ... return is to original calling sub-system
             icom = i_system
             goto 2000
           end if
C .. set return_system if the calling sub-system has called a
C .. basic-command
           return_system = cmd_data(1).lt.0

           if (return_system) then
C ... a basic command is required to be executed - do so
             icom = - cmd_data(1)
C ... jump and execute basic command (at this level)
             goto 2000
           end if

         end if
       if (chr_lenb(cmd_results).gt.0) then
         cs = cmd_results(1:chr_lenb(cmd_results))//char(0)
         lcs = chr_lenb(cmd_results)
       else
         cs = ' '
         lcs = 0
       endif
       call iocmd_pars2tcl( interp, status)
5000   continue

       end










