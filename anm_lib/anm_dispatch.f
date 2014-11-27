*+ anm_dispatch

       subroutine anm_dispatch( interp, map_array, cs, lcs, status )
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
C Version 7.4(f) SUNOS Version
C
C Copyrigth P. Alexander MRAO, Cambridge
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
C
C results string
       integer           iocmd_results(258)
       character*1024    results
       equivalence      (iocmd_results(3), results)
       
C number of commands defined in main level and number of sub-options
       integer          number_commands
       parameter       (number_commands = 14)
C list of commands defined at this level (basic)
       character*80     liscom(number_commands)
C command-line parameters to pass to commands & current command name
       character*80     command_name

C error code
       integer          status

C command number found in parsing routine
       integer          icom
       integer          len_c
C
C-----------------------------------------------------------------------
C
C initialise commands
       data liscom(1) /
     *  'TV-load .................. load an image into the TV'
     *                /
       data liscom(5) /
     *  'synchrotron-analysis ..... enter synchrotron analysis'
     *                /
       data liscom(6) /
     *  'clean-system ............. enter clean sub-system'
     *                /
       data liscom(7) /
     *  'map-display .............. maps/images display system'
     *                /
       data liscom(8) /
     *  'drawing-display .......... drawing system'
     *                /
       data liscom(9) /
     *  'scratch-display .......... scratch display system'
     *                /
       data liscom(10)/
     *  'data-display ............. data/graphs display system'
     *                /
       data liscom(11)/
     *  'graphic-system ........... control of graphic displays'
     *                /
       data liscom(12)/
     *  'map-analysis ............. image analysis commands'
     *                /
       data liscom(13)/
     *  'edit-redtape ............. redtape editor'
     *                /
       data liscom(14)/
     *  'edit-image ............... edit image data'
     *                /




C-----------------------------------------------------------------------
C

C Define error flag for exception handling
       status = 0
       if (lcs.eq.0) return
       call anm_disstart( interp, cs, lcs, status )
       results = ' '

C interprete command
         call io_getcmd('Anmap> ',liscom,
     *                   number_commands,icom,status)
         if (status.ne.0) then
           call iocmd_err(status,'ANMAP',' ')
           goto 1000
         end if

         len_c = chr_lenw(liscom(icom))
         command_name = liscom(icom)
         call chr_chlcas(command_name)
         if (command_name(1:len_c).eq.'tv-load') then
            call tv_load( status )

         elseif (command_name(1:len_c).eq.'synchrotron-analysis') then
             call synch_dispatch(interp,map_array,cs,lcs,status)

         elseif (command_name(1:len_c).eq.'clean-system') then
             call clean_sys(interp,iocmd_results,map_array,status)

         else if (command_name(1:len_c).eq.'map-display') then
             call plot_sys(interp,iocmd_results,map_array,status)

         else if (command_name(1:len_c).eq.'drawing-display') then
             call drawing_sys(interp,iocmd_results,map_array,status)

         else if (command_name(1:len_c).eq.'scratch-display') then
             call scratch_sys(interp,iocmd_results,map_array,status)

         else if (command_name(1:len_c).eq.'data-display') then
             call datadisplay_sys(interp,iocmd_results,map_array,status)

         else if (command_name(1:len_c).eq.'graphic-system') then
             call graphic_sys(interp,iocmd_results,map_array,status)

         else if (command_name(1:len_c).eq.'map-analysis') then
             call mapanal_sys(interp,iocmd_results,map_array,status)

         else if (command_name(1:len_c).eq.'edit-redtape') then
             call redtape_sys(interp,iocmd_results,map_array,status)

         else if (command_name(1:len_c).eq.'edit-image') then
             call image_edit(interp,iocmd_results,map_array,status)

         end if

1000   call anm_disend( interp, results, cs, lcs, status )
       end












