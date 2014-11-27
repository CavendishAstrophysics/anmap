*$ Routines to operate on map data
*  -------------------------------
C
C
C Version 1.7    12/08/87 P. Alexander MRAO
C Version 1.7a   28/08/87 Changes to map_endwk implemented to take map
C                         entry and calculate max/min and store
C                         redtape away.
C Version 1.7b   10/08/87 Better error handling and checking.
C Version 1.8a   21/10/87 Included a "CHANGE-BLANK-VALUES" command.
C Version 2.0a   12/04/88 Updated for new STACK routines.
C Version 2.1a   17/06/88 Addition of do_ring_flux.
C Version 2.1b    7/07/88 All variables now declared so that implicit off
C                         can be used as a compiler option.
C Version 3.0    24/08/88 A number of new routines and re-ordering
C                         in the file. Mainly the new routines deal with
C                         map combinations.
C Version 3.1    15/10/88 do_stretch added.
C Version 3.2    07/06/89 Calls to redt_updscr added to routines producing
C                         new map types.
C         3.3    26/07/89 Added do_conv_pos
C         3.4    19/07/89 Added do_shift
C         3.5    15/10/89 Added print-map facility
C         3.6    19/10/89 Added do_loc_max routine: modified do_zapmap
C         3.7    12/03/90 Bug corrected in do_perc_map re polarization
C
C Version 4.0    25/07/90 Major code overhaul:
C                         - Change to new mapcat library routines
C                         - Data array passed by argument to all routines
C                         - Routine spec now standardised
C                         - A few long standing bugs fixed
C                         - Use of enquiry routines to access REDTAPE
C Version 4.1    15/08/90 Added: do_convolve, do_vflip, do_outmap
C Version 4.2    04/04/91 Improved use of enquiry routines and
C                         miscellaneous functions.
*-
*+ do_mapscl

       subroutine do_mapscl(map_array,status)
C      --------------------------------------
C
C Scale a map and apply a zero level correction
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The map is scaled such that:
C
C   "output-map"  =  "scale-factor" * ("input-map"  -  "offset-level")
C
C where scale-factor and offset-level are supplied by the user.
*-

C Local variables
       real*4     scale_factor, offset_level, blank_value
       integer    imap, ip_map, imapo, ip_mapo
       integer    i
       integer    minirt(8)

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call ennull(blank_value,status)
C force input map into core
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
C find output map
       call map_alloc_out(0,0,'DIRECT',imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C read the scale factor and zero-level offset to apply
       call io_getr('Scale-Factor : ','1.0',scale_factor,status)
       call io_getr('Offset-Level : ','0.0',offset_level,status)
       if (status.ne.0) goto 999

C scale the map
       call redt_load( imap, status )
       call enminirt(minirt,status)
       do i = 1,minirt(5)*minirt(6)
         if (map_array(ip_map+i-1).ne.blank_value) then
           map_array(ip_mapo+i-1) = scale_factor *
     *                            (map_array(ip_map+i-1) - offset_level)
         else
           map_array(ip_mapo+i-1) = blank_value
         end if
       end do

C Update the TEXT of the redtape
       call adredt('CREATED','SCALE',status)
       call stnull(blank_value,status)

C Tidy up, make output map new current map etc.
999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'SCALE-MAP','Failed ')
       end
