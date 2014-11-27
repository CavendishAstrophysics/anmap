C
C
*+ plot_setmap

       subroutine plot_setmap(overlay,map_array,status)
C      ------------------------------------------------
C
C define map to use for the display
C
C Given:
C   flag indicating whether this is the main or overlay map
       logical     overlay
C   map data array
       real*4      map_array(*)
C Updated:
C   errors status
       integer     status
C
C If OVERLAY is set to false on entry to the routine then the user
C defines a new map to plot. If FRAME_DONE is set to TRUE then a call
C to NEXT-FRAME is effectively made to move onto the next plot. The
C UV_RANGE etc. are then redefined before plotting.
C
C If OVERLAY is set to true then the map is checked for the UV_REGION
C as defined being on the map. If it is, then the map is setup as an
C overlayed map, otherwise the command is faulted and no change is made.
*-
       include '../include/anmap_sys_pars.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

       integer      uv_test(4)
       logical      uv_within

C check status value on entry
       imap_current = 1
       if (status.ne.0) return

C check that overlay mode must follow an ordinary "set-map" command
       if (overlay .and. (.not.map_defined .or. .not.plot_open)) then
         call cmd_wrerr('SET-MAP','No existing frame/map')
         return
       end if

C read input map from user
       if (.not.overlay) then
         call mapcat_getmap('Catalogue-entry : ','Default-Map',
     *                      'Read',imap,status)
         call redt_load(imap,status)
       else
         call mapcat_getmap('Catalogue-entry : ','Default-Map',
     *                      'Read',imap_overlay,status)
         call redt_load(imap_overlay,status)
       endif
       if (status.eq.0 .and. overlay) then

C .. test for overlap of defined uv_range on overlay map
         call iuv_load(uv_test,status)
         if (.not.uv_within(uv_range,uv_test)) then
           call cmd_wrerr('SET-MAP',
     >      '"OVERLAY" Map does not contain current UV-range')
           call cmd_wrerr('SET-MAP','Map not defined as overlay')
           imap_overlay = 0
           return
         end if
         imap_current = 2

       else
C .. define new plot
         map_defined   = .true.
         call plot_init_plot( status )
         imap_current = 1
C .. define default UV_range (whole map)
         call iuv_load(uv_range,status)
C .. define pips for default uv_range
         call plot_getpip(.false.,status)
C .. define executable command
         call cmd_exec( plotsys_setmap_command, status )
       end if

C find data range for this map
       call enrnge( data_range(1,imap_current), status )
       call cmd_err(status,'SET-MAP',' ')
       end
