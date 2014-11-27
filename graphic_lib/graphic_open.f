*+ graphic_open

       subroutine graphic_open( defn, s )
C      ----------------------------------
C
C Initialise the graphics device/window for the specified definition
C
C Given:
C  the definition for this graphic
       integer   defn(*)
C
C Updated:
C  error return
       integer   s
C
C The graphics device is initialised for the specified graphic -- if the
C device is not open the the device will be opened for graphical access.
C-


       include '../include/plt_basic_defn.inc'
       include '../include/plt_scratch_defn.inc'
       include '../include/plt_error_defn.inc'

       real*4    x1, x2, y1, y2, xr, yr
       integer   iunit, istat
       integer   pgindev

C check status on entry
       if (s.ne.0) return

C copy information for this graphic structure
       call graphic_copy_graphic( defn, graphic, s )

C check graphic device status
       if (.not.plot_open) then
           istat = pgindev( iunit, plot_device,
     *                     plot_segments, plot_segments,
     *                     plot_list_id(plot_list_selected))
           call pgpap(plot_size,plot_aspect)
           call pgadvance
           call pgpap(plot_size,plot_aspect)
           call pgask( .false. )
         if (istat.ne.1) then
           s = ill_device
         else
           plot_open = .true.
           if (s.eq.0 .and. plot_open) then
             if (plot_aspect.lt.0.01) then
               plot_aspect = 0.7
             endif
             call pgpap(plot_size,plot_aspect)
             call pgadvance
             call pgpap(plot_size,plot_aspect)
           endif
         endif
       elseif (plot_next) then
         call pgpap(plot_size,plot_aspect)
         call pgadvance
         call pgpap(plot_size,plot_aspect)
         plot_next = .false.
       endif
       if (s.ne.0) goto 999

C initialise view port
       call pgsetvp( graphic_view_port(1), graphic_view_port(2),
     *               graphic_view_port(3), graphic_view_port(4) )
       call pgsetwin( 0.0, 1.0, 0.0, 1.0 )

C check graphic status and if necessary re-initialise
       if (graphic_status.eq.0 .or. plot_refresh .or.
     *     (graphic_type.eq.graphic_scratch) ) then
         if (.not.graphic_transparent) then
            call pgsfs( 1 )
            call pgsci( colour_background )
            call pgrect( 0.0, 1.0, 0.0, 1.0 )
         endif
         graphic_status = 1
       endif
C take special action to setup scratch options
       if (graphic_type.eq.graphic_scratch .and.
     *     scratch_view_port_opt.eq.1 ) then
         xr = (graphic_view_port(2)-graphic_view_port(1))
         yr = (graphic_view_port(4)-graphic_view_port(3))
         x1 = graphic_view_port(1) + scratch_view_port(1)*xr
         x2 = graphic_view_port(1) + scratch_view_port(2)*xr
         y1 = graphic_view_port(3) + scratch_view_port(3)*yr
         y2 = graphic_view_port(3) + scratch_view_port(4)*yr
         call pgsetvp( x1,x2,y1,y2 )
         call pgwindow(0.0,1.0,0.0,1.0)
       endif
       call pgsci( colour_foreground )
       call graphic_set_line_opt( graphic_line_opts, s )
       call graphic_set_text_opt( graphic_text_opts, s )
       call graphic_copy_graphic( graphic, defn, s )
999    call cmd_err(s, 'Graphic_open',
     *                 'Failed to open graphics device')
       end





