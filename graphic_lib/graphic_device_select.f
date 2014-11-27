*+ graphic_device_select

       subroutine graphic_device_select( idev, s )
C      -------------------------------------------
C
C Select one of the standard Graphic devices
C
C Given:
C  selected device
      integer    idev
C
C Updated:
C  error status
      integer    s
C
C The specified device is selected.  If the device is currently open then
C subsequent plotting commands are directed to this device, if the device is
C not currently open then the device is still selected but will not be physically
C opened, an output-device command can then be used to change the device
C specification for this standard device.
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_error_defn.inc'

C local variables
       integer  n

       if (s.ne.0) return

C enable default selection
       if (idev.eq.0) then
         n = plot_list_default
       else
         n = idev
       endif

C perform selection
       if (n.le.0 .or. n.gt.plot_list_max) then
          s = ill_grdev
       else
          plot_list_open(plot_list_selected) = plot_open
          plot_list_device(plot_list_selected) = plot_device
          plot_list_aspect(plot_list_selected) = plot_aspect
          plot_list_size(plot_list_selected) = plot_size
          if (n.ne.plot_list_selected) then
             plot_list_selected = n
             plot_open = plot_list_open(plot_list_selected)
             plot_device = plot_list_device(plot_list_selected)
             if (plot_open) then
               plot_size = plot_list_size(plot_list_selected)
               plot_aspect = plot_list_aspect(plot_list_selected)
               call pgselect(plot_list_id(plot_list_selected))
               call pgpap(plot_size,plot_aspect)
             endif
          endif
       endif

       call cmd_err(s,'graphic_device_select',' ')
       end


