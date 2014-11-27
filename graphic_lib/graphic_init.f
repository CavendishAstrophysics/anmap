*+ graphic_init

       subroutine graphic_init( type, defn, s )
C      ----------------------------------------
C
C Initialise for the specified graphics type and return a full definition
C
C Given:
C  graphics type
       integer   type
C
C Returned:
C  the current definition for this graphic
       integer   defn(*)
C
C Updated:
C  error return
       integer   s
C
C The appropriate graphics definition of the specified type is returned;
C if no definition of the requested type is available then the routine will
C return a default definition with only the graphic initialised.
C-


       include '../include/plt_basic_defn.inc'
       include '../include/plt_graphic_array.inc'

C counters
       integer      i

C local control variable
       integer                       ig, ig_type
       common /graphic_init_control/ ig, ig_type

C check status on entry
       if (s.ne.0) return
       ig = 0
       if (type.ge.1 .and. type.le.graphic_max) then
         ig = graphic_default_current(type)
       endif
       if (ig.ne.0) then
         ig_type = type
         do i=1,len_struct
           defn(i) = graphic_array(i,ig)
         enddo
       else
         graphic_type = type
         graphic_view_port(1) = 0.0
         graphic_view_port(2) = 1.0
         graphic_view_port(3) = 0.0
         graphic_view_port(4) = 1.0
         graphic_device = 0
         graphic_transparent = .false.
         graphic_depth = 1
         graphic_index = -1
         call graphic_copy_graphic(graphic, defn, s)
       endif
       call graphic_copy_graphic(defn,graphic,s)
       call graphic_device_select(graphic_device,s)
       if (plot_open) then
         call pgsetvp(graphic_view_port(1),graphic_view_port(2),
     *                graphic_view_port(3),graphic_view_port(4))
         call pgsetwin( 0.0, 1.0, 0.0, 1.0 )
       endif

       end


