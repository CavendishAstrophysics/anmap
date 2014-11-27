*+ graphic_vport

       subroutine graphic_vport( gr, vp, s )
C      -------------------------------------
C
C Define a view port within the view-port of the current graphic
C
C Given:
C   Current graphic definition
       integer   gr(*)
C   View-port, normalised device coordinates
       real*4    vp(*)
C
C Updated:
C   error status
       integer   s
C
C A view-port is setup relative to the graphic view port, the view
C port must be specified in normalized device coordinates.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       real*4   xr, yr, x1, x2, y1, y2

C check status on entry
       if (s.ne.0) return

       call graphic_copy_graphic( gr, graphic, s )

C setup a scaled view port
       xr = graphic_view_port(2) - graphic_view_port(1)
       yr = graphic_view_port(4) - graphic_view_port(3)
       x1 = graphic_view_port(1) + vp(1)*xr
       x2 = graphic_view_port(1) + vp(2)*xr
       y1 = graphic_view_port(3) + vp(3)*yr
       y2 = graphic_view_port(3) + vp(4)*yr

C define this view port to the graphics package
       call pgsetvp( x1, x2, y1, y2 )
       call cmd_err(s,'graphic_view_port',' ')

       end
