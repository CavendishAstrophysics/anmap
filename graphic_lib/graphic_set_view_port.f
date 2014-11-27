*+ graphic_set_view_port

       subroutine graphic_set_view_port( defn, s )
C      -------------------------------------------
C
C Set the view-port for the supplied graphic structure
C
C Given:
C  the definition for this graphic
       integer   defn(*)
C
C Updated:
C  error return
       integer   s
C
C The view-port for the supplied graphic structure is initialised
C-


       include '../include/plt_basic_defn.inc'
       include '../include/plt_error_defn.inc'

C check status on entry
       if (s.ne.0) return

C copy information for this graphic structure
       call graphic_copy_graphic( defn, graphic, s )

C initialise view port
       call pgsetvp( graphic_view_port(1), graphic_view_port(2),
     *               graphic_view_port(3), graphic_view_port(4) )
       call pgsetwin( 0.0, 1.0, 0.0, 1.0 )

       call cmd_err(s, 'Graphic_set_view_port',
     *                 'Failed to initialise view-port')
       end
