*+ graphic_show_object

       subroutine graphic_show_object( obj, s )
C      ----------------------------------------
C
C Display information for the specified object
C
C Given:
C  object definition
       integer    obj(*)
C
C Updated:
C  error status
       integer    s
C
C Information about the supplied object is written to the output.
C
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer    iout

C check status on entry
       if (s.ne.0) return

C find output unit
       call io_enqout( iout )

C copy object object to standard definition
       call graphic_copy_object (obj, object, s )

C output information to unit iout
       write(iout,10) object_status, object_name(object_type),
     *                object_dim
10     format(' OBJECT: Status=',I2,' Type=',A/
     *        '         Dimensions=',6(1PE9.1))

       end
