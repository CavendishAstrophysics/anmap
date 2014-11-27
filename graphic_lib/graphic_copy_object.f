*+ graphic_copy_object

       subroutine graphic_copy_object( object_from, object_to, s )
C      -----------------------------------------------------------
C
C Copy the object structure from object_from to object_to
C
C Given:
C   structure to copy object from
       integer    object_from(*)
C Returned:
C   structure to copy graphic to
       integer    object_to(*)
C
C Updated:
C   error status
       integer    s
C
C Copy the object structure between the two arrays used to hold the
C structure.
C-

       include '../include/plt_basic_defn.inc'

C local variables
       integer     l

C check status on entry
       if (s.ne.0) return
       do l=1,len_object
          object_to(l) = object_from(l)
       enddo

       end

