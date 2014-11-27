*+ graphic_end

       subroutine graphic_end( type, defn, s )
C      ---------------------------------------
C
C Save the definition for the specified graphics type
C
C Given:
C  graphics type
       integer   type
C
C  the current definition for this graphic
       integer   defn(*)
C
C Updated:
C  error return
       integer   s
C
C The appropriate graphics definition of the specified type is saved if a
C previous call to graphic_init had loaded the definition from the definition
C array stack.
C
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
       if (type.ge.ig_type .and. ig.ne.0) then
         do i=1,len_struct
           graphic_array(i,ig) = defn(i)
         enddo
       endif
       end
