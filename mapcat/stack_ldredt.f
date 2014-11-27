C
C
*+ stack_ldredt

       subroutine stack_ldredt(ipr,status)
C      -----------------------------------
C
C Load the standard common blocks with the redtape stored at IPR
C
C Given:
C   redtape address in redt_array
       integer     ipr
C Returned:
C   error return
       integer     status
C
*-
       include 'mapcat_stack.inc'

       call ldredt(redt_array(ipr),status)
       call mapcat_err(status,'stack_ldredt',' ')

       end
