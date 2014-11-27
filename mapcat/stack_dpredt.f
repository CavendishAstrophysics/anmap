C
C
*+ stack_dpredt

       subroutine stack_dpredt(ipr,status)
C      -----------------------------------
C
C Sump the standard common blocks into the redtape array stored at IPR
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

       call dpredt(redt_array(ipr),status)
       call mapcat_err(status,'stack_ldredt',' ')

       end
