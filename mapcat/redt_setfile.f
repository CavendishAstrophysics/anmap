C
C
*+ redt_setfile

       subroutine redt_setfile(file,status)
C      ------------------------------------
C
C Set the current value of the file name
C
C Given:
C    file name
       character*(*)   file
C Returned:
C    status word
       integer         status
C
C The current value of the map filename is reset
C-
       include 'mapcat_stack.inc'

       if (status.ne.0) return
       current_filename = file
       call mapcat_err(status,'redt_setfile',' ')
       end
