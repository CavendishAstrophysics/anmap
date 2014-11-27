C
C
*+ redt_setsr

       subroutine redt_setsr(source,program,status)
C      --------------------------------------------
C
C Define the current source name for the map file
C
C Given:
C   Source name
       character*(*)      source
C   Program name
       character*(*)      program
C Returned:
C    Status word
       integer            status
C
C Define the  source and program names for the current redtape
*-
       include 'mapcat_stack.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer           len_source, len_program

       if (status.ne.0) return

       len_source = chr_lenb(source)
       if (len_source.gt.0) then
         current_source = source
       end if
       len_program = chr_lenb(program)
       if (len_program.gt.0) then
         current_program = program
       end if
       call mapcat_err(status,'redt_setsr',' ')

       end
