C
C
*+ redt_setxrdt

       subroutine redt_setxrdt( status )
C      ---------------------------------
C
C Returned:
C   error return code
       integer    status
C
C Request next output map will be created with a page of extra redtape
C-
C integer flag indicating output map should have extra redtape
       integer                    extra_redtape_on_output
       common /mapcat_local_redt/ extra_redtape_on_output
       extra_redtape_on_output = 1
       end
