C
C
*+ redt_enqcurr

       subroutine redt_enqcurr(ic_redt,status)
C      ---------------------------------------
C
C Return the current redtape
C
C Returned:
C    catalogue entry of the current redtape
        integer   ic_redt
C    status word
        integer   status
C
C The map catalogue entry of the redtape currenty loaded in the standard
C redtape buffer is returned.  The standard redtape buffer may be used
C by including the maplib_redtape.inc include file
C
C-
       include 'mapcat_stack.inc'

       if (status.ne.0) return
       ic_redt = current_redtape
       call mapcat_err(status,'redt_enqcurr',' ')
       end
