C
C
*+ mapcat_delete

       subroutine mapcat_delete(imap,prompt_state,status)
C      --------------------------------------------------
C
C Delete a map referenced in the catalogue (including file)
C
C Input:
C    Map entry
       integer       imap
C    Flag controlling prompting
       logical       prompt_state
C Returned:
C    Status
       integer       status
*-
       include 'mapcat_pars.inc'
       include '/mrao/include/iolib_constants.inc'

       character*(iolen_file)    filename
       integer                   minirt(10), iprompt, map_status(10)

       if (status.ne.0) return
       if (prompt_state) then
         iprompt = 2
       else
         iprompt = 0
       end if
       call mapcat_enqrt(imap,filename,minirt,status)
       call io_delfil(filename,iprompt,status)
       if (status.eq.0) then
         call mapcat_enqst(imap,map_status,status)
         map_status(ip_access) = access_clear
         map_status(ip_data)   = false
         map_status(ip_open)   = false
         map_status(ip_unit)   = 0
         map_status(ip_page)   = false
         call mapcat_setst(imap,map_status,status)
         call mapcat_err(status,'mapcat_delete',' ')
       else
         status = 0
       endif
       end

