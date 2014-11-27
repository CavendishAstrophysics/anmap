C
C
*+ mapcat_enqsr

       subroutine mapcat_enqsr(imap,source,program,status)
C      ---------------------------------------------------
C
C Enquire the source name for catalogue entry IMAP
C
C Input:
C    Map catalogue entry
       integer             imap
C Returned:
C    Source name for entry IMAP
       character*(*)       source
C    Program name
       character*(*)       program
C    Status
       integer             status
*-
       include 'mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       call mapcat_read(imap,status)
       source  = current_source
       program = current_program
       call mapcat_err(status,'mapcat_enqsr',' ')
       end
