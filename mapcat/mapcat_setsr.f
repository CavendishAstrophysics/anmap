C
C
*+ mapcat_setsr

       subroutine mapcat_setsr(imap,source,program,status)
C      ---------------------------------------------------
C
C Set the source name for catalogue entry imap
C
C Input:
C    Map catalogue entry
       integer             imap
C    Source for entry IMAP
       character*(*)       source
C    Program for entry IMAP
       character*(*)       program
C Returned:
C    Status
       integer             status
*-
       include 'mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       current_source  = source
       call chr_chucas(current_source)
       current_program = program
       call chr_chucas(current_program)
       call mapcat_write(imap,status)
       call mapcat_err(status,'mapcat_setsr',' ')
       end
