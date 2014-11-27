C
C
*+ mapcat_enqrec

       subroutine mapcat_enqrec(imap,record,status)
C      --------------------------------------------
C
C Enquire the current record definition
C
C Input:
C    Map catalogue entry
       integer             imap
C Returned:
C    The current catalogue record definition
       integer             record(*)
C    Status
       integer             status
*-
       integer   i
       include 'mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_read(imap,status)
       do i=1,cat_blocksize/4
          record(i) = current_record(i)
       end do
       call mapcat_err(status,'mapcat_enqrec',' ')
       end
