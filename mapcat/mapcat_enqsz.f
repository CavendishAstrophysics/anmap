C
C
*+ mapcat_enqsz

       subroutine mapcat_enqsz(number_cat_entries,status)
C      --------------------------------------------------
C
C Enquire the maximum number of catalogue entries possible
C
C Returned:
C    Number of catalogue entries
       integer      number_cat_entries
C    Status
       integer      status
*-
       include 'mapcat_cat.inc'
       if (status.ne.0) return
       number_cat_entries = max_cat_entries
       end
