C
C
*+ ic2_end_alloc

       subroutine ic2_end_alloc(imap,map_array,status)
C      -----------------------------------------------
C
C End the allocation made to map IMAP
C
C Input:
C    Map entry
       integer       imap
C    Map data
       real*4        map_array(*)
C Returned
C    Status word
       integer       status
C
C End the allocation to map IMAP.  If the map was opened for write
C explicit IO is now performed.  This routine should always be called
C to end the access requested to a particular map whatever the requested
C access state.
*-
       call map_end_alloc(imap,map_array,status)
       call mapcat_err(status,'ic2_end_alloc','Fatal allocation error')
       end
