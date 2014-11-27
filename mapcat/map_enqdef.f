
C
C
*$ 2) High-Level Map System Routines Access Catalogue, Stack and Data
*  ------------------------------------------------------------------
C
C
*+ map_enqdef

       subroutine map_enqdef(imap,status)
C      ----------------------------------
C
C Enquire the default map
C
C Returned:
C   Default map
       integer         imap
C   Status word
       integer         status
C
C The current value of the default map is returned
C
*-
       include 'mapcat_cat.inc'
C
       if (status.ne.0) return
       imap =  default_map
       call mapcat_err(status,'map_enqdef',' ')
       end
