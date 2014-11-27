C
C
*+ ic2_alloc_toout

       subroutine ic2_alloc_toout(imap,access,map_array,
     *                             imapo,ip_mapo,status)
C      -------------------------------------------------
C
C Allocate map data IMAP to the new output map imapo
C
C Given:
C   Input map entry
       integer        imap
C   Access mode required -- DIRECT or SEQUENTIAL
       integer        access
C Updated:
C   Map data array
       real*4         map_array(*)
C Returned:
C   Map catalogue entry to output map
       integer        imapo
C   Map pointer to output map
       integer        ip_mapo
C   Status word
       integer        status
C
C Input map data is allocated to a new output map.
C
C The only access mode currently supported is:
C    DIRECT     -- full image in core
C
C The pointer to the start of the allocated space is returned in ip_mapo.
C The map data for map IMAP will have been read into this space.
C
C On completion a call to MAP_END_ALLOC is required to tidy the output
C and write out the image.
C
*-
       include 'ic_pars.inc'
       if (access.eq.icp_Direct) then
         call map_alloc_toout(imap,'DIRECT',map_array,
     *                        imapo,ip_mapo,status)
       else
         call map_alloc_toout(imap,'SEQUENTIAL',map_array,
     *                        imapo,ip_mapo,status)
       endif
999    call mapcat_err(status,'ic2_alloc_out','Space allocation failed')

       end
