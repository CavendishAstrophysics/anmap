C
C
*+ ic2_alloc_in

       subroutine ic2_alloc_in(imap,access,map_array,ip_map,status)
C      ------------------------------------------------------------
C
C Allocate a map for input
C
C Input:
C    Map to allocate for input
       integer           imap
C    Access mode -- icp_DIRECT or icp_SEQUENTIAL
       integer           access
C Updated:
C    Map data
       real*4            map_array(*)
C Returned:
C    Pointer to map_array and start of IMAP data
       integer           ip_map
C    Status
       integer           status
C
C The map IMAP is allocated for READ.  If the access requested is DIRECT
C then space is allocated in CORE and the map is read in (if not already)
C in core.  If the acces requested is SEQUENTIAL then no direct access to
C the map data is performed, but an allocation is made for subsequent
C calls to IC2_ROW_READ and IC2_ROW_WRITE.
C
*-
       include 'ic_pars.inc'
       if (access.eq.icp_Direct) then
         call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       else
         call map_alloc_in(imap,'SEQUENTIAL',map_array,ip_map,status)
       endif
999    call mapcat_err(status,'ic2_alloc_in','Fatal allocation error')

       end
