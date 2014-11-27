C
C
*+ ic2_alloc_scr

       subroutine ic2_alloc_scr(sizeu,sizev,access,imap,ip_map,status)
C      ---------------------------------------------------------------
C
C Allocate scratch maps required in the remainder of a routine
C
C Given:
C   Size of scratch map in U and V
       integer        sizeu, sizev
C   Access mode required -- DIRECT
       integer        access
C Returned:
C   Map catalogue entry
       integer        imap
C   Map pointer
       integer        ip_map
C   Status word
       integer        status
C
C Scratch maps for a routine are allocated.
C
C Two access modes are supported in general:
C    SEQUENTIAL -- line by line; the entire image is not needed at once
C    DIRECT     -- full image in core
C HOWEVER, only DIRECT access is sensible for scratch files and
C SEQUENTIAL mode is therefore faulted.
C
C If DIRECT access is specified then the pointer to the start of the
C allocated space is returned in ip_map, if the access mode is
C SEQUENTIAL calls to IC2_ROW_READ and IC2_ROW_WRITE will be required
C nested in the sequential access loop.
C
C SIZEU, SIZEV are the sizes of the image in the U and V direction.  If
C either is zero then the values are obtained from the current redtape.
C
C On completion a call to IC2_END_ALLOC is required to tidy access state.
C
*-
       include 'ic_pars.inc'
       if (access.eq.icp_Direct) then
         call map_alloc_scr(sizeu,sizev,'DIRECT',imap,ip_map,status)
       else
         call map_alloc_scr(sizeu,sizev,'SEQUENTIAL',imap,ip_map,status)
       endif
999    call mapcat_err(status,'ic2_alloc_scr','Space allocation failed')

       end
