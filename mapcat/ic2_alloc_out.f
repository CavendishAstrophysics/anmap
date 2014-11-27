C
C
*+ ic2_alloc_out

       subroutine ic2_alloc_out(usize,vsize,access,imap,ip_map,status)
C      ---------------------------------------------------------------
C
C Allocate output maps required in the remainder of a routine
C
C Given:
C   Size of output map in U and V
       integer        usize, vsize
C   Access mode required -- DIRECT or SEQUENTIAL
       integer        access
C Returned:
C   Map catalogue entry
       integer        imap
C   Map pointer
       integer        ip_map
C   Status word
       integer        status
C
C Output maps for a routine are allocated.
C
C Two access modes are supported:
C    SEQUENTIAL -- line by line; the entire image is not needed at once
C    DIRECT     -- full image in core
C
C If DIRECT access is specified then the pointer to the start of the
C allocated space is returned in ip_map, if the access mode is
C SEQUENTIAL calls to IC2_ROW_READ and IC2_ROW_WRITE will be required
C nested in the sequential access loop.
C
C SIZEU, SIZEV are the sizes of the image in the U and V direction.  If
C either is zero then the values are obtained from the current redtape.
C
C On completion a call to IC2_END_ALLOC is required to tidy the output
C and indeed write out the image if the access requested is DIRECT.
C
*-
       include 'ic_pars.inc'
       if (access.eq.icp_Direct) then
         call map_alloc_out(usize,vsize,'DIRECT',imap,ip_map,status)
       else
         call map_alloc_out(usize,vsize,'SEQUENTIAL',imap,ip_map,status)
       endif
999    call mapcat_err(status,'ic2_alloc_out','Space allocation failed')

       end


