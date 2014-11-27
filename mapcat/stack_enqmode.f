C
C
*+ stack_enqmode

       subroutine stack_enqmode(imap,mode,ipm,ipr,iunit,status)
C      --------------------------------------------------------
C
C Return access mode for the specified map
C
C Input:
C    Map catalogue entry
       integer        imap
C Returned
C    Access mode
       integer        mode
C    Pointer to data
       integer        ipm
C    Pointer to redtape
       integer        ipr
C    Unit number if map is open for sequential access
       integer        iunit
C    Status
       integer        status
C
C The access mode and associated information is returned for map IMAP.
C If the map is not "open" for any access then an error is returned via
C the status word (ill_access).
*-
       include 'mapcat_stack.inc'
       include 'mapcat_errors.inc'

       integer   ib, ib0

C check status on entry
       if (status.ne.0) return

C find entry in active list of maps
       ib0 = -1
       do ib=1,max_active_maps
         if (active_list(1,ib).eq.imap) then
           ib0 = ib
         end if
       end do
       if (ib0.eq.-1) then
          status = ill_active
          goto 999
       end if

       mode = active_list(2,ib0)
       if (mode.eq.mode_sequential) then
         iunit = active_list(3,ib0)
         ipm = active_list(4,ib0)
         ipr = active_list(5,ib0)
       else
         ipm = stack_status(ip_pointer,active_list(6,ib0))
         ipr = active_list(5,ib0)
       end if

999    call mapcat_err(status,'stack_enqmode',' ')

       end
