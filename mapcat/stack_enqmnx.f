C
C
*+ stack_enqmnx

       subroutine stack_enqmnx(imap,zmnx,izmnx,status)
C      -----------------------------------------------
C
C Enquire the statistics for active map IMAP
C
C Input:
C    Map catalogue entry
       integer        imap
C Returned:
C    Real data (max, min)
       real*4         zmnx(*)
C    Integer data (max position, min position)
       integer        izmnx(*)
C    Status
       integer        status
C
C The statistics are returned for map IMAP.
C If the map is not "open" for any access then an error is returned via
C the status word (ill_active).
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

       zmnx(1)  = active_zmnx(1,ib0)
       izmnx(1) = active_izmnx(1,ib0)
       izmnx(2) = active_izmnx(2,ib0)
       zmnx(2)  = active_zmnx(2,ib0)
       izmnx(3) = active_izmnx(3,ib0)
       izmnx(4) = active_izmnx(4,ib0)

999    call mapcat_err(status,'stack_enqmnx',' ')

       end
