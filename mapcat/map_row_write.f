C
C
*+ map_row_write

       subroutine map_row_write(imap,iv,map_array,ip_map,status)
C      ---------------------------------------------------------
C
C Save data during sequential access
C
C Given:
C   map catalogue entry
       integer    imap
C   row number
       integer    iv
C   map array data array
       real*4     map_array(*)
C   pointer to row in map_array
       integer    ip_map
C Returned:
C   status word
       integer    status
C
C Perform sequential access for map imap.  The row at map_array(ip_map)
C is written to the output file.  A call to MAP_ALLOC_OUT is required
C before this routine can be used.  The max and min are maintained
C for this map.
C-

C local variables defining map access mode
       integer          ipm, ipr, mode, iunit
C mini redtape
       integer          minirt(8)
C loop counter
       integer          i
C maxmin values
       integer          izmnx(4)
       real*4           zmnx(4)

       include 'mapcat_pars.inc'

C check status on entry
       if (status.ne.0) return

C find access mode to this file
       call stack_enqmode(imap,mode,ipm,ipr,iunit,status)
       if (status.ne.0) goto 999

C decode mode and take appropriate action
       call stack_ldredt(ipr,status)
       call enminirt(minirt,status)
       zmnx(1) = -1.0E+30
       zmnx(2) =  1.0E+30
       do i=0,minirt(5)-1
         if (map_array(ip_map+i).gt.zmnx(1)) then
           zmnx(1) = map_array(ip_map+i)
           izmnx(1)= i+minirt(1)
           izmnx(2)= iv
         end if
         if (map_array(ip_map+i).lt.zmnx(2)) then
           zmnx(2) = map_array(ip_map+i)
           izmnx(3)= i+minirt(1)
           izmnx(4)= iv
         end if
       end do
       call stack_setmnx(imap,zmnx,izmnx,status)
       if (mode.eq.mode_sequential) then
          call wrrow(iunit,iv,map_array(ip_map),status)
       else
          ip_map = ip_map + minirt(5)
       end if

999    call mapcat_err(status,'map_row_write',' ')

       end
