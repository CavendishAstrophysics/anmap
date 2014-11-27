C
C
*+ map_row_read

       subroutine map_row_read(imap,iv,map_array,ip_map,status)
C      --------------------------------------------------------
C
C Find pointer to map data during sequential access
C
C Given:
C   map catalogue entry
       integer    imap
C   row number
       integer    iv
C Updated:
C   map array data array
       real*4     map_array(*)
C Returned:
C   pointer to row in map_array
       integer    ip_map
C   status word
       integer    status
C
C Perform sequential access for map imap.  If the map is already in
C the stack then no access to the disc data file is required and a
C pointer to the start of the requested row is returned.  If the map
C is not in core then the data is read from the disc file.  A previous
C call to MAP_ALLOC_IN is required to setup the access state to the
C data file.
C
C-
       include '/mrao/include/iolib_constants.inc'
       include 'mapcat_pars.inc'

C local variables defining map access mode
       integer          ipm, ipr, mode, iunit
C mini redtape
       integer          minirt(8)

C check status on entry
       if (status.ne.0) return

C find access mode to this file
       call stack_enqmode(imap,mode,ipm,ipr,iunit,status)
       call stack_ldredt(ipr,status)
       if (status.ne.0) goto 999

C decode mode and take appropriate action
       if (mode.eq.mode_direct) then
*         print *,'..DIRECT iv = ',iv
         call enminirt(minirt,status)
         ip_map = ipm+(minirt(3)-iv)*minirt(5)
*         print *,'.. ipm/ip_map = ',ipm,ip_map

       else if (mode.eq.mode_sequential) then
         print *,'.. ERROR iv = ',iv
         ip_map=ipm
*         call rdrow(iunit,iv,map_array(ip_map),status)

       end if

999    call mapcat_err(status,'map_row_read','Data not read')

       end
