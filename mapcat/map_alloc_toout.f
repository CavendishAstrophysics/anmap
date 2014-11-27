C
C
*+ map_alloc_toout

       subroutine map_alloc_toout(imap,access,map_array,
     *                             imapo,ip_mapo,status)
C      -------------------------------------------------
C
C Allocate map data IMAP to the new output map imapo
C
C Given:
C   Input map entry
       integer        imap
C   Access mode required -- DIRECT or SEQUENTIAL
       character*(*)  access
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
C PA, 1/8/90
*-
       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer     minirt(10), iunit
       integer     is_mapi, is_mapo, ip_mapi

C check status on entry
       if (status.ne.0) return

C check existing catalogue entry
       call mapcat_chk(imap,'NONE',status)
       if (status.ne.0) goto 999

C if no access state previously defined then make sure map available
C for read
       call mapcat_read(imap,status)
       if (current_map_status(ip_access).eq.access_clear) then
         call mapcat_chk(imap,'READ',status)
       end if

C find if map is in core alread
       call stack_enqmap(imap,is_mapi,status)
       ip_mapi = -1
       if (is_mapi.gt.0) then
         call stack_enqpnt(is_mapi,ip_mapi,status)
       end if

C find suitable output map and allocate
       call redt_load(imap,status)
       if (status.ne.0) goto 999
       call mapcat_next(imapo,status)
       call mapcat_acc(imapo,'WRITE',status)
       if (status.ne.0) then
         call mapcat_err(status,'map_alloc_out',
     *                   'Allocation of output maps failed')
         goto 999
       end if

C set size
       call enminiRT(minirt,status)
C create output map
       call redt_setfile('PAGEING-FILE',status)
       minirt(7) = 3
       call redt_setrt(minirt,status)
       call redt_dump(imapo,status)

C allocate space as required:
       if (chr_cmatch(access,'DIRECT') .or.
     *     chr_cmatch(access,'SEQUENTIAL')) then

         if (is_mapi.lt.0) then
C .. IMAP data not in core allocate space and read data
           call stack_access(imapo,'WRITE','MAP',
     *                       minirt(5)*minirt(6),is_mapo,status)
           call stack_enqpnt(is_mapo,ip_mapo,status)
           call mapcat_mapopen(imap,'READ',iunit,status)
           call rdmap(iunit,map_array(ip_mapo),status)
           call mapcat_mapclose(imap,status)

         else
C .. IMAP data in core -- reallocate to imapo
           call stack_access(imapo,'ALLOCATE','MAP',imap,
     *                       is_mapo,status)
           call stack_enqpnt(is_mapo,ip_mapo,status)

         end if
       else
         status = ill_unmode
         goto 999
       end if

999    call mapcat_err(status,'map_alloc_out','Space allocation failed')

       end
