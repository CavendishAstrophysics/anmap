C
C
*+ map_alloc_in

       subroutine map_alloc_in(imap,access,map_array,ip_map,status)
C      ------------------------------------------------------------
C
C Allocate a map for input
C
C Input:
C    Map to allocate for input
       integer           imap
C    Access mode -- DIRECT or SEQUENTIAL
       character*(*)     access
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
C the map data is performed, but an allocation is made for supsequent
C calls to MAP_ROW_READ and MAP_ROW_WRITE.
C
*-
C Local Variables
C stack entry for map and redtape and file unit number
       integer    is_map, iunit, mode, ipm, ipr
C result of stack enquiry routines
       integer    data_in_core

       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

C check status on entry
       if (status.ne.0) return

C check catalogue entry
       call mapcat_chk(imap,'NONE',status)
       if (status.ne.0) goto 999

C if no access state previously defined then define it
       call mapcat_read(imap,status)
       if (current_map_status(ip_access).eq.access_clear) then
         call mapcat_chk(imap,'READ',status)
         call mapcat_acc(imap,'READ',status)
       end if

C allocate space for this map
       call redt_load(imap,status)
       if (chr_cmatch(access,'DIRECT').or.
     *     chr_cmatch(access,'SEQUENTIAL')) then
         call stack_access(imap,'READ','MAP',
     *                     current_minirt(5)*current_minirt(6),
     *                     is_map,status)
         call stack_enqdat(is_map,data_in_core,status)
         call stack_enqpnt(is_map,ip_map,status)
C .. read data into core if required
         if (data_in_core.eq.false) then
           call mapcat_mapopen(imap,'READ',iunit,status)
           call rdmap(iunit,map_array(ip_map),status)
           call mapcat_mapclose(imap,status)
         end if
         call stack_setdat(is_map,true,status)
         call stack_enqmode(imap,mode,ipm,ipr,iunit,status)
         call stack_dpredt(ipr,status)
       else
         status = ill_mode
       end if

C report any errors
999    call mapcat_err(status,'map_alloc_in','Fatal allocation error')

       end
