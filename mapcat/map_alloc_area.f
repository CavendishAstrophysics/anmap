C
C
*+ map_alloc_area

       subroutine map_alloc_area(imap,uv_range,map_array,ip_map,status)
C      ----------------------------------------------------------------
C
C Allocate a region of a map for input
C
C Input:
C    Map to allocate for input
       integer           imap
C    Range of map to read
       integer           uv_range(4)
C Updated:
C    Map data
       real*4            map_array(*)
C Returned:
C    Pointer to map_array and start of IMAP data
       integer           ip_map
C    Status
       integer           status
C
C The map IMAP is allocated for READ.  Only the specified region of the
C map is read into core.  If the region corresponds to the whole map
C then the action of the routine is idential to MAP_ALLOC_IN with
C access = 'DIRECT'.  The pointer to the map data is returned in
C IP_MAP -- this pointer is to the start of the COMPLETE map although
C only the specified region will contain sensible data.
C
C The following rules are used to determine the action of this
C routine:
C    1) only the V bounds of UV_RANGE are important as the structure
C       of the data files insists complete rows are read
C    2) if UV_RANGE exceeds the range of the map the whole map is read
C    3) if the region exceeds AREA_MAX_READ then the whole map is read
C if the whole map is read this routine is identical to MAP_ALLOC_IN
C with access='DIRECT'
*-
C Local Variables:
C stack entry for map and redtape and file unit number
       integer    is_map, iunit, ipm
C result of stack enquiry routines
       integer    data_in_core
C mini redtape
       integer    minirt(8)
C variable to test area to read
       real*4     test_area

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
       call enminirt(minirt,status)
       test_area = abs( float(uv_range(4)-uv_range(3)) /
     *                  float(minirt(4)-minirt(3))     )
       if (area_max_read.le.0.000001) area_max_read = 0.5
       if (
     *       (uv_range(3).ge.minirt(3) .and.
     *        uv_range(4).le.minirt(4)     ) .or.
     *       (uv_range(3).gt.minirt(3)     ) .or.
     *       (uv_range(4).lt.minirt(4)     ) .or.
     *       (test_area.gt.area_max_read   )    ) then
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
       else
         call stack_access(imap,'READ','MAP',
     *                     current_minirt(5)*current_minirt(6),
     *                     is_map,status)
         call stack_enqdat(is_map,data_in_core,status)
         call stack_enqpnt(is_map,ip_map,status)
C .. read data into core if required
         if (data_in_core.eq.false) then
           call mapcat_mapopen(imap,'READ',iunit,status)
           ipm = ip_map + (minirt(3)-uv_range(3))*minirt(5)
           minirt(3) = uv_range(3)
           minirt(4) = uv_range(4)
           call rdarea(iunit,minirt,
     *                 map_array(ipm),status)
           call mapcat_mapclose(imap,status)
         else
           call stack_setdat(is_map,true,status)
         end if
       end if

C report any errors
999    call mapcat_err(status,'map_alloc_area','Fatal allocation error')

       end
