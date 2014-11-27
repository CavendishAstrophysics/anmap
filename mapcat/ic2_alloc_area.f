C
C
*+ ic2_alloc_area

       subroutine ic2_alloc_area(imap,uv_range,map_array,ip_map,status)
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
C then the action of the routine is idential to IC2_ALLOC_IN with
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
C if the whole map is read this routine is identical to IC2_ALLOC_IN
C with access='DIRECT'
*-
       include 'ic_pars.inc'
       call map_alloc_area(imap,uv_range,map_array,ip_map,status)
999    call mapcat_err(status,'ic2_alloc_area','Fatal allocation error')

       end
