C
C
*+ do_scan_map

       subroutine do_scan_map(map_array,status)
C      ----------------------------------------
C
C Determine statistics within a specified portion of a map
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The statistics are returned for a specified region of the input map.
*-

C Local variables
       integer    minirt(8), iscan(4), iout,
     *            imap, ip_map, i
       real*4     rscan(4)

C check status on entry
       if (status.ne.0) return

C read map
       call map_getmap('Map : ','Default-Map','READ',imap,status)

C get map region
       call enminirt(minirt,status)
       call plot_getuv('UV-range : ','*',minirt,status)
       call map_alloc_area(imap,minirt,map_array,ip_map,status)

C get statistics
       call scnmap(map_array(ip_map),minirt,rscan,iscan,status)
       if (status.ne.0) goto 999

C write out results
       call io_enqout(iout)
       write(iout,10)imap,(minirt(i),i=1,4),
     *               rscan(1),iscan(1),iscan(2),
     *               rscan(2),iscan(3),iscan(4),rscan(3),rscan(4)
10     format('  Map=',i4,' UV=',4i6,/'  Max=',1pe12.3,' at ',2i6/
     *        '  Min=',1pe12.3,' at ',2i6/'  Mean=',1pe12.3,
     *        ' STD=',1pe12.3)

999    call map_end_alloc(imap,map_array,status)
       call cmd_err(status,'SCAN-MAP','Failed')

       end

