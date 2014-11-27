C
C
*+ do_loc_max

       subroutine do_loc_max(map_array,status)
C      ---------------------------------------
C
C Determine local interpolated maximum and flux
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C Find the local maximum on a map within a specified UV search window.
C The maximim returned is found at a fractionl pixel location -- the
C position and map value are returned for the maximum and the centre
C of the maximum pixel.
*-

C projection information
       integer       iproj, tscope
       real*8        usamp, skew, epoch, prang
       real*8        rapnt, decpnt
       real*4        freq

C define local variables
       integer   imap, ip_map, iout, i, uv_range(4), mini_redtape(10)
       real*8    ra_1950, dec_1950, ra_epoch, dec_epoch, uv(2)
       real*4    map_value, pb_corr, pb_value
       integer   iscan_max(10)
       real*4    scan_max(10)

       integer   hours, mins_time, degrees, mins_arc
       real*4    secs_time, secs_arc

C test status on entry
       if (status.ne.0) return

C request map on which to work
       call map_getmap('Map-entry : ','Default-Map','READ',imap,status)
       call enmapj(iproj,usamp,skew,epoch,prang,status)
       call entpnt(tscope,freq,rapnt,decpnt,status)
       if (status.ne.0) goto 999

C read UV range:
       call enminirt(mini_redtape,status)
       do i=1,4
         uv_range(i) = mini_redtape(i)
       end do
       call plot_getuv('UV-range to search : ','*',uv_range,status)
       call map_alloc_area(imap,uv_range,map_array,ip_map,status)
       call redt_load( imap, status )

C find maximum pixel then interpolated maximum
       call scnmap2(mini_redtape, map_array(ip_map), uv_range,
     *              scan_max, iscan_max, status )
       call ruvmax2(mini_redtape, map_array(ip_map), iscan_max,
     *              uv, map_value, status )

C convert pixel position to RA/DEC and report results
       call uvtord(uv(1),uv(2),ra_epoch,dec_epoch,status)
C .. find PB value at this location
       call pbcorr(ra_epoch,dec_epoch,rapnt,decpnt,tscope,
     *             pb_value,status)
       call precrd(
     *             epoch,ra_epoch,dec_epoch,
     *             1950.0D+0,ra_1950,dec_1950,
     *             status)
       call frrads('HMS',ra_1950,hours,mins_time,secs_time)
       call frrads('DMS',dec_1950,degrees,mins_arc,secs_arc)
       call io_enqout( iout )
       write(iout,20) hours,mins_time,secs_time,
     *                degrees,mins_arc,secs_arc,map_value
20     format(' .. Maximum (1950.0) = ',
     *        i2,1x,i2,1x,f6.2,i4,1x,i2,f6.2,
     *        '  value = ',1PE12.2)
       if (pb_value.gt.0.0) then
         pb_corr = map_value/pb_value
       else
         pb_corr = 0.0
       end if
       write(iout,25) pb_corr, pb_value
25     format(' .. PB corrected value = ',1PE12.2,'  PB = ',1PE12.2)


999    call map_end_alloc(imap,map_array,status)
       call cmd_err(status,'LOCAL-MAXIMUM',' ')

       end
