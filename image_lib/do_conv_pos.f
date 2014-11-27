C
C
*+ do_conv_pos

       subroutine do_conv_pos(map_array,status)
C      ----------------------------------------
C
C Map position to RA/DEC conversion and vice-versa
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C Convert from RA/DEC to UV position and vice-versa returning map value
C at the specified location
*-
       include '/mrao/include/chrlib_functions.inc'

C define local variables
       integer       imap, ip_map, iout
       real*8        ra_1950, dec_1950, ra_epoch, dec_epoch, u, v
       real*4        map_value

C projection information
       integer       iproj
       real*8        usamp, skew, epoch, prang

C define list of options
       integer       number_options
       parameter    (number_options = 2)
       character*80  list_options(number_options), option
       integer       len_option

       integer       hours, mins_time, degrees, mins_arc
       real*4        secs_time, secs_arc

       integer       uv_range(10)

       data          list_options(1) /
     * 'RADEC-to-UV .......................... convert RA,DEC to map UV'
     *                               /
       data          list_options(2) /
     * 'UV-to-RADEC .......................... convert map UV to RA,DEC'
     *                               /

C test status on entry
       if (status.ne.0) return

C read in option
       call io_enqout(iout)
       call io_getopt('Convert-Option (?=list) : ','RADEC-to-UV',
     *             list_options,number_options,option,status)
       len_option = chr_lenb(option)
       call map_getmap('Map-entry : ','Default-Map','READ',imap,status)
       call enmapj(iproj,usamp,skew,epoch,prang,status)
       call enminirt(uv_range,status)
       if (status.ne.0) goto 999

       if (chr_cmatch(option(1:len_option),list_options(1))) then
         call io_getra('RA  (1950.0) : ','12 0 0.0',ra_1950,status)
         call io_getdec('DEC (1950.0) : ','90 0 0.0',dec_1950,status)
         call precrd(1950.0D+0,ra_1950,dec_1950,
     *               epoch,ra_epoch,dec_epoch,status)
         call rdtouv(ra_epoch,dec_epoch,u,v,status)
         uv_range(3) = nint(v)+5
         uv_range(4) = nint(v)-5
         call map_alloc_area(imap,uv_range,map_array,ip_map,status)
         call ruvval(map_array(ip_map),u,v,1,map_value,status)
         if (status.ne.0) goto 999
         write(iout,10) u, v, map_value
 10      format(' .. map-position = ',2f8.1,'  value = ',1PE12.2)

       elseif (chr_cmatch(option(1:len_option),list_options(2))) then
         call io_getd('U-position : ','0.0',u,status)
         call io_getd('V-position : ','0.0',v,status)
         call uvtord(u,v,ra_epoch,dec_epoch,status)
         call precrd(
     *               epoch,ra_epoch,dec_epoch,
     *               1950.0D+0,ra_1950,dec_1950,
     *               status)
         call frrads('HMS',ra_1950,hours,mins_time,secs_time)
         call frrads('DMS',dec_1950,degrees,mins_arc,secs_arc)
         uv_range(3) = nint(v)+5
         uv_range(4) = nint(v)-5
         call map_alloc_area(imap,uv_range,map_array,ip_map,status)
         call ruvval(map_array(ip_map),u,v,1,map_value,status)
         write(iout,20) hours,mins_time,secs_time,
     *                  degrees,mins_arc,secs_arc,map_value
20       format(' .. sky-position (1950.0) = ',
     *          i2,1x,i2,1x,f6.2,i4,1x,i2,f6.2,
     *          '  value = ',1PE12.2)

       end if

999    call map_end_alloc(imap,map_array,status)
       call cmd_err(status,'CONVERT-POSITION',' ')

       end
