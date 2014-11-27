C
C
*+ plot_get

       subroutine plot_get(map_array,status)
C      -------------------------------------
C
C Enquire the value of the named quantity and return as a parameter
C
C Given:
C   map-data array
       real*4           map_array(*)
C Returned:
C   error status return
       integer          status
C
C The value of the requested quantity is returned in the named command
C langauge parameter.  The options provided by this routine are concerned
C with access to information specific to the current plot.
C
C The following quantities may be enquired:
C   CURRENT-MAP       -      current map in plot frame
C   MAP-POSITION      -      prompt for a map-position optionally using cursor
C   UV-RANGE          -      prompt for a UV-range optionally using cursor
C   MAP-READ          -      read standard map parameters from plot
C   GRAPHIC-READ      -      read standard graphic parameters from plot
C   GRAPHIC-POSITION  -      cursor position on the graphics device
C   UV-FROM-RADEC     -      convert RA/DEC to UV
C   RADEC-FROM-UV     -      convert UV to RA/DEC
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'
C
C variables used locally
       character      quantity*30, value*80, number*4
       integer        len_q, len_v, uvrng(4), data_type
       real*4         ruv_pos(2)
       integer        i1, i2, iin, im, n, ip_map, minirt(8)
       real*8         ra_1950, dec_1950, ra_epoch, dec_epoch, u, v
       real*8         usamp, skew, epoch, prang
       integer        hours, mins_time, degrees, mins_arc
       real*4         secs_time, secs_arc
       integer        iproj

C define the quantites to be enquired
       integer        number_quantities
       parameter     (number_quantities = 8)
       character*60   quantities(number_quantities)
       data quantities(1)/'current-map ...... currently defined map'/
       data quantities(2)/'map-position ..... position on map'/
       data quantities(3)/'uv-range ......... UV-range on map'/
       data quantities(4)/'map-read ......... cursor input from map'/
       data quantities(5)/'graphic-read ..... cursor input from plot'/
       data quantities(6)/'graphic-position . position from plot'/
       data quantities(7)/'uv-from-radec .... convert RA/DEC to UV'/
       data quantities(8)/'radec-from-uv .... convert UV to RA/DEC'/


C check status on entry
       if (status.ne.0) return

C find quantity to enquire and parameter name for result
       call io_getopt('Get-option (?=list) : ','uv-range',quantities,
     *                8,quantity,status)
       len_q = chr_lenb(quantity)

       if (chr_cmatch(quantity(1:len_q),'map-read')) then
         call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
         call redt_load( imap, status )
         call plot_cursor_read( map_array(ip_map), .false., status )
         call map_end_alloc( imap, map_array, status )

       elseif (chr_cmatch(quantity(1:len_q),'map-position')) then
         call io_enqin(iin)
         call io_setin(terminal_in)
         call redt_load( imap, status )
         call enminirt( minirt, status )
         call plot_getpos2(minirt,'Map-position (UV) : ','*',
     *                            ruv_pos,status)
         call io_setin(iin)
         call plot_setpos_parms(ruv_pos,status)

       elseif (chr_cmatch(quantity(1:len_q),'graphic-position') .or.
     *         chr_cmatch(quantity(1:len_q),'graphic-read')) then
         call plot_cursor_get(ruv_pos,status)

       else if (chr_cmatch(quantity(1:len_q),'uv-range')) then
         call io_enqin(iin)
         call mapcat_getmap('Map-entry : ','Default-Map','READ',
     *                      im,status)
         call redt_load(im,status)
         call enminirt(minirt,status)
         call enredt(uvrng,data_type,status)
         call io_setin(terminal_io)
         call plot_getuv2(minirt,'UV-range : ','*',uvrng,status)
         call io_setin(iin)
         if (status.eq.0) then
           value = ' '
           do n=1,4
             call chr_chitoc(uvrng(n),number,i2)
             i1 = chr_intlc(number)
             len_v = chr_lenb(value)
             if (n.eq.1) then
               value = number(i1:i2)
             else
               value = value(1:len_v)//','//number(i1:i2)
             end if
           end do
         end if
         len_v = chr_lenb(value)
         call cmd_setlocal( '%uv-range', value(1:len_v), status )

       else if (chr_cmatch(quantity(1:len_q),'current-map')) then
         call chr_chitoc(imap,value,len_v)
         call cmd_setlocal( '%current-map', value(1:len_v), status )

       else if (chr_cmatch(quantity(1:len_q),'uv-from-radec')) then
         call redt_load(imap,status)
         call enmapj(iproj,usamp,skew,epoch,prang,status)
         call io_getra('RA  (1950.0) : ','12 0 0.0',ra_1950,status)
         call io_getdec('DEC (1950.0) : ','90 0 0.0',dec_1950,status)
         call precrd(1950.0D+0,ra_1950,dec_1950,
     *               epoch,ra_epoch,dec_epoch,status)
         call rdtouv(ra_epoch,dec_epoch,u,v,status)
         ruv_pos(1) = u
         ruv_pos(2) = v
         call plot_setpos_parms(ruv_pos,status)

       else if (chr_cmatch(quantity(1:len_q),'radec-from-uv')) then
         call redt_load(imap,status)
         call enmapj(iproj,usamp,skew,epoch,prang,status)
         call io_getd('U-position : ','0.0',u,status)
         call io_getd('V-position : ','0.0',v,status)
         call uvtord(u,v,ra_epoch,dec_epoch,status)
         call precrd(
     *               epoch,ra_epoch,dec_epoch,
     *               1950.0D+0,ra_1950,dec_1950,
     *               status)
         call frrads('HMS',ra_1950,hours,mins_time,secs_time)
         call frrads('DMS',dec_1950,degrees,mins_arc,secs_arc)
         value = ' '
         write(value,'(i2,1x,i2,1x,f6.2)') hours,mins_time,secs_time
         len_v = chr_lenb(value)
         call cmd_setlocal( '%ra', value(1:len_v), status )
         value = ' '
         write(value,'(i4,1x,i2,1x,f6.2)') degrees,mins_arc,secs_arc
         len_v = chr_lenb(value)
         call cmd_setlocal( '%dec', value(1:len_v), status )

       else
         call cmd_wrerr('MAP-DISPLAY GET','Unknown option to GET')
         goto 999
       end if

999    call cmd_err(status,'plot_get',' ')

       end


