C
C
*+ plot_cursor_read

       subroutine plot_cursor_read(map_array,report,s)
C      -----------------------------------------------
C
C Read cursor position and return UV, RA/DEC and value
C
C Given:
C  map array
       real*4    map_array(*)
C  flag requesting results are summarized on output
       logical   report
C
C Updated:
C  error status
       integer   s
C
C This is a high level routine and may be called at any time. The routine
C checks that a valid plot is open. The output consists of:
C
C    the nearest UV pixel and value %u, %v, %uv-pos, %uv-value
C    the interpolated position and value %map-u, %map-v, %map-pos, %map-value
C    primary-beam corrected values %map-pbvalue
C    RA and DEC at cursor location %ra, %dec
C
C These data are returned via standard named parameters and if report is
C set to true then they are also reported on the output unit.
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '/mrao/include/constants.inc'
       include '/mrao/include/chrlib_functions.inc'

C graphic position
       real*4       gx, gy
       character    ch_typed*1, rnumber*20, value*80
       integer      i1, i2, len_v

C cursor position in map coordinates
       real*4       pos_u, pos_v
       real*8       dpos_u, dpos_v

C value at nearest pixel, interpolated value, and primary-beam corrected
       real*4       value_int, value_pix, pb_value
       character    string_int*14, string_pix*14, string_pb*14
       integer      len_val_int, len_val_pix, len_val_pb

C RA/DEC of current position
       real*8       ra, dec, ra0, dec0, epoch, rapnt, decpnt
       real*4       freq
       integer      tscope

C parameters to hold RA/DEC position
       integer      i_hour, i_min_time, i_deg, i_min_arc
       real*4       sec_time, sec_arc

C functions and flags
       integer      pgcurse, iout
       logical      cursor_available, cmd_dblev

       if (s.ne.0) return

       if (.not.plot_open) then
         call cmd_wrerr('CURSOR-POSITION',
     *                  'Plot not open - unable to access cursor')
         return
       end if
       if (.not.frame_init) then
         call cmd_wrerr('CURSOR-POSITION',
     *                  'Frame not initialised for image')
         return
       end if
       call pgqcur(cursor_available)
       if (.not.cursor_available) then
         call cmd_wrerr('CURSOR-POSITION',
     *                   'Device does not support input')
         return
       end if

C find initial position for cursor
       if (cmd_dblev(1)) then
         call io_wrout('.. move cursor: type any character to end')
       endif

       pos_u = (frame_win(1)+frame_win(2))/2.0
       pos_v = (frame_win(3)+frame_win(4))/2.0

C read cursor position
       s = pgcurse(pos_u,pos_v,ch_typed) - 1
       if (s.ne.0) then
         call cmd_wrerr('CURSOR-POSITION','Error reading CURSOR')
         return
       end if

C set value for graphic position and character
       gx = (pos_u-frame_win(1))*(frame_vp(2)-frame_vp(1))/
     *      (frame_win(2)-frame_win(1)) + frame_vp(1)
       gy = (pos_v-frame_win(3))*(frame_vp(4)-frame_vp(3))/
     *      (frame_win(4)-frame_win(3)) + frame_vp(3)
       call cmd_setlocal( '%gr-char', ch_typed, s )
       call chr_chrtoc(gx,rnumber,i2)
       i1 = chr_intlc(rnumber)
       call cmd_setlocal( '%gr-x', rnumber(i1:i2), s )
       call chr_chrtoc(gy,rnumber,i2)
       i1 = chr_intlc(rnumber)
       call cmd_setlocal( '%gr-y', rnumber(i1:i2), s )

C determine map value at cursor position and validity
       dpos_u = pos_u
       dpos_v = pos_v
       call ruvval(map_array,dpos_u,dpos_v,2,value_int,s)
       s = 0
       call iuvval(map_array,nint(pos_u),nint(pos_v),value_pix,s)
       call chr_chrtoc(value_int,string_int,len_val_int)
       call chr_chrtoc(value_pix,string_pix,len_val_pix)
       call cmd_setlocal('%map-value',string_int(1:len_val_pix),s)
       call cmd_setlocal('%uv-value',string_pix(1:len_val_pix),s)
       call chr_chitoc(nint(pos_u),rnumber,i2)
       i1 = chr_intlc(rnumber)
       call cmd_setlocal('%u',rnumber(i1:i2),s)
       value = rnumber(i1:i2)
       call chr_chitoc(nint(pos_v),rnumber,i2)
       i1 = chr_intlc(rnumber)
       call cmd_setlocal('%v',rnumber(i1:i2),s)
       len_v = chr_lenb(value)
       value = value(1:len_v)//','//rnumber(i1:i2)
       len_v = chr_lenb(value)
       call cmd_setlocal( '%uv-pos', value(1:len_v), s )

       call chr_chrtoc(pos_u,rnumber,i2)
       i1 = chr_intlc(rnumber)
       call cmd_setlocal('%map-u',rnumber(i1:i2),s)
       value = rnumber(i1:i2)
       call chr_chrtoc(pos_v,rnumber,i2)
       i1 = chr_intlc(rnumber)
       call cmd_setlocal('%map-v',rnumber(i1:i2),s)
       len_v = chr_lenb(value)
       value = value(1:len_v)//','//rnumber(i1:i2)
       len_v = chr_lenb(value)
       call cmd_setlocal( '%map-pos', value(1:len_v), s )

C output results if reporting
       if (report) then
         call io_enqout(iout)
         write(iout,10) nint(pos_u),nint(pos_v),value_pix
         write(iout,11) pos_u,pos_v,value_int
10       format(' Map pixel    : ',2I6,12X,'  Pixel-value : ',1PE12.3)
11       format(' Map position : ',2F12.3, '  Map-value   : ',1PE12.3)
       endif

C return if point not on map
       if (s.ne.0) then
         s = 0
         return
       endif

C find primary beam corrected value
       call entpnt(tscope, freq, rapnt, decpnt, s)
       call enepoch( epoch, s )
       call uvtord(dpos_u,dpos_v,ra0,dec0,s)
       call precrd(epoch,ra0,dec0,1950.0d0,ra,dec)
       call pbcorr(ra0,dec0,rapnt,decpnt,tscope,pb_value,s)
       if (pb_value.gt.0.0) then
         value_int = value_int/pb_value
         call chr_chrtoc(value_int,string_pb,len_val_pb)
       else
         call chr_chrtoc(0.0,string_pb,len_val_pb)
       end if
       call cmd_setlocal('%map-pbvalue',string_pb(1:len_val_pb),s)
       if (report) then
         write(iout,12)value_int
12       format('                ',24X, '  PB-value    : ',1PE12.3)

       endif

C find RA/DEC
       call frrads('HMS',ra,i_hour,i_min_time,sec_time)
       call frrads('DMS',dec,i_deg,i_min_arc,sec_arc)
       value = ' '
       write(value,1) i_hour,i_min_time,sec_time
       len_v = chr_lenb(value)
       call cmd_setlocal( '%RA', value(1:len_v), s )
 1     format(I3,1X,I2,1X,F7.3)
       value = ' '
       write(value,2) i_deg,i_min_arc,sec_arc
       len_v = chr_lenb(value)
       call cmd_setlocal( '%DEC', value(1:len_v), s )
 2     format(I3,1X,I2,1X,F7.2)

       if (report) then
         write(iout,13) i_hour,i_min_time,sec_time,
     *                  i_deg,i_min_arc,sec_arc
13       format(' RA DEC       : ',I4,1X,I2,1X,F7.3,I4,1X,I2,1X,F7.3)
       endif

       call cmd_err( s,'PLOT_CURSOR_READ',' ')

       end

