C
C
*+ plot_cursor_get

       subroutine plot_cursor_get(uv,s)
C      --------------------------------
C
C Read cursor position and return it
C
C Returned:
C  cursor position -- map coordinates
       real*4    uv(2)
C
C Updated:
C  error status
       integer   s
C
C A position is read from the current graphics map display and returned
C in uv.  The standard parameters %map-u, %map-v %map-pos, %u, %v,
C %uv-pos and %gr-char are also set.
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '/mrao/include/constants.inc'
       include '/mrao/include/chrlib_functions.inc'

C graphic position
       real*4       gx, gy
       character    ch_typed*1, rnumber*20, value
       integer      i1, i2, len_v

C cursor position in map coordinates
       real*4       pos_u, pos_v

C functions and flags
       integer      pgcurse
       logical      cursor_available, cmd_dblev

       if (s.ne.0) return

       if (.not.plot_open) then
         call cmd_wrerr('CURSOR-GET',
     *                  'Plot not open - unable to access cursor')
         return
       end if
       if (.not.frame_init) then
         call cmd_wrerr('CURSOR-GET',
     *                  'Frame not initialised for image')
         return
       end if
       call pgqcur(cursor_available)
       if (.not.cursor_available) then
         call cmd_wrerr('CURSOR-GET',
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
         call cmd_wrerr('CURSOR-GET','Error reading CURSOR')
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

       if (s.eq.0) then
         uv(1) = pos_u
         uv(2) = pos_v
       endif
       call cmd_err( s,'PLOT_CURSOR_GET',' ')

       end

