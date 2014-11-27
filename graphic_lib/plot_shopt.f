C
C
*+ plot_shopt

       subroutine plot_shopt(status)
C      -----------------------------
C
C Display options as setup
C
C Updated:
C   error status
       integer    status
C
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'

C character option
       character       char_io_onoff*3
C output unit
       integer         iout
C name of source
       character*40    source

C check status on entry
       if (status.ne.0) return

C get output unit
       call io_enqout(iout)
       write(iout,10)
10     format(1X/1X,'Options and Parameters for MAP-DISPLAY'/
     *           1X,'--------------------------------------'/1X)

C map read in
       if (map_defined) then
         call ensrcn( source, status )
         write(iout,100)source,uv_range
 100     format(1X,'  Map read : Title = ',A/1X/
     >          1X,'  UV-range         = ',4I6)
         write(iout,105)data_range(1,1), data_range(2,1)
 105     format(1X,'  Min/Max in range = ',1P2E12.3/1X)
       end if
       write(iout,110)char_io_onoff(interpolate_opt),
     *                char_io_onoff(pips_opt),
     *                char_io_onoff(uvpip_opt),
     *                char_io_onoff(grid_opt),
     *                char_io_onoff(uvgrid_opt)
       write(iout,120)char_io_onoff(cont_opt),
     *                char_io_onoff(cont_opt)
 110   format(1X,'  Interpolation    = ',A/
     >        1X,'  Draw pips        = ',A,'  Draw UV-pips  = ',A/
     >        1X,'  Draw grid        = ',A,'  Draw UV-grid  = ',A)
 120   format(1X,'  Plot date        = ',A/
     >        1X,'  Plot levels      = ',A)
       if (scale_bar_opt.ne.0) then
         write(iout,135) scale_bar_width
 135     format(1X,'  Scale-bar width  = ',F12.3)
       end if

       if (image_opt) then
         write(iout,300)image_min,image_max
       end if
300    format(1X/1X,'Grey Scale Defined'/
     >           1X,'------------------'/
     >           1X,'  Minimum = ',1PE12.3/
     >           1X,'  Maximum = ',1PE12.3)

       end
