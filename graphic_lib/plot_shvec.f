C
C
*+ plot_shvec

       subroutine plot_shvec(status)
C      -----------------------------
C
C Display vector plot options
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '/mrao/include/chrlib_functions.inc'

C error flag
       integer    status
C output unit
       integer    iout
C character strings
       character  source*30, program*8

C check status on entry
       if (status.ne.0) return

C get output unit
       call io_enqout(iout)
       write(iout,10)
10     format(1X/1X,'Vector Plot Option for MAP-DISPLAY'/
     *           1X,'----------------------------------'/1X)

C map read in
       if (vectors_opt) then
         write(iout,'(1X,A)') '  Plotting of Vectors Enabled'
       else
         write(iout,'(1X,A)') '  Plotting of Vectors NOT Enabled'
       end if
       if (vec_chi_map.ne.0) then
         call mapcat_enqsr(vec_chi_map,source,program,status)
         write(iout,100)vec_chi_map,
     *         source(1:chr_lenb(source)),'-',program
 100     format(1X,'  Position-Angle-Map [',i3,'] = ',A,A1,A)
       end if
       if (vec_int_map.ne.0) then
         call mapcat_enqsr(vec_int_map,source,program,status)
         write(iout,110)vec_int_map,
     *                  source(1:chr_lenb(source)),'-',program
 110     format(1X,'  Intensity-Map      [',i3,'] = ',A,A1,A)
       end if

       if (vec_type.eq.0) then
         write(iout,120) 'Vectors scaled by intensity map',vec_scale,
     *                   'scale (grid-points/map-units)'
       else if (vec_type.eq.1) then
         write(iout,120) 'Vectors scaled by intensity map',vec_length,
     *                   'maximum length (grid-points)'
       else if (vec_type.eq.1) then
         write(iout,120) 'Constant length vectors',vec_length,
     *                   'length (grid-points)'
       end if
120    format(1x/1x,'  Plot-Type   : ',A/
     *           1x,'  Length-Type : ',F10.4,' ',A)
       write(iout,130)vec_gate,vec_rotate,vec_u_samp,vec_v_samp
130    format(1x,   '  Gate        : ',1PE10.2/
     *        1x,   '  Rotation    : ',1PE10.2/
     *        1x,   '  Sampling-UV : ',2i5/1x)
       end
