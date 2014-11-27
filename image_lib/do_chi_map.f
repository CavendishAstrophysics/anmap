C
C
*+ do_chi_map

       subroutine do_chi_map(map_array,status)
C      ---------------------------------------
C
C construct a position-angle/error map from a Q and a U map
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C A correction for RICEAN bias is applied
*-
       include '../include/error_file_full.inc'
C
       integer    imapq, ip_mapq, imapu, ip_mapu, iwarn,
     *            minirt(8), iu, iv, imape, ip_mape, imapo,
     *            ip_mapo, poln_code
       real*4     invtan, val_lim, val_chi, val_mi, p_noise,
     *            ricean_bias, gate, blank_value, frequency
       character  name*16, unit*16

C check status on entry
       if (status.ne.0) return

C find q map
       call map_getmap('Map-Q : ','Default-Map','READ',imapq,status)
C find u map
       call map_getmap('Map-U : ','Default-Map','READ',imapu,status)

C input data
       call io_getr('Noise on Q/U maps : ','0.0',p_noise,status)
       call io_getr('Gate (mI >= gate*noise) : ','3.0',gate,status)
       if (status.ne.0) goto 999

C check redtape
       iwarn = 2
       call redt_comp(imapq,imapu,iwarn,status)
       if (status.ne.0. .or. iwarn.gt.5) goto 999

C check input polarizations
       call redt_load(imapq,status)
       call ennull( blank_value, status )
       call entype( frequency, poln_code, name, unit, status )
       if (poln_code.ne.2) then
         call cmd_wrerr('CHI-MAP','Invalid polarization code')
         call cmd_wrerr('CHI-MAP','Error on input Q-map')
         status = ill_poln
       end if
       call redt_load(imapu,status)
       call entype( frequency, poln_code, name, unit, status )
       if (poln_code.ne.3) then
         call cmd_wrerr('CHI-MAP','Invalid polarization code')
         call cmd_wrerr('CHI-MAP','Error on input U-map')
         status = ill_poln
       end if
       if (status.ne.0) goto 999

C allocate maps
       call map_alloc_in(imapq,'SEQUENTIAL',map_array,ip_mapq,status)
       call map_alloc_in(imapu,'SEQUENTIAL',map_array,ip_mapu,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imapo,ip_mapo,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imape,ip_mape,status)

C construct the mi map
       ricean_bias = p_noise**2
       val_lim = (p_noise*gate)**2
       call enminirt(minirt,status)
       do iv = minirt(3),minirt(4),-1
         call map_row_read(imapq,iv,map_array,ip_mapq,status)
         call map_row_read(imapu,iv,map_array,ip_mapu,status)
         do iu = 1,minirt(5)
           val_mi = map_array(ip_mapq+iu-1)**2
     *            + map_array(ip_mapu+iu-1)**2 - ricean_bias
           val_chi =
     *        invtan(map_array(ip_mapu+iu-1),map_array(ip_mapq+iu-1))
           val_chi = val_chi*0.5
           if (val_mi.gt.val_lim .and. val_mi.gt.0.0) then
             map_array(ip_mape+iu-1) = 0.5*p_noise/val_mi
             map_array(ip_mapo+iu-1) = val_chi
           else
             map_array(ip_mape+iu-1) = blank_value
             map_array(ip_mapo+iu-1) = blank_value
           end if
         end do
         call map_row_write(imapo,iv,map_array,ip_mapo,status)
         call map_row_write(imape,iv,map_array,ip_mape,status)
       end do

C update redtapes
       call adredt('created','CHI-ERR',status)
       call redt_update('CHI',11,-1.0,
     *                  'Error Pos-Angle','RADIANS',status)

C end error map
999    call map_end_alloc(imape,map_array,status)

C main map
       call adredt('created','CHI-MAP',status)
       call redt_update('CHI',11,-1.0,
     *                  'Position Angle','RADIANS',status)
       call map_end_alloc(imapo,map_array,status)

C input maps
       call map_end_alloc(imapq,map_array,status)
       call map_end_alloc(imapu,map_array,status)

C check status value
       call cmd_err(status,'CHI-MAP','Failed')
       end
