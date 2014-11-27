C
C
*+ do_perc_map

       subroutine do_perc_map(map_array,status)
C      ----------------------------------------
C
C construct a %mI/error map from an mI/I map
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C A %mI map with an associated error map are calculated from an mI and
C an I map.
*-
       include '../include/error_file_full.inc'

C Local variables
       integer    minirt(8), imapmi, ip_mapmi, iu, iv,
     *            iwarn, imapo, ip_mapo, imape, ip_mape, imapi, ip_mapi
       real*4     i_noise, p_noise, gate, val_lim_mi, val_lim_i,
     *            val_i, val_mi, val_per
       real*4     freq, blank
       integer    ipoln
       character  name*16, unit*16

C check status on entry
       if (status.ne.0) return

C find mI and I maps
       call map_getmap('Map-mI : ','Default-Map','READ',imapmi,status)
       call io_getr('noise on mi-map : ','0.0',p_noise,status)
       call map_getmap('Map-I : ','Default-Map','READ',imapi,status)
       call io_getr('Noise on I-map : ','0.0',i_noise,status)
       call io_getr('Gate : ','0.0',gate,status)
       if (status.ne.0) goto 999

C check redtape
       iwarn = 2
       call redt_comp(imapmi,imapi,iwarn,status)
       if (status.ne.0. .or. iwarn.gt.5) goto 999

C check input polarizations
       call redt_load(imapmi,status)
       call entype(freq,ipoln,name,unit,status)
       if (ipoln.ne.10) then
         call cmd_wrerr('PERC-MAP','Invalid polarization code')
         call cmd_wrerr('PERC-MAP','Warning Input mI-map')
       end if
       call redt_load(imapi,status)
       call entype(freq,ipoln,name,unit,status)
       if (ipoln.ne.1) then
         call cmd_wrerr('PERC-MAP','Invalid polarization code')
         call cmd_wrerr('PERC-MAP','Warning Input I-map')
       end if

C allocate maps
       call map_alloc_in(imapi,'SEQUENTIAL',map_array,ip_mapi,status)
       call map_alloc_in(imapmi,'SEQUENTIAL',map_array,ip_mapmi,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imapo,ip_mapo,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imape,ip_mape,status)

C construct the mI map
       call ennull(blank,status)
       call enminirt(minirt,status)
       val_lim_mi = p_noise*gate
       val_lim_i =  i_noise*gate
       do iv = minirt(3),minirt(4),-1
         call map_row_read(imapi,iv,map_array,ip_mapi,status)
         call map_row_read(imapmi,iv,map_array,ip_mapmi,status)
         do iu = 1,minirt(5)
           val_i  = map_array(ip_mapi+iu-1)
           val_mi = map_array(ip_mapmi+iu-1)
           if (val_mi.gt.val_lim_mi .and. val_i.gt.val_lim_i) then
             val_per = 100.0*val_mi/val_i
             map_array(ip_mapo+iu-1) = val_per
             map_array(ip_mape+iu-1) = sqrt((p_noise**2)/(val_mi**2)
     *                             + (i_noise**2)/(val_i**2))*val_per
           else
             map_array(ip_mapo+iu-1) = 0.0
             map_array(ip_mape+iu-1) = blank
           end if
         end do
         call map_row_write(imapo,iv,map_array,ip_mapo,status)
         call map_row_write(imape,iv,map_array,ip_mape,status)
       end do

C update the redtape
       call adredt('created','PERC-ERR',status)
       call redt_update('PERC',12,-1.0,'Error %mI','PERCENT',status)

C end error map
999    call map_end_alloc(imape,map_array,status)

C main map
       call redt_update('PERC',12,-1.0,'%mI','PERCENT',status)
       call adredt('created','PERC-MAP',status)
       call map_end_alloc(imapo,map_array,status)

C input maps
       call map_end_alloc(imapi,map_array,status)
       call map_end_alloc(imapmi,map_array,status)

C check status value
       call cmd_err(status,'PERC-MAP','Failed ')
       end

