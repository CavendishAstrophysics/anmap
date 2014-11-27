C
C
*+ do_mi_map

       subroutine do_mi_map(map_array,status)
C      --------------------------------------
C
C Construct an mI map from a Q and a U map
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C A correction for RICEAN bias is also applied. Q, U maps and a noise
C estimate are required.
*-
       include '../include/error_file_full.inc'

C Local variables
       integer    imapq, ip_mapq, imapu, ip_mapu, iu, iv,
     *            iwarn, imapmi, ip_mapmi, minirt(8)
       real*4     p_noise, gate, ricean_bias, val_lim, val_mi
       integer    ipoln
       real*4     freq
       character  name*16, unit*16

C check status on entry
       if (status.ne.0) return

C find Q-map
       call map_getmap('Map-Q : ','Default-Map','READ',imapq,status)
C find U-map
       call map_getmap('Map-U : ','Default-Map','READ',imapu,status)
       call io_getr('Noise on Q/U maps : ','0.0',p_noise,status)
       call io_getr('Gate (mI >= gate*noise) : ','3.0',gate,status)
       if (status.ne.0) goto 999

C check redtapes
       iwarn = 2
       call redt_comp(imapq,imapu,iwarn,status)
       if (status.ne.0. .or. iwarn.gt.5) goto 999

C check input polarizations
       call redt_load(imapq,status)
       call entype(freq,ipoln,name,unit,status)
       if (ipoln.ne.2 .and. ipoln.ne.3) then
         call cmd_wrerr('mI-MAP','Invalid polarization code')
         call cmd_wrerr('mI-MAP','Error on input Q-map')
         status = ill_poln
       end if
       call redt_load(imapu,status)
       call entype(freq,ipoln,name,unit,status)
       if (ipoln.ne.2 .and. ipoln.ne.3) then
         call cmd_wrerr('mI-MAP','Invalid polarization code')
         call cmd_wrerr('mI-MAP','Error on input U-map')
         status = ill_poln
       end if

C allocate maps
       call map_alloc_in(imapq,'SEQUENTIAL',map_array,ip_mapq,status)
       call map_alloc_in(imapu,'SEQUENTIAL',map_array,ip_mapu,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imapmi,ip_mapmi,status)
       if (status.ne.0) goto 999

C construct the mi map
       val_lim = (p_noise*gate)**2
       call enminirt(minirt,status)
       do iv = minirt(3),minirt(4),-1
         call map_row_read(imapq,iv,map_array,ip_mapq,status)
         call map_row_read(imapu,iv,map_array,ip_mapu,status)
         do iu = 1,minirt(5)
           val_mi = map_array(ip_mapq+iu-1)**2
     *            + map_array(ip_mapu+iu-1)**2
           if (val_mi.gt.val_lim) then
             map_array(ip_mapmi+iu-1) = sqrt(max(val_mi,0.0))
           else
             map_array(ip_mapmi+iu-1) = 0.0
           end if
         end do
         call map_row_write(imapmi,iv,map_array,ip_mapmi,status)
       end do

C Update the redtape
       call adredt('created','mI-MAP',status)
       call redt_update('mI',10,-1.0,' ',' ',status)

C Tidy up, make output map new current map etc.
999    call map_end_alloc(imapmi,map_array,status)
       call map_end_alloc(imapq,map_array,status)
       call map_end_alloc(imapu,map_array,status)

C check STATUS value
       call cmd_err(status,'mI-MAP','Failed')
       end

