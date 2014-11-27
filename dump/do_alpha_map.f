C
C
*+ do_alpha_map

       subroutine do_alpha_map(map_array,status)
C      -----------------------------------------
C
C construct a spectral-index/error map from 2 I maps
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The spectral index is defined using the convention:
C
C   S = constant * freq**-alpha
C
C In addition to the spectral index map and error map is also constructed.
*-

C Local variables
       integer    minirt(8), iu, iv, iwarn
       integer    imap1, ip_map1, imap2, ip_map2
       integer    imapo, ip_mapo, imape, ip_mape
       integer    ifreq_1, ifreq_2
       real*4     noise_1, noise_2, gate_1, gate_2, freq_1, freq_2,
     *            val_lim_1, val_lim_2, val_alpha, val_1, val_2
       real*4     const, val_e
       character  unit*16, name*16
       integer    ipoln
       real*4     blank

C check status on entry
       if (status.ne.0) return

C read map -- 1
       call map_getmap('Map-1 : ','Default-Map','READ',imap1,status)
       call io_getr('Noise 1-map : ','0.0',noise_1,status)
       call io_getr('Gate-1 : ','0.0',gate_1,status)
       call entype(freq_1,ipoln,name,unit,status)
C read map -- 2
       call map_getmap('Map-2 : ','Default-Map','READ',imap2,status)
       call io_getr('Noise 2-Map : ','0.0',noise_2,status)
       call io_getr('Gate-2 : ','0.0',gate_2,status)
       call entype(freq_2,ipoln,name,unit,status)
       if (status.ne.0) goto 999

C check frequencies of the two maps
       if (abs(freq_1-freq_2).lt.1.0E-12) then
         call cmd_wrerr('ALPHA-MAP',
     *        'Map frequencies equal, unable to make ALPHA map: STOP')
         goto 999
       end if
       if (freq_1.le.0.0) then
         call cmd_wrerr('ALPHA-MAP','Frequency map-1 <= 0.0: STOP')
         goto 999
       end if
       if (freq_2.le.0.0) then
         call cmd_wrerr('ALPHA-MAP','Frequency map-2 <= 0.0: STOP')
         goto 999
       end if

C check redtape
       iwarn = 2
       call redt_comp(imap1,imap2,iwarn,status)
       if (status.ne.0. .or. iwarn.gt.5) goto 999

C allocate maps
       call map_alloc_in(imap1,'SEQUENTIAL',map_array,ip_map1,status)
       call map_alloc_in(imap2,'SEQUENTIAL',map_array,ip_map2,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imapo,ip_mapo,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imape,ip_mape,status)
       if (status.ne.0) goto 999

C determine limiting map values and constants
       call ennull(blank,status)
       val_lim_1 = noise_1*gate_1
       val_lim_2 = noise_2*gate_2
       call enminirt(minirt,status)
       const = 1.0/log(freq_2/freq_1)
      if (status.ne.0) goto 999

C construct the alpha map
       do iv = minirt(3),minirt(4),-1
         call map_row_read(imap1,iv,map_array,ip_map1,status)
         call map_row_read(imap2,iv,map_array,ip_map2,status)
         do iu = 1,minirt(5)
           val_1 = map_array(ip_map1+iu-1)
           val_2 = map_array(ip_map2+iu-1)
           if (val_1.gt.val_lim_1 .and. val_2.gt.val_lim_2) then
             val_alpha = const * log(val_1/val_2)
             map_array(ip_mapo+iu-1) = val_alpha
             val_e = abs(const)*
     *               sqrt((noise_1/val_1)**2 + (noise_2/val_2)**2)
             map_array(ip_mape+iu-1) = val_e
           else
             map_array(ip_mapo+iu-1) = 0.0
             map_array(ip_mape+iu-1) = blank
           end if
         end do
         call map_row_write(imapo,iv,map_array,ip_mapo,status)
         call map_row_write(imape,iv,map_array,ip_mape,status)
         if (status.ne.0) goto 999
       end do

C update redtapes
       ifreq_1 = freq_1 + 0.5
       ifreq_2 = freq_2 + 0.5
       write (unit,10)ifreq_1,ifreq_2
10     format('a :',i6,':',i6)
       call redt_update(' ',13,0.0,'Error Sp-Index',unit,status)
       call adredt('created','ALPH-ERR',status)

C end error map
999    continue
       call map_end_alloc(imape,map_array,status)

C main map
       call redt_update(' ',13,0.0,'Spectral Index',unit,status)
       call adredt('created','ALPH-MAP',status)
       call map_end_alloc(imapo,map_array,status)

C input maps
       call map_end_alloc(imap1,map_array,status)
       call map_end_alloc(imap2,map_array,status)

C check status value
       call cmd_err(status,'ALPHA-MAP','Failed')
       end

