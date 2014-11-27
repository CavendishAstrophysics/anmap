C
C
*+ do_predict

       subroutine do_predict(map_array,status)
C      ---------------------------------------
C
C predict flux at a third frequency f3, given maps at f1, f2
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The routine predicts a map at a frequency f3 given maps at f1 and f2
C assuming that the spectrum is a normal synchrotron spectrum.
C The spectral index is defined using the convention:
C
C   S = constant * freq**-alpha
C
C and is calculated at each point; the flux at f3 us then calculated
C by extrapulation.
*-

C Local variables
       integer    minirt(8), imap1, ip_map1, imap2,
     *            ip_map2, iwarn, iu, iv, imapo, ip_mapo
       real       noise_1, noise_2, gate_1, gate_2, freq_1, freq_2,
     *            freq_3, max_out, val_out,
     *            val_lim_1, val_lim_2, val_alpha, val_1, val_2
       real*4     const
       integer    ipoln
       character  name*16, unit*16

C check status on entry
       if (status.ne.0) return

C find map -- 1
       call map_getmap('Map-1 : ','Default-Map','READ',imap1,status)
       call io_getr('Noise 1-map : ','0.0',noise_1,status)
       call io_getr('Gate-1 : ','0.0',gate_1,status)
       call entype(freq_1,ipoln,name,unit,status)
C find map -- 2
       call map_getmap('Map-2 : ','Default-Map','READ',imap2,status)
       call io_getr('Noise 2-Map : ','0.0',noise_2,status)
       call io_getr('Gate-2 : ','0.0',gate_2,status)
       call entype(freq_2,ipoln,name,unit,status)

C check redtape
       iwarn = 2
       call redt_comp(imap1,imap2,iwarn,status)
       if (status.ne.0. .or. iwarn.gt.5) goto 999

C read parameters to control the prediction
       call io_getr('Output map frequency (MHz) : ','151.0',
     *              freq_3,status)
       call io_getr('Maximum allowed on output  : ','1.0E30',
     *           max_out,status)

C allocate maps
       call map_alloc_in(imap1,'SEQUENTIAL',map_array,ip_map1,status)
       call map_alloc_in(imap2,'SEQUENTIAL',map_array,ip_map2,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C construct the alpha map
       val_lim_1 = noise_1*gate_1
       val_lim_2 = noise_2*gate_2
       call enminirt(minirt,status)
       const = log10(freq_3/freq_1)/log10(freq_2/freq_1)
       do iv = minirt(3),minirt(4),-1
         call map_row_read(imap1,iv,map_array,ip_map1,status)
         call map_row_read(imap2,iv,map_array,ip_map2,status)
         do iu = 1,minirt(5)
           val_1 = map_array(ip_map1+iu-1)
           val_2 = map_array(ip_map2+iu-1)
           if (val_1.gt.val_lim_1 .and. val_2.gt.val_lim_2) then
             val_alpha = const * log10(val_1/val_2)
             val_out   = log10(val_1) - val_alpha
             map_array(ip_mapo+iu-1) = 10.0**val_out
             if (val_out.gt.max_out) then
               map_array(ip_mapo+iu-1) = 0.0
             end if
           else
             map_array(ip_mapo+iu-1) = 0.0
           end if
         end do
         call map_row_write(imapo,iv,map_array,ip_mapo,status)
       end do

C update redtapes
       call redt_update(' ',freq_3,-1,' ',' ',status)
       call adredt('created','PREDICT',status)

C end allocations
999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap1,map_array,status)
       call map_end_alloc(imap2,map_array,status)

C check status value
       call cmd_err(status,'PREDICT','Failed')
       end
