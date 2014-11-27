*$ RA-DISPATCH :  program to dispatch RA specific commands for Anmap
*  -------------
C
C Last updated Paul Alexander MRAO, 6/5/94
C
C define work array
       integer       nm, nb
       integer       msize, bsize
       parameter    (msize = 256*256)
       parameter    (bsize=1024)
       parameter    (nm=16)
       parameter    (nb=1)
       real*4        map_array( nm*msize + nb*bsize )
       character     cline*(1024)
C error status and interpreter pointer
       integer       status, interp

C perform standard initialization
       status = 0
       call io_enqline( cline, status )
       call io_setcli( cline )
       call anm_start(interp,nm,msize,nb,bsize,status)
C call user routine
       call ra_sys( interp, map_array, status )

C check status value and report an error to the user
       call cmd_err(status,'RA-DISPATCH','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end
C
C
C
*+ ra_sys

       subroutine ra_sys(interp,map_array,status)
C      ------------------------------------------------
C
C Map analysis sub-system
C
C Updated:
C    command interpreter data structure
       integer       interp(*)
C   map array work space
       real*4         map_array(*)
C   error return
       integer        status
C
C Sub-system providing commands to operate on maps.
*-
C
       include  '../include/anmap_sys_pars.inc'
       include  '/mrao/include/chrlib_functions.inc'
C
       integer          ncomms
       parameter       (ncomms = 10)
       character*60     liscom(ncomms), com

       data liscom(1) /
     *  'alpha-map ................ make spectral index map'
     *                /
       data liscom(2) /
     *  'CHI-map .................. make position-angle map'
     *                /
       data liscom(3) /
     *  'mI-map ................... make polarization map'
     *                /
       data liscom(4) /
     *  'PERC-map ................. make percentage pol. map'
     *                /
       data liscom(5) /
     *  'makei .................... make I map from I+-Q/Q'
     *                /
       data liscom(6) /
     *  'predict-map .............. extrapolate alpha to f3'
     *                /
       data liscom(7) /
     *  'pb-correction ............ RT/CLFST primary beam'
     *                /
       data liscom(8) /
     *  'pbcor-omt ................ correct OMT Primary-Beam'
     *                /
       data liscom(9) /
     *  'pb-make .................. make a map of RT/CLFST PB'
     *                /
       data liscom(10)/
     *  'pbcor-cat ................ correct CAT Primary-Beam'
     *                /

       if (status.ne.0) return

C .. reset error return
         status=0

C .. read command
         call io_getopt('Ra-analysis (?=list) : ','?',liscom,
     *                   ncomms,com,status)

C check for error
         if (status.ne.0) return

         if (chr_cmatch(com,'alpha-map')) then
            call do_alpha_map(map_array,status)
         elseif (chr_cmatch(com,'CHI-map')) then
            call do_chi_map(map_array,status)
         elseif (chr_cmatch(com,'mI-map')) then
            call do_mi_map(map_array,status)
         elseif (chr_cmatch(com,'PERC-map')) then
            call do_perc_map(map_array,status)
         elseif (chr_cmatch(com,'predict-map')) then
            call do_predict(map_array,status)
         elseif (chr_cmatch(com,'makei')) then
            call do_makei(map_array,status)
         elseif (chr_cmatch(com,'pb-correction')) then
            call do_pbcorr(map_array,status)
         elseif (chr_cmatch(com,'pbcor-omt')) then
            call do_omtpbc(map_array,status)
         elseif (chr_cmatch(com,'pb-make')) then
            call do_pb_map(map_array,status)
         elseif (chr_cmatch(com,'pbcor-cat')) then
            call do_catpbc(map_array,status)
         endif
       end
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

C
C
*+ do_catpbc

       subroutine do_catpbc(map_array,status)
C      --------------------------------------
C
C Apply a primary beam correction for the CAT
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The values for the primary beam correction are obtained from the
C local routine CAT_PBCORR.
*-

C Local variables
       integer    tscope, minirt(8), imap, imapo, ip_mapo, iu, iv, ip
       integer    iuvmap
       real*8     u, v, ra, dec, rapnt, decpnt
       real*4     pb_value, freq

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call map_alloc_toout(imap,'DIRECT',map_array,
     *                      imapo,ip_mapo,status)
       call entpnt( tscope, freq, rapnt, decpnt, status )
       if (status.ne.0) goto 999

C read the map size etc.
       call enminirt(minirt,status)
       if (status.ne.0) goto 999

C scale the map
       do iv = minirt(3),minirt(4),-1
         do iu = minirt(1),minirt(2),1
           ip = iuvmap(iu,iv) - 1
           u = iu
           v = iv
           call uvtord( u, v, ra, dec, status )
           call cat_pbcorr(ra,dec,rapnt,decpnt,freq,pb_value,status)

           if (pb_value.gt.0.0) then
             map_array(ip_mapo+ip) = map_array(ip_mapo+ip) /
     *                               pb_value
           else
             map_array(ip_mapo+ip) = 0.0
           end if
         end do
       end do

C Update the TEXT of the redtape
       call adredt('CREATED','CATPBC',status)

C Tidy up
999    call map_end_alloc(imap,map_array,status)
       call map_end_alloc(imapo,map_array,status)

C check status value
       call cmd_err(status,'CATPBC','Failed ')
       end
C
C
C
C
C+cat_pbcorr
C
      subroutine cat_pbcorr ( ra, dec,
     *                        ra_aerial, dec_aerial, freq,
     *                        pb_value, status                 )
C
C     Calculates the primary beam correction for a telescope.
C
C     Given:
C         RA (or hour angle) and dec of the direction that the
C         correction is to be calculated for (angles in radians)
              real*8      ra, dec
C         RA (or hour angle) and dec of the direction that the
C         aerial was pointing at the time
              real*8      ra_aerial, dec_aerial
C         CAT operating frequency
              real*4      freq
C
C     Return values:
C         Primary beam correction factor - a value between 0 and 1
              real        pb_value
C         Status value
              integer     status
C
C     Returns the value of the primary beam for the CAT.
C
C-
             include  '/mrao/include/constants.inc'

C     Variables for calculating tabulated beams
          real*8          s

      if (status.ne.0) return


C Symmetrical beams tabulated in include file.
       s = dsin(dec)*dsin(dec_aerial)+
     *     dcos(dec)*dcos(dec_aerial)*dcos(ra_aerial-ra)
       if ( s .gt. 1.0 ) then
              s = 0.0D+0
       else if ( s .lt. -1.0 ) then
              s = const_pi
       else
              s = abs(acos( s ))
       end if

C Convert separation to a scale factor
       s = freq*s/(1000.0*const_d2r)

C Return PB value
       pb_value = 0.9987 + s*1.2E-3 - (s**2)*2.543E-3 +
     *            (s**3)*8.49E-6 + (s**4)*3.262E-6 -
     *            (s**5)*7.954E-8 + (s**6)*5.573E-10

 9999 if (status.ne.0) call maperr(status,'in subroutine cat_pbcorr.')
      return
      end
C Chi_map:
C
C Document origin and update of this routine
C   P. Alexander, MRAO, Cambridge, 09/04/92
C
*-


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
*       call entype( frequency, poln_code, name, unit, status )
*       if (poln_code.ne.2) then
*         call cmd_wrerr('CHI-MAP','Invalid polarization code')
*         call cmd_wrerr('CHI-MAP','Error on input Q-map')
*         status = ill_poln
*       end if
*       call redt_load(imapu,status)
*       call entype( frequency, poln_code, name, unit, status )
*       if (poln_code.ne.3) then
*         call cmd_wrerr('CHI-MAP','Invalid polarization code')
*         call cmd_wrerr('CHI-MAP','Error on input U-map')
*         status = ill_poln
*       end if
*       if (status.ne.0) goto 999

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
       print *,'.. status 1 = ',status
       call adredt('created','CHI-ERR',status)
       call redt_update('CHI',11,-1.0,
     *                  'Error Pos-Angle','RADIANS',status)
       print *,'.. status 2 = ',status

C end error map
999    call map_end_alloc(imape,map_array,status)
       print *,'.. status 3 = ',status

C main map
       call adredt('created','CHI-MAP',status)
       print *,'.. status 4 = ',status

       call redt_update('CHI',11,-1.0,
     *                  'Position Angle','RADIANS',status)
       print *,'.. status 5 = ',status

       call map_end_alloc(imapo,map_array,status)
       print *,'.. status 6 = ',status

C input maps
       call map_end_alloc(imapq,map_array,status)
       call map_end_alloc(imapu,map_array,status)

C check status value
       call cmd_err(status,'CHI-MAP','Failed')
       end

C
C
*+ do_makei

       subroutine do_makei(map_array,status)
C      -------------------------------------
C
C make an I map from an I+/-Q and Q combination
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The routine will prompt for an I+/-Q and a Q map. The redtape defining
C the STOKES parameter must be correct as this is used to determine the
C way in which the maps are combined.
*-

C Local variables
       integer    imapiq, ip_mapiq, imapq, ip_mapq,
     *            imapo, ip_mapo, iu, iv,
     *            iwarn, isign, minirt(8)
       integer    ipoln
       character  name*16, unit*16
       real*4     freq

C check status on entry
       if (status.ne.0) return

C find I+/-Q and Q maps
       call map_getmap('Map-I+/-Q : ','Default-Map','READ',imapiq,
     *                 status)
       call entype(freq,ipoln,name,unit,status)
       if (status.ne.0) goto 999
       if (ipoln.eq.5) then
         isign = +1
       else if (ipoln.eq.6) then
         isign = -1
       else
         call cmd_wrerr('MAKEI','Input map is not I+/-Q: Failed')
         goto 999
       end if

       call map_getmap('Map-Q : ','Default-Map','READ',
     *                  imapq,status)
       call entype(freq,ipoln,name,unit,status)
       if (ipoln.ne.2) then
         call cmd_wrerr('MAKEI','Map is not Q-map: Failes')
         goto 999
      end if

C check redtape
       iwarn = 2
       call redt_comp(imapiq,imapq,iwarn,status)
       if (status.ne.0. .or. iwarn.gt.5) goto 999

C allocate maps
       call map_alloc_in(imapiq,'SEQUENTIAL',map_array,ip_mapiq,status)
       call map_alloc_in(imapq,'SEQUENTIAL',map_array,ip_mapq,status)
       call map_alloc_out(0,0,'SEQUENTIAL',imapo,ip_mapo,status)

C do the addition
       call enminirt(minirt,status)
       do iv = minirt(3),minirt(4),-1
         call map_row_read(imapiq,iv,map_array,ip_mapiq,status)
         call map_row_read(imapq,iv,map_array,ip_mapq,status)
         do iu = 1,minirt(5)
           map_array(ip_mapo+iu-1) = map_array(ip_mapiq+iu-1)
     *                           + isign*map_array(ip_mapq+iu-1)
         end do
           call map_row_write(imapo,iv,map_array,ip_mapo,status)
       end do

C update the redtape
       call adredt('created','MAKEI',status)
       call redt_update('IMAP',1,-1.0,' ',' ',status)

C tidy up
999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imapiq,map_array,status)
       call map_end_alloc(imapq,map_array,status)

C check status value
       call cmd_err(status,'MAKEI','Failed ')
       end
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

C
C
C
C
*+  do_omtpbc

       subroutine do_omtpbc(map_array,status)
C      --------------------------------------
C
C Apply a primary beam correction for the OMT at 1407MHz
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The functional form assumed is exp-(theta**2/theta0**2)
C                                    theta0 = 28.5 arcmin
*-

C Local variables
       real*4     u_centre, v_centre, theta, theta0
       integer    minirt(8), imap, imapo, ip_mapo, iu, iv, ip
       integer    iuvmap
       real*8     usamp, vsamp

       data       theta0 /1710.0/

C check status on entry
       if (status.ne.0) return

C read input map
       call io_wrout('.. Applying OMT PBCOR (approximate)')
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call map_alloc_toout(imap,'DIRECT',map_array,
     *                      imapo,ip_mapo,status)
       call ensamp(usamp,vsamp,status)
       if (status.ne.0) goto 999

C read the central pixel
       call enminirt(minirt,status)
       call io_getr('U-centre : ','0.0',u_centre,status)
       call io_getr('V-centre : ','0.0',v_centre,status)
       if (status.ne.0) goto 999

C scale the map
       do iv = minirt(3),minirt(4),-1
         do iu = minirt(1),minirt(2),1
           ip = iuvmap(iu,iv) - 1
           theta = sqrt( ((float(iu)-u_centre)*usamp)**2
     *                  +((float(iv)-v_centre)*vsamp)**2 )
           map_array(ip_mapo+ip) = exp((theta/theta0)**2) *
     *                             map_array(ip_mapo+ip)
         end do
       end do

C Update the TEXT of the redtape
       call adredt('CREATED','PB-OMT',status)

C Tidy up
999    call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'PBCOR-OMT','Failed ')
       end
C
C
C
C
*+ do_pb_map

       subroutine do_pb_map(map_array,status)
C      --------------------------------------
C
C Construct a map of the primary beam
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The values for the primary beam correction are obtained from the
C MAPLIB routine PBCORR.  An input map is required to define the
C sampling of the output primary beam.
*-

C Local variables
       integer    tscope, minirt(8), imap, imapo, ip_mapo, iu, iv, ip
       integer    iuvmap
       real*8     u, v, ra, dec, rapnt, decpnt
       real*4     pb_value, freq

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map-defining-pb-region : ',
     *                 'Default_Map','READ',imap,status)
       call map_alloc_toout(imap,'DIRECT',map_array,
     *                      imapo,ip_mapo,status)
       call entpnt( tscope, freq, rapnt, decpnt, status )
       if (status.ne.0) goto 999

C read the map size etc.
       call enminirt(minirt,status)
       if (status.ne.0) goto 999

C scale the map
       do iv = minirt(3),minirt(4),-1
         do iu = minirt(1),minirt(2),1
           ip = iuvmap(iu,iv) - 1
           u = iu
           v = iv
           call uvtord( u, v, ra, dec, status )
           call pbcorr( ra, dec, rapnt, decpnt, tscope,
     *                  pb_value, status )
           if (pb_value.gt.0.0) then
             map_array(ip_mapo+ip) = pb_value
           else
             map_array(ip_mapo+ip) = 0.0
           end if
         end do
       end do

C Update the TEXT of the redtape
       call adredt('CREATED','PBMAKE',status)

C Tidy up
999    call map_end_alloc(imap,map_array,status)
       call map_end_alloc(imapo,map_array,status)

C check status value
       call cmd_err(status,'PBMAKE','Failed ')
       end
C
C
*+ do_pbcorr

       subroutine do_pbcorr(map_array,status)
C      --------------------------------------
C
C Apply a primary beam correction for the RT and CLFST
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The values for the primary beam correction are obtained from the
C MAPLIB routine PBCORR.
*-

C Local variables
       integer    tscope, minirt(8), imap, imapo, ip_mapo, iu, iv, ip
       integer    iuvmap
       real*8     u, v, ra, dec, rapnt, decpnt
       real*4     pb_value, freq

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call map_alloc_toout(imap,'DIRECT',map_array,
     *                      imapo,ip_mapo,status)
       call entpnt( tscope, freq, rapnt, decpnt, status )
       if (status.ne.0) goto 999

C read the map size etc.
       call enminirt(minirt,status)
       if (status.ne.0) goto 999

C scale the map
       do iv = minirt(3),minirt(4),-1
         do iu = minirt(1),minirt(2),1
           ip = iuvmap(iu,iv) - 1
           u = iu
           v = iv
           call uvtord( u, v, ra, dec, status )
           call pbcorr( ra, dec, rapnt, decpnt, tscope,
     *                  pb_value, status )
           if (pb_value.gt.0.0) then
             map_array(ip_mapo+ip) = map_array(ip_mapo+ip) /
     *                               pb_value
           else
             map_array(ip_mapo+ip) = 0.0
           end if
         end do
       end do

C Update the TEXT of the redtape
       call adredt('CREATED','PBCORR',status)

C Tidy up
999    call map_end_alloc(imap,map_array,status)
       call map_end_alloc(imapo,map_array,status)

C check status value
       call cmd_err(status,'PBCORR','Failed ')
       end
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
