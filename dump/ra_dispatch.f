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
