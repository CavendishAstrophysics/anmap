*+ synch_dispatch

       subroutine synch_dispatch( interp, map_array, cs, lcs, status )
C      ---------------------------------------------------------------
C
C Synchrotron analysis command dispatcher
C
C Updated:
C    command interpreter data structure
       integer            interp(*)
C    map data array
       real*4             map_array(*)
C    command line and length
       character*1024     cs
       integer            lcs 
C Returned:
C    error status
       integer            status
C
C
C Mullard Radio Astronomy Observatory, SUNOS implementation.
C
C Version 3.1
C
C-

       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/iolib_errors.inc'

C Local variables
C ---------------
C  command line interpretation
       integer            number_commands, icom
       parameter         (number_commands = 13)
       character*80       liscom(number_commands)
C spectral details
       integer            spectral_type
       real*4             injection_index, break, alpha, bfield, age
C functions
       real*4             synch_age
C local variables used in the synchrotron analysis
       logical            spectrum_defined
       common /synch_sys/ spectrum_defined

       data liscom(1) /
     *  'equipartition-parameters ..... set equipartition parameters'
     *                /
       data liscom(2) /
     *  'define-synchrotron-spectrum .. define the spectrum to use'
     *                /
       data liscom(3) /
     *  'enquire-spectrum ............. enquire the spectral info'
     *                /
       data liscom(4) /
     *  'fit-synchrotron-spectrum ..... fit data to spectral model'
     *                /
       data liscom(5) /
     *  'make-alpha-X-table ........... make alpha lookup table'
     *                /
       data liscom(6) /
     *  'fit-maps ..................... fit maps with spectral model'
     *                /
       data liscom(7) /
     *  'alpha-to-age-map ............. age-map from alpha, B constant'
     *                /
       data liscom(8) /
     *  'break-to-age-map ............. age-map from break, B constant'
     *                /
       data liscom(9) /
     *  'B-spiral ..................... calculate B (spiral model)'
     *                /
       data liscom(10)/
     *  'equipartition-calculation .... calculate equipartition B-field'
     *                /
       data liscom(11)/
     *  'output-synchrotron-spectrum .. write out region of spectrum'
     *                /
       data liscom(12)/
     *  'calculate-break-from-alpha ... calculate alpha -> break freq.'
     *                /
       data liscom(13)/
     *  'calculate-age-from-break ..... calculate break freq. -> age'
     *                /

C check status on entry
       if (status.ne.0) return
       call anm_disstart( interp, cs, lcs, status )
       call io_getcmd( 'Synchrotron> ',liscom,number_commands,
     *                  icom,status)

       if (status.ne.0) then
         call iocmd_err(status,'Synchrotron',' ')
         goto 1000
       end if

       if ( chr_cmatch('equipartition-parameters',
     *                 liscom(icom))) then
         call bfield_set(status)

       elseif ( chr_cmatch('equipartition-calculation',
     *                 liscom(icom))) then
         call do_bfield_calc(cs, status)

       elseif (chr_cmatch('define-synchrotron_spectrum',
     *                    liscom(icom))) then
         call io_geti('Spectral-type (1-5) : ','5',spectral_type,status)
         if (status.ne.0) goto 999
         if (spectral_type.lt.1 .or. spectral_type.gt.5) then
           spectral_type = 1
         end if
         call io_getr('Injection-index (GAMMA, electron spectrum) : ',
     *                '*',injection_index,status)
         call synch_setsp(injection_index,spectral_type,status)
         spectrum_defined = .true.

       elseif (chr_cmatch('output-synchrotron-spectrum',
     *                    liscom(icom)) )then
         if (.not.spectrum_defined) call do_define_sp(status)
         call do_synch_sp(status)

       elseif (chr_cmatch('make-alpha-x-table',liscom(icom))) then
         call do_2p_lookup(status)

       elseif (chr_cmatch('fit-synchrotron-spectrum',
     *                    liscom(icom))) then
         if (.not.spectrum_defined) call do_define_sp(status)
         call do_fit_data(cs,status)

       elseif (chr_cmatch('fit-maps',liscom(icom))) then
         if (.not.spectrum_defined) call do_define_sp(status)
         call do_fit_maps(map_array,status)

       elseif (chr_cmatch('alpha-to-age-map',liscom(icom))) then
         if (.not.spectrum_defined) call do_define_sp(status)
         call do_alpha_to_age(map_array,status)

       elseif (chr_cmatch('break-to-age-map',liscom(icom))) then
         if (.not.spectrum_defined) call do_define_sp(status)
         call do_age_b_const(map_array,status)

       elseif (chr_cmatch('b-spiral',liscom(icom))) then
         call do_b_spiral(map_array,cs,status)

       elseif (chr_cmatch('enquire-spectrum',liscom(icom))) then
         cs = ' '
         call synch_enqsp(injection_index,spectral_type)
         write(cs,'(I1,'' '',F8.3)') spectral_type,
     *                               injection_index

       elseif (chr_cmatch('calculate-break-from-alpha',
     *                     liscom(icom))) then
         call io_getr('Spectral-index : ','0.75',alpha,status)
         call synch_2p_aatofb( -1.0, -1.0, alpha, break, status)
         cs = ' '
         write(cs,'(1PE12.4)') break

       elseif (chr_cmatch('calculate-age-from-break',
     *                     liscom(icom))) then
         call io_getr('Break-frequency (GHz) : ','1',break,status)
         call io_getr('B-field (nT) : ','1',bfield,status)
         age = synch_age(break,bfield)
         cs = ' '
         write(cs,'(1PE12.4)') age

       end if
 999   call iocmd_err(status,'SYNCHROTRON-ANALYSIS',' ')
1000   call anm_disend( interp, ' ', cs, lcs, status )
       end







