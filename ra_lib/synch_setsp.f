C
C
*+ synch_setsp

       subroutine synch_setsp(injection_index_E,spectral_type,status)
C      --------------------------------------------------------------
C
C Set-up the theoretical spectrum for later use with defined properties
C
C Input:
C    injection index of electron population == GAMMA
       real*4         injection_index_E
C    spectra type code
       integer        spectral_type
C Returned:
C    status
       integer        status
C
C The electron spectrum is assumed to be of the form:
C      N(E)dE = N0 E**-GAMMA dE
C The spectral type is a code as follows:
C      1      ==     KP spectrum aged angle averaged
C      2      ==     KP spectrum aged not angle averaged
C      3      ==     KP spectrum continuous injection angle averaged
C      4      ==     KP spectrum continuous injection not angle averaged
C      5      ==     JP spectrum
C
C The theoretical spectrum appropriate to these values is defined.  If
C a suitable spectrum is not on file then the spectrum is calculated.
C Note that the injection index is assumed accurate to only two places
C of decimals.
C
*-
       include '../include/anmap_sys_pars.inc'
       include '../include/synch_defn.inc'
C
       integer     local_igamma, ifile, istat, iword
       integer     header_record(220), nrec
       logical     spectrum_not_found
       character   name*80

       if (status.ne.0) return

C define integer value of gamma to use in search
       local_igamma = injection_index_E*100.0
C test current spectrum
       if (local_igamma.eq.igamma .and. spectral_type.eq.iflag) return
C open file and search for a suitable spectrum
       istat = 0
       call io_namfil(synch_sys_file,name,0,istat)
       if (istat.ne.0) then
C .. create spectral file
         istat = 0
         call io_operan(ifile,synch_sys_file,'WRITE',880,0,status)
         header_record(1) = 0
         call io_wrfile(ifile,1,header_record,220,status)
         if (status.ne.0) goto 999
         spectrum_not_found = .true.
       else
         call io_operan(ifile,synch_sys_file,'WRITE',880,0,status)
         iword = 220
         call io_rdfile(ifile,1,header_record,iword,status)
         nrec = 2
         if (status.ne.0) goto 999
         spectrum_not_found = .true.
         do while ( spectrum_not_found .and.
     *              nrec.le.(header_record(1)+1) )
           iword = 220
           call io_rdfile(ifile,nrec,synch_curr_sp,iword,status)
           nrec = nrec + 1
           if (igamma.eq.local_igamma .and.
     *         spectral_type.eq.iflag) then
             spectrum_not_found = .false.
           end if
         end do
       end if
100    continue
       if (spectrum_not_found) then
         iflag  = spectral_type
         igamma = local_igamma
         gamma  = injection_index_E
         call io_wrout(' ')
         call io_wrout('.. constructing spectrum please be patient')
         call synch_makesp(status)
         if (status.ne.0) goto 999
         call io_wrout('.. spectrum constructed successfully')
         call io_wrout(' ')
         header_record(1) = header_record(1) + 1
         call io_wrfile(ifile,1,header_record,220,status)
         nrec = header_record(1) + 1
         call io_wrfile(ifile,nrec,synch_curr_sp,220,status)
       end if

999    close (ifile)
       call iocmd_err(status,'SYNCH_GETSP','Failed')

       end

