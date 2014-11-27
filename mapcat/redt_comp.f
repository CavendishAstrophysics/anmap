C
C
*+ redt_comp

       subroutine redt_comp(imap1,imap2,iwarn,status)
C      ----------------------------------------------
C
C To compare the redtapes for two maps for compatability.
C
C Given:
C   Map entry for first redtape
       integer     imap1
C   Map entry for second redtape
       integer     imap2
C Updated:
C   Report control indicator
C     On entry:-
C          report control  <0 no output report
C                          >0 report only >IWARN
C     On exit:-
C          warning of soft errors
C                          =0   all OK
C                           1   Map names
C                           2   1950.0 map centres
C                           3   Cell size
C                           4   UV range
       integer     iwarn
C
C Returned:
C   Status word
       integer     status
C
C The redtapes of the maps are compared and checked for compatability.
C If the initial setting of IWARN is <0 output will not be to the
C terminal, IWARN is set. IWARN=0 all warnings are output. If IWARN>0
C only erros whose code >IWARN are output. If IWARN .ne. 0 on entry
C then IWARN must be tested on exit. STATUS is solely an error return
C
*-
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       integer    nerror, iout, i, isave
       real*4     usamp1, vsamp1, usamp2, vsamp2
       real*8     ramap1, ramap2, decmap1, decmap2

       character  title1*40, title2*40

       integer    iuv1(8), iuv2(8), level_report
       logical    report, in_error

C test initial value of STATUS and set flags determined by IWARN
       if (status.ne.0) return
       report = iwarn.ge.0
       level_report = abs(iwarn)
       iwarn = 0

C save current redtape
       call redt_enqcurr(isave,status)

C recover redtape items
       call redt_load(imap1,status)
       title1 = rtitle(1)
       call enminirt(iuv1,status)
       ramap1 = ramap
       decmap1 = decmap
       usamp1 = usamp
       vsamp1 = vsamp
       call redt_load(imap2,status)
       title2 = rtitle(1)
       call enminirt(iuv2,status)
       ramap2 = ramap
       decmap2 = decmap
       usamp2 = usamp
       vsamp2 = vsamp
       if (status.ne.0) then
         call cmd_err(status,'redt_comp','comparison failed')
         return
       end if

C restore redtape
       if (isave.gt.0) then
         call redt_load(isave,status)
       end if

C do comparison
       nerror = 0
       call io_enqout(iout)
       if (.not.chr_cmatch(title1,title2)) then
         nerror = nerror + 1
         iwarn = 1
         if (report.and.level_report.le.1) then
           write(iout,10)
  10       format(' ***(REDT_COMPARE) Map/Source title differ')
         end if
       end if
       if (abs(ramap1-ramap2).gt.1.0D-10 .or.
     *     abs(decmap1-decmap2).gt.1.0D-10)   then
         iwarn = 2
         nerror = nerror + 1
         if (report.and.level_report.le.2) then
           write(iout,80)
80         FORMAT(' ***(REDT_COMPARE) 1950.0 map centres differ')
         end if
       end if
       in_error=.false.
       if (abs(usamp1-usamp2).gt.1.0e-4 .or.
     *     abs(vsamp1-vsamp2).gt.1.0e-4)   then
         nerror = nerror + 1
         iwarn =  3
         if (report.and.level_report.le.3) then
           write(iout,100)usamp1,vsamp1,usamp2,vsamp2
100        FORMAT(' ***(REDT_COMPARE) Map-Samplings differ'/
     *            ' ...    Map1 = ',2F12.4,'  Map2 = ',2F12.4)
         end if
       end if
       do i=1,4
         in_error=iuv1(i).ne.iuv2(i) .or. in_error
       end do
       if (in_error) then
         iwarn = 4
         nerror = nerror + 1
         if (report.and.level_report.le.4) then
           write(iout,90)iuv1,iuv2
90         FORMAT(' ***(REDT_COMPARE) UV-ranges differ'/
     *            ' ...    MAP1 = ',4I6,'  Map2 = ',4I6)
         end if
       end if
       end
