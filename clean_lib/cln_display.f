C
C
*+ cln_display

       subroutine cln_display(iout,status)
C      -----------------------------------
C
C Display settings for all CLEAN parameters on unit IOUT
C
C Given:
C   output unit number
       integer      iout
C Returned:
C   error status
       integer      status
C
C Display all the information concerning the current CLEAN setup.
C
*-
       include '../include/clean_record.inc'
       include '/mrao/include/maplib_redtape.inc'

       integer    n, i

C check status on entry
       if (status.ne.0) return

       call ldredt(redt_map,status)
       write(iout,100)generic_name,beam_name
       write(iout,101)iumap1,iumap2,ivmap1,ivmap2
       write(iout,102)(clean_window(i), i=1,4)
       write(iout,103)(beam_window(i), i=1,4)
       do n=1,number_search
         write(iout,104)n,(search_window(i,n), i=1,4)
       end do
100    FORMAT(1X/1X,' Generic file name  = ',A/
     >           1X,' Beam file name     = ',A/1X)
101    FORMAT(1X,' Range on input map = ',4I6)
102    FORMAT(1X,' Clean window       = ',4I6)
103    FORMAT(1X,' Beam window        = ',4I6)
104    FORMAT(1X,' Search window (',I1,')  = ',4I6)
       write(iout,105)itlim
       write(iout,106)fllim
       write(iout,107)fract
105    FORMAT(1X,' Iteration limit    = ',I6)
106    FORMAT(1X,' Flux limit         = ',1PE12.3)
107    FORMAT(1X,' Fraction removed   = ',F8.3/1X)

C options
       if (lstop0) then
         write(iout,*)' '
         write(iout,*)' Truncation at first zero component enabled'
       end if
       if (lnotbx) then
         write(iout,*)' '
         write(iout,*)' Not box selected:'
         write(iout,108)(box_window(i), i=1,4)
       end if
108    FORMAT(1X,' NOT-box window     = ',4I6/1X)
       if (lprhat) then
         write(iout,*)' '
         write(iout,*)' Prussian Hat clean enabled'
         write(iout,109)prhat
       END IF
109    FORMAT(1X,' Prussian hat height= ',F8.2/1X)
       write(iout,110)beam_size_u,beam_size_v,beam_pa
110    FORMAT(1X/1X,' Gaussian CLEAN beam size = ',2F8.2,' arcsec ',
     >           'pa = ',f8.2,' degrees')
       if (ltrbm) then
         write(iout,*)' '
         write(iout,*)' Truncated dirty-beam option enabled'
       end if
       if (lsort) then
         write(iout,*)' '
         write(iout,*)' SORT option enabled - MRAO go-faster CLEAN'
         write(iout,111)sort_depth, sort_inner, sort_gate
111      FORMAT(1X,' Sort-Depth         = ',I10/
     >             ' Inner-Iterations   = ',I10/
     >             ' Sort-Gate          = ',1PE10.2)
       end if
       write(iout,*)' '
       if (times_cleaned .ne.0) call cln_result(iout,status)

       end
