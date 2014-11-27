C
C
*+ mapcat_setrm

       subroutine mapcat_setrm(report_mode)
C      ------------------------------------
C
C Set the report mode status
C
C Input
C     Report mode flag
       logical report_mode
*-
       include 'mapcat_cat.inc'
       current_report = report_mode
       end
C
C
*+ mapcat_setrm1

       subroutine mapcat_setrm1(rm)
C      ----------------------------
C
C Set the report mode status
C
C Input
C     Report mode flag
       integer   rm
*-
       if (rm.eq.0) then
         call mapcat_setrm( .false. )
       else
         call mapcat_setrm( .true. )
       endif
       end
