C
C
*+ mapcat_enqrm

       subroutine mapcat_enqrm(report_mode)
C      ------------------------------------
C
C Enquire the report mode status
C
C Returned:
C     Report mode flag
       logical report_mode
*-
       include 'mapcat_cat.inc'
       report_mode = current_report
       end
