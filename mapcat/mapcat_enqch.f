C
C
*+ mapcat_enqch

       subroutine mapcat_enqch(check_mode)
C      -----------------------------------
C
C Enquire the check mode status
C
C Returned:
C     Check mode flag
       logical check_mode
*-
       include 'mapcat_cat.inc'
       check_mode = current_check
       end
