C
C
*+ mapcat_setch

       subroutine mapcat_setch(check_mode)
C      -----------------------------------
C
C Set the check mode status
C
C Input:
C     Check mode flag
       logical check_mode
*-
       include 'mapcat_cat.inc'
       current_check = check_mode
       end
