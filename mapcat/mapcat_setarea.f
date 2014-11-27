C
C
*+ mapcat_setarea

       subroutine mapcat_setarea( status )
C      -----------------------------------
C
C Set the maximum area beyond which the WHOLE map is read
C
C Returned:
C   error status
       integer   status
C-
       include 'mapcat_cat.inc'
       call io_getr('Maximum fractional map-area to read WHOLE map : ',
     *           '0.5',area_max_read,status)
       call mapcat_err(status,'mapcat_setarea',' ')
       end
C
C
*+ mapcat_setarea1

       subroutine mapcat_setarea1( a, status )
C      ---------------------------------------
C
C Set the maximum area beyond which the WHOLE map is read
C
C Given:
C   area 
       real*4    a
C Returned:
C   error status
       integer   status
C-
       include 'mapcat_cat.inc'
       area_max_read = a
       call mapcat_err(status,'mapcat_setarea',' ')
       end
