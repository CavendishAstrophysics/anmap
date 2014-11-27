*+ spi_check_id

       subroutine spi_check_id( id, status)
C      -----------------------------------
C
C Check the validity of the spectrum identifier ID
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C Updated:
C   status return code
       integer          status
C
C The specified internal spectrum file identifier is checked
C-
       include 'spec_global.inc'
       include 'spec_control.inc'
       include 'spec_errors.inc'


C check status on entry
       if (status.ne.0) return
       if ( id.le.0 .or. id.gt.max_buff ) then
         status = ill_id
         call spi_err(status,'SPI_CHECK_ID','Illegal identifier')
       end if
       end
