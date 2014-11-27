*+ spi_dealloc

       subroutine spi_dealloc( id, status)
C      -----------------------------------
C
C De-allocate the specified internal spectrum id
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
C The specified internal spectrum file is de-allocated
C-
       include 'spec_global.inc'
       include 'spec_control.inc'
       include 'spec_errors.inc'


C check status on entry
       if (status.ne.0) return

C de-allocation
       call spi_check_id(id,status)
       if (status.ne.0) goto 999
       control_records(alloc,id) = alloc_none

999    if (status.ne.0) then
         call spi_err( status, 'SPI_DEALLOC',' ')
       end if
       end
