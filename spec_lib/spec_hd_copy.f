*+ spec_hd_copy

       subroutine spec_hd_copy( id_from, id_to, status)
C      ------------------------------------------------
C
C Copy spectrum header descriptors from id_from to id_to
C
       implicit   NONE
C
C Given:
C   spectrum identifier for source spectrum
       integer          id_from
C   spectrum identifier for output spectrum
       integer          id_to
C Updated:
C   status return code
       integer          status
C
C The spectrum header is copied from spectrum ID_FROM to ID_TO
C-
       include 'spec_global.inc'
       include 'spec_header.inc'
       include 'spec_errors.inc'

C check status on entry
       if (status.ne.0) return

C get header information
       call spi_get_header( id_from,
     *                      standard_info, item_name, item_data,
     *                      status )
C put spectrum header
       call spi_put_header( id_to,
     *                      standard_info, item_name, item_data,
     *                      status )

999    if (status.ne.0) then
         call spi_err(status,'SPEC_HD_COPY',' ')
       end if
       end
