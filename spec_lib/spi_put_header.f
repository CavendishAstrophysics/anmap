*+ spi_put_header

       subroutine spi_put_header( id, info, items, values, status )
C      ------------------------------------------------------------
C
C Put header information into an internal spectrum file
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C   info header block
       integer          info(*)
C   character items list
       character*(*)    items(*)
C   character values associated with the items list
       character*(*)    values(*)
C Updated:
C   status return code
       integer          status
C
C The supplied header information is put into / associated with the
C specified internal spectrum file id
C-

       include 'spec_global.inc'
       include 'spec_data.inc'
       include 'spec_control.inc'
       include 'spec_errors.inc'

C Local variables
       integer      i
       character*80 string

C check status on entry
       if (status.ne.0) return

C check allocation
       call spi_check_id(id,status)
       if (status.ne.0) goto 999
       if (control_records(alloc,id).eq.alloc_none) then
         status = ill_alloc
         write(string,'(A,I3)') 'Spectrum not allocated ID = ',ID
         call spi_err(status,'SPI_PUT_HEADER',string)
         goto 999
       end if

C put header information
       do i=1,max_standard_info
         spec_hdi(i,id) = info(i)
       end do
       do i=1,info(3)
         spec_hdc(i,id)(1:len_item_name) = items(i)
         spec_hdc(i,id)(1+len_item_name:len_item_name+len_item_data)
     *       = values(i)
       end do

999    if (status.ne.0) then
         call spi_err(status,'SPI_PUT_HEADER',' ')
       end if
       end

