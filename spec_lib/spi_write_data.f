*+ spi_write_data

       subroutine spi_write_data( id, status )
C      ---------------------------------------
C
C Data data to open file from an internal file -- id
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
C Data is written for the spectrum with identifier id.
C-

       include 'spec_global.inc'
       include 'spec_data.inc'
       include 'spec_control.inc'
       include 'spec_errors.inc'

C Local variables
       integer      iunit, ndata, nd, ncols, nc
       character*80 string

C check status on entry
       if (status.ne.0) return

C check allocation
       call spi_check_id(id,status)
       if (status.ne.0) goto 999
       if (control_records(alloc,id).eq.alloc_none) then
         status = ill_alloc
         write(string,'(A,I3)') 'Spectrum not allocated ID = ',ID
         call spi_err(status,'SPI_WRITE_DATA',string)
         goto 999
       end if
       if (control_records(alloc,id).eq.alloc_read .or.
     *     control_records(alloc,id).eq.alloc_scratch ) then
         status = ill_alloc_write
         write(string,'(A,I3)')
     *    'Spectrum not allocated for write ID = ',ID
         call spi_err(status,'SPI_WRITE_DATA',string)
         goto 999
       end if

C read data
       iunit = control_records(unit,id)
       ndata = spec_hdi(1,id)
       ncols = spec_hdi(2,id)
       do nd = 1,ndata
         write (iunit,*,iostat=status,err=999)
     *         (spec_data(nc,nd,id), nc=1,ncols)
       end do

999    if (status.ne.0) then
         call spi_err(status,'SPI_WRITE_DATA',' ')
       end if
       end
