*+ spi_read_data

       subroutine spi_read_data( id, line, status )
C      --------------------------------------------
C
C Read data from open file into an internal file -- id
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C   first line of data file
       character*(*)    line
C Updated:
C   status return code
       integer          status
C
C Data is read for the spectrum with identifier id.
C-
       include '/mrao/include/chrlib_functions.inc'
       include 'spec_global.inc'
       include 'spec_data.inc'
       include 'spec_control.inc'
       include 'spec_errors.inc'

C Local variables
       integer      iunit, nend, ndata, nd, ncols, nc, n1
       character*80 string

C check status on entry
       if (status.ne.0) return

C check allocation
       call spi_check_id(id,status)
       if (status.ne.0) goto 999
       if (control_records(alloc,id).eq.alloc_none) then
         status = ill_alloc
         write(string,'(A,I3)') 'Spectrum not allocated ID = ',ID
         call spi_err(status,'SPI_READ_DATA',string)
         goto 999
       end if

C read data
       iunit = control_records(unit,id)
       ndata = spec_hdi(1,id)
       ncols = spec_hdi(2,id)
       if (ndata.eq.0) then
         nend = max_ndata
       else
         nend = ndata
       end if
       ndata = 0
       n1 = 1
       if (chr_lenb(line).ne.0) then
         nd = 1
         read(line,*) (spec_data(nc,nd,id), nc=1,ncols)
         n1 = 2
         ndata = 1
       endif
       do nd = n1,nend
         read (iunit,*,iostat=status,end=1)
     *        (spec_data(nc,nd,id), nc=1,ncols)
         if (status.eq.0) ndata = ndata + 1
       end do
1      continue
       spec_hdi(1,id) = ndata

999    if (status.ne.0) then
         call spi_err(status,'SPI_READ_DATA',' ')
       end if
       end


