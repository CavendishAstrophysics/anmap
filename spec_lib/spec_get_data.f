*+ spec_get_data

       subroutine spec_get_data( id, ic, array, ndata, status)
C      -------------------------------------------------------
C
C Return data for spectrum ID column IC in array ARRAY
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C   column number
       integer          ic
C Returned:
C   data array
       real*4           array(*)
C   number of data points returned
       integer          ndata
C Updated:
C   status return code
       integer          status
C
C Data is returned for spectrum ID, column IC.
C-

       include 'spec_global.inc'
       include 'spec_data.inc'
       include 'spec_control.inc'
       include 'spec_errors.inc'

C Local variables
       integer           ncols, n
       character*80      string

C check status on entry
       if (status.ne.0) return

C check allocation
       call spi_check_id(id,status)
       if (control_records(alloc,id).eq.alloc_none) then
         write(string,'(A,I3)')
     *     '***(SPEC_GET_DATA) ID not allocated ID = ',id
         status = ill_alloc
         call spi_err(status,'SPEC_GET_DATA',string)
       end if
       if (status.ne.0) goto 999

C check inputs
       ndata = spec_hdi(1,id)
       ncols = spec_hdi(2,id)
       if (ic.le.0 .or. ic.gt.ncols) then
         status = ill_column
         write(string,'(A,I3)')
     *     '***(SPEC_GET_DATA) Illegal column number IC = ',ic
         call spi_err(status,'SPEC_GET_DATA',string)
         goto 999
       end if

       do n=1,ndata
         array(n) = spec_data(ic,n,id)
       end do

999    if (status.ne.0) then
         call spi_err(status,'SPEC_GET_DATA',' ')
       end if
       end
