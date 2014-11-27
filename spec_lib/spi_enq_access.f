*+ spi_enq_access

       subroutine spi_enq_access( id, access, iunit, status)
C      -----------------------------------------------------
C
C Enquire access to internal spectrum number ID
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C Returned:
C   access to this file
       character*(*)    access
C   unit number of this file
       integer          iunit
C Updated:
C   status return code
       integer          status
C
C Information on an allocated internal file is returned
C The following access codes are possible:
C   READ    -- allocate the file for read, the file MUST exist
C   WRITE   -- allocate the file for write, the file MUST NOT exist
C   UPDATE  -- allocate a file for update access, file MUST exist
C   SCRATCH -- allocate a scratch file
C
C-
       include 'spec_global.inc'
       include 'spec_control.inc'
       include 'spec_errors.inc'

C Local variables
       integer      i
       logical      allocated
       character*80 string

C check status on entry
       if (status.ne.0) return

C make allocation
       call spi_check_id(id,status)
       if (status.ne.0) goto 999
       allocated = .false.
       do i=1,max_buff
         if (i.eq.id) then
           allocated = .true.
         end if
       end do
       if (allocated) then
         if (control_records(alloc,id).eq.alloc_read) then
           access = 'read'
         else if (control_records(alloc,id).eq.alloc_write) then
           access = 'write'
         else if (control_records(alloc,id).eq.alloc_update) then
           access = 'update'
         else if (control_records(alloc,id).eq.alloc_scratch) then
           access = 'scratch'
         end if
         iunit = control_records(unit,id)
       end if
       if (.not.allocated) then
         status = ill_alloc
         write(string,'(A,I3)') 'Spectrum not allocated ID = ',ID
         call spi_err(status,'SPI_ENQ_ACCESS',string)
         goto 999
       end if

999    if (status.ne.0) then
         call spi_err(status,'SPI_ENQ_ACCESS',' ' )
       end if
       end
