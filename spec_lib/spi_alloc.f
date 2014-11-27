*+ spi_alloc

       subroutine spi_alloc( access, id, iunit, status)
C      ------------------------------------------------
C
C Allocate an internal spectrum file with the specified access
C
       implicit   NONE
C
C Given:
C   access required to this file
       character*(*)    access
C Returned:
C   spectrum identifier
       integer          id
C Updated:
C   unit number of this file
       integer          iunit
C   status return code
       integer          status
C
C An internal file with the requested access is allocated.
C The following access codes are possible:
C   READ    -- allocate the file for read, the file MUST exist
C   WRITE   -- allocate the file for write, the file MUST NOT exist
C   UPDATE  -- allocate a file for update access, file MUST exist
C   SCRATCH -- allocate a scratch file
C
C To create a new file use the access WRITE; to screate a temporary
C spectrum file use the access SCRATCH. If iunit := terminal_in/out then
C no reallocation of the unit is made and I/O will be done on one of these
C two units.
C-
       include '/mrao/include/iolib_constants.inc'
       include 'spec_global.inc'
       include 'spec_control.inc'
       include 'spec_errors.inc'

C Local variables
       integer      i
       logical      allocated
C Functions
       logical      chr_cmatch

C check status on entry
       if (status.ne.0) return

C make allocation
       allocated = .false.
       do i=1,max_buff
         if (control_records(alloc,i).eq.alloc_none) then
           allocated = .true.
           id = i
         end if
       end do
       if (allocated) then
         if (chr_cmatch(access,'READ')) then
           control_records(alloc,id) = alloc_read
         else if (chr_cmatch(access,'UPDATE')) then
           control_records(alloc,id) = alloc_update
         else if (chr_cmatch(access,'WRITE')) then
           control_records(alloc,id) = alloc_write
         else if (chr_cmatch(access,'SCRATCH')) then
           control_records(alloc,id) = alloc_scratch
         end if
         if (iunit.ne.terminal_in .and. iunit.ne.terminal_out) then
           call io_nxtlun( iunit, status )
         endif
         control_records(unit,id) = iunit
       else
         status = ill_do_alloc
         goto 999
       end if

999    if (status.ne.0) then
         call spi_err( status, 'spi_alloc', 'Space allocation failed')
       end if
       end

