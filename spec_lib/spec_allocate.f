*+ spec_allocate

       subroutine spec_allocate( file, access, id, status)
C      ---------------------------------------------------
C
C Allocate a spectrum file with the specified access
C
       implicit   NONE
C
C Given:
C   file name of file to allocate
       character*(*)    file
C   access required to this file
       character*(*)    access
C Returned:
C   spectrum identifier
       integer          id
C Updated:
C   status return code
       integer          status
C
C The specified file is allocated and a spectrum id returned.
C The following access codes are possible:
C   READ    -- allocate the file for read, the file MUST exist
C   WRITE   -- allocate the file for write, the file MUST NOT exist
C   UPDATE  -- allocate a file for update access, file MUST exist
C   SCRATCH -- allocate a scratch file
C
C To create a new file use the access WRITE; to screate a temporary
C spectrum file use the access SCRATCH.
C-
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include 'spec_global.inc'
       include 'spec_header.inc'
       include 'spec_errors.inc'

C Local variables
       integer      iunit, istat, i1
       logical      file_exists, found_ndata, extended_format
       character    line*100
       character    name*256

C check status on entry
       if (status.ne.0) return

C find new spectrum slot
       iunit = 0
       if (file(1:chr_lenb(file)).eq.'-') then
         if (chr_cmatch(access,'READ')) then
           iunit = terminal_in
           file_exists = .true.
         elseif (chr_cmatch(access,'WRITE')) then
           iunit = terminal_out
           file_exists = .false.
         endif
       else
         istat = 0
         call io_namfil( file, name, 0, istat )
         file_exists = istat.eq.0
       endif
       call spi_alloc( access, id, iunit, status )

C depending on access open file
       if (chr_cmatch(access,'READ') .or.
     *     chr_cmatch(access,'UPDATE')   ) then
         if (.not.file_exists) then
           call io_wrout('*** File not found; file = '//file)
           call io_wrout('*** Unable to open file for READ')
           status = ill_file_access
           goto 999
         end if
         if (iunit.ne.terminal_in .and. iunit.ne.terminal_out) then
           open (iunit, file=name(1:chr_lenb(name)),
     *           access='SEQUENTIAL')
         endif
         if (status.ne.0) goto 999
         read (iunit,fmt='(A)') line
         i1 = chr_intlc(line)
         if (line(i1:i1).eq.'%') then
           extended_format = .true.
         else
           extended_format = .false.
         end if
         nitems = 0
         found_ndata = .false.
         ncols = 2
         if (extended_format) then
           do while (line(i1:i1).eq.'%' .and. status.eq.0)
             nitems = nitems+1
             item_name(nitems) = line(i1+1:chr_lenw(line))
             item_data(nitems) = line(chr_lenw(line)+1:chr_lenb(line))
             if (chr_cmatch(item_name(nitems),'ndata')) then
               call chr_chctoi(item_data(nitems),ndata,status)
               found_ndata = .true.
             end if
             if (chr_cmatch(item_name(nitems),'ncols')) then
               call chr_chctoi(item_data(nitems),ncols,status)
             end if
             read (iunit,fmt='(A)') line
             i1 = chr_intlc(line)
           end do
         else
           call chr_chctoi(line(1:chr_lenb(line)),ndata,status)
           if (ndata.gt.0) then
             found_ndata = .true.
           end if
           line = ' '
         end if
         if (.not.found_ndata) ndata = 0
         if (status.ne.0) goto 999

C store header information
         call spi_put_header( id, standard_info, item_name, item_data,
     *                        status )
C read actual data
         call spi_read_data( id, line, status )

       else if (chr_cmatch(access,'WRITE')) then
         if (file_exists) then
           call io_wrout('*** File already exists; file = '//file)
           call io_wrout('*** Unable to open file for WRITE')
           status = ill_file_access
           goto 999
         end if
         if (iunit.ne.terminal_in .and. iunit.ne.terminal_out) then
           open (iunit, file=name(1:chr_lenb(name)),
     *           access='SEQUENTIAL', status='NEW')
         endif
         if (status.ne.0) goto 999
         call spi_put_header( id, standard_info, item_name, item_data,
     *                        status )

       else
         call spi_put_header( id, standard_info, item_name, item_data,
     *                        status )

       end if

999    if (status.ne.0) then
         call spi_err(status,'SPEC_ALLOCATE',' ')
       end if
       end


