*+ spec_deallocate

       subroutine spec_deallocate( id, status)
C      ---------------------------------------
C
C De-allocate (and save) spectrum file
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
C If the spectrum had been allocated with access WRITE or update
C then the spectrum is written to the associated file.
C-
       include '/mrao/include/iolib_constants.inc'
       include 'spec_global.inc'
       include 'spec_header.inc'
       include 'spec_errors.inc'

C Local variables
       integer      iunit, i, i1
       character    line*100
       character    access*10
C Functions
       integer      chr_lenb
       logical      chr_cmatch

C check status on entry
       if (status.ne.0) return
C get header information
       call spi_get_header( id, standard_info, item_name, item_data,
     *                      status )
C enquire access state
       call spi_enq_access( id, access, iunit, status )

C write out data if required
       if ( chr_cmatch(access,'WRITE') .or.
     *      chr_cmatch(access,'UPDATE') ) then
          line = ' '
          write(line,'(I10)') ndata
          call spec_hd_set( id, 'ndata',
     *                      line(1:chr_lenb(line)), status )
          line = ' '
          write(line,'(I10)') ncols
          call spec_hd_set( id, 'ncols',
     *                      line(1:chr_lenb(line)), status )
          call spi_get_header( id, standard_info, item_name, item_data,
     *                         status )
          if (status.ne.0) goto 999
          if (iunit.ne.terminal_in .and. iunit.ne.terminal_out) then
            rewind (iunit)
          endif
          do i=1,nitems
            line = ' '
            line(1:1) = '%'
            line(2:100) = item_name(i)
            i1 = chr_lenb(line)
            line(i1+2:100) = item_data(i)
            i1 = chr_lenb(line)
            write(iunit,*) line(1:i1)
          end do
          call spi_write_data( id, status )
          if (iunit.ne.terminal_in .and. iunit.ne.terminal_out) then
            close (iunit)
          endif
       elseif ( chr_cmatch(access,'READ')) then
          if (iunit.ne.terminal_in .and. iunit.ne.terminal_out) then
            close (iunit)
          endif
       end if

C de-allocate spectrum slot
       call spi_dealloc( id, status )

999    if (status.ne.0) then
         call spi_err(status,'SPEC_DEALLOCATE',' ')
       end if
       end





