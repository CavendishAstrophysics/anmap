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
       include 'spec_global.inc'
       include 'spec_header.inc'
       include 'spec_errors.inc'

C Local variables
       integer      iunit, istat
       logical      file_exists, found_ndata, extended_format
       character    line*100
       character    name*256
C Functions
       integer      chr_lenb, chr_lenw
       logical      chr_cmatch

C check status on entry
       if (status.ne.0) return

C find new spectrum slot
       call spi_alloc( access, id, iunit, status )

C depending on access open file
       istat = 0
       call io_namfil( file, name, 0, istat )
       file_exists = istat.eq.0
       if (chr_cmatch(access,'READ') .or.
     *     chr_cmatch(access,'UPDATE')   ) then
         if (.not.file_exists) then
           call io_wrout('*** File not found; file = '//file)
           call io_wrout('*** Unable to open file for READ')
           status = ill_file_access
           goto 999
         end if
         open (iunit, file=name(1:chr_lenb(name)),
     *         access='SEQUENTIAL')
         if (status.ne.0) goto 999
         read (iunit,fmt='(A)') line
         if (line(1:1).eq.'%') then
           extended_format = .true.
         else
           extended_format = .false.
         end if
         nitems = 0
         found_ndata = .false.
         ncols = 2
         if (extended_format) then
           do while (line(1:1).eq.'%' .and. status.eq.0)
             nitems = nitems+1
             item_name(nitems) = line(2:chr_lenw(line))
             item_data(nitems) = line(chr_lenw(line)+1:chr_lenb(line))
             if (chr_cmatch(item_name(nitems),'ndata')) then
               call chr_chctoi(item_data(nitems),ndata,status)
               found_ndata = .true.
             end if
             if (chr_cmatch(item_name(nitems),'ncols')) then
               call chr_chctoi(item_data(nitems),ncols,status)
             end if
             read (iunit,fmt='(A)') line
           end do
           backspace (iunit)
         else
           call chr_chctoi(line(1:chr_lenb(line)),ndata,status)
           if (ndata.gt.0) then
             found_ndata = .true.
           end if
         end if
         if (.not.found_ndata) ndata = 0
         if (status.ne.0) goto 999

C store header information
         call spi_put_header( id, standard_info, item_name, item_data,
     *                        status )
C read actual data
         call spi_read_data( id, status )

       else if (chr_cmatch(access,'WRITE')) then
         if (file_exists) then
           call io_wrout('*** File already exists; file = '//file)
           call io_wrout('*** Unable to open file for WRITE')
           status = ill_file_access
           goto 999
         end if
         open (iunit, file=name(1:chr_lenb(name)),
     *         access='SEQUENTIAL', status='NEW')
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
          rewind (iunit)
          do i=1,nitems
            line(1:1) = '%'
            line(2:100) = item_name(i)
            i1 = chr_lenb(line)
            line(i1+2:100) = item_data(i)
            i1 = chr_lenb(line)
            write(iunit,*) line(1:i1)
          end do
          call spi_write_data( id, status )
          close (iunit)
       elseif ( chr_cmatch(access,'READ')) then
          close (iunit)
       end if

C de-allocate spectrum slot
       call spi_dealloc( id, status )

999    if (status.ne.0) then
         call spi_err(status,'SPEC_DEALLOCATE',' ')
       end if
       end
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
*+ spec_getcols

       subroutine spec_getcols( id, ncols, clist, status )
C      ---------------------------------------------------
C
C Get list of columns to work on
C
       implicit   NONE
C
C Given:
C    spectrum identifier
       integer    id
C Returned:
C    number of columns
       integer    ncols
C    column control list
       integer    clist(*)
C Updated:
C    error return code
       integer    status
C
C Use the standard parameters XC, COLS to decide which columns of
C spectrum id to use in analysis.
C
C    XC specifies the X-column ; coded with clist(xc) = 0
C    COLS specify a list of columns to use ; colded with clist() = 1
C    columns not to use are coded with clist() = -1
C    COLS = -1 or ALL specifies all columns are to be used
C    The default for XC is 1 and COLS is ALL
C
C-
       include    'spec_global.inc'
C
       integer        xc, list(max_ncols), m, n
       character      cols*20
       integer        chr_lenb

       if (status.ne.0) return
       call spec_hd_enqi( id, 'ncols', ncols, status )
       call cmd_itemi( 'XC', 1, xc, status )
       call cmd_items( 'COLS', cols, status )
       call chr_chucas( cols )
       if (cols(1:3).eq.'ALL') then
         do n=1,ncols
           clist(n) = 1
         end do
       else
         call chr_chlsti( cols(1:chr_lenb(cols)),
     *                    list, ncols, m, status )
         do n=1,ncols
           clist(n) = -1
         end do
         do n=1,m
           clist(list(n)) = 1
         end do
       end if
       if (xc.gt.0 .and. xc.le.ncols) then
         clist(xc) = 0
       end if
       end
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
*+ spec_hd_enq

       subroutine spec_hd_enq( id, item, data, status)
C      -----------------------------------------------
C
C Enquire an item in the spectrum header
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C   name of item
       character*(*)    item
C Returned:
C   data for item
       character*(*)    data
C Updated:
C   status return code
       integer          status
C
C The spectrum header is enquired for the named item.  If the
C item is not present in the header then the returned data string
C is empty. If the ITEM is COMMENT or HISTORY the last entry in the
C header file of that type is returned.
C-
       include 'spec_global.inc'
       include 'spec_header.inc'
       include 'spec_errors.inc'

C Local varaibles
       integer         i, i1, i2, n
       logical         found_item
       character*20    string, test
C Functions
       integer         chr_lenb

C check status on entry
       if (status.ne.0) return

C get header information
       call spi_get_header( id,
     *                      standard_info, item_name, item_data,
     *                      status )
       string = item
       call chr_chucas(string)
       i1 = chr_lenb(string)
       found_item = .false.
       do n=1,nitems
         test = item_name(n)
         call chr_chucas(test)
         i2 = chr_lenb(test)
         if (string.eq.test) then
           found_item = .true.
           i = n
         end if
       end do
       if (found_item) then
         data = item_data(i)
       else
         data = ' '
       end if

999    if (status.ne.0) then
         call spi_err(status,'SPEC_HD_ENQ',' ')
       end if
       end
*+ spec_hd_enqi

       subroutine spec_hd_enqi( id, item, data, status)
C      ------------------------------------------------
C
C Enquire an item in the spectrum header
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C   name of item
       character*(*)    item
C Returned:
C   data for item
       integer          data
C Updated:
C   status return code
       integer          status
C
C The spectrum header is enquired for the named item.  If the
C item is not present in the header then the returned data string
C is zero.  If the ITEM is of the wrong type then this routine will
C return with a non-zero error code and approprite error message.
C-
       include 'spec_global.inc'
       include 'spec_header.inc'
       include 'spec_errors.inc'

C Local varaibles
       integer         i, i1, i2, n
       logical         found_item
       character*20    string, test
C Functions
       integer         chr_lenb

C check status on entry
       if (status.ne.0) return

C get header information
       call spi_get_header( id,
     *                      standard_info, item_name, item_data,
     *                      status )
       string = item
       call chr_chucas(string)
       i1 = chr_lenb(string)
       found_item = .false.
       do n=1,nitems
         test = item_name(n)
         call chr_chucas(test)
         i2 = chr_lenb(test)
         if (string.eq.test) then
           found_item = .true.
           i = n
         end if
       end do
       if (found_item) then
         call chr_chctoi( item_data(i), data, status )
         if (status.ne.0) then
           goto 999
         end if
       else
         data = 0
       end if

999    if (status.ne.0) then
         call spi_err(status,'SPEC_HD_ENQI',item_data(i))
       end if
       end
*+ spec_hd_enqr

       subroutine spec_hd_enqr( id, item, data, status)
C      ------------------------------------------------
C
C Enquire an item in the spectrum header
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C   name of item
       character*(*)    item
C Returned:
C   data for item
       real*4           data
C Updated:
C   status return code
       integer          status
C
C The spectrum header is enquired for the named item.  If the
C item is not present in the header then the returned data string
C is zero.  If the ITEM is of the wrong type then this routine will
C return with a non-zero error code and approprite error message.
C-
       include 'spec_global.inc'
       include 'spec_header.inc'
       include 'spec_errors.inc'

C Local varaibles
       integer         i, i1, i2, n
       logical         found_item
       character*20    string, test
C Functions
       integer         chr_lenb

C check status on entry
       if (status.ne.0) return

C get header information
       call spi_get_header( id,
     *                      standard_info, item_name, item_data,
     *                      status )
       string = item
       call chr_chucas(string)
       i1 = chr_lenb(string)
       found_item = .false.
       do n=1,nitems
         test = item_name(n)
         call chr_chucas(test)
         i2 = chr_lenb(test)
         if (string.eq.test) then
           found_item = .true.
           i = n
         end if
       end do
       if (found_item) then
         call chr_chctor( item_data(i), data, status )
         if (status.ne.0) then
           goto 999
         end if
       else
         data = 0
       end if

999    if (status.ne.0) then
         call spi_err(status,'SPEC_HD_ENQR',item_data(i))
       end if
       end
*+ spec_hd_print

       subroutine spec_hd_print( id, iunit, extra_title, status)
C      ---------------------------------------------------------
C
C Print the spectrum header descriptors on unit IUNIT
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C   output unit for printing
       integer          iunit
C   additional title for display
       character*(*)    extra_title
C Updated:
C   status return code
       integer          status
C
C The spectrum header is printed on the specified unit.
C-
       include 'spec_global.inc'
       include 'spec_header.inc'
       include 'spec_errors.inc'

C Local variables
       integer            width
       parameter         (width = 80)
       character          line*(width), string*(len_item_data)
       integer            i1, i2, n
C Functions
       integer            chr_lenb, chr_intlc

C check status on entry
       if (status.ne.0) return

C get header information
       call spi_get_header( id,
     *                      standard_info, item_name, item_data,
     *                      status )

       i1 = len_item_name + 3
       write (iunit,*) ' '
       line = ' '
       write (line,'(A,A)') 'Header for spectrum : ',
     *                      extra_title(1:chr_lenb(extra_title))
       write (iunit,*) line(1:chr_lenb(line))
       i2 = chr_lenb(line)
       line =
     *        '----------------------------------------'//
     *        '----------------------------------------'
       write (iunit,*) line(1:i2)
       write (iunit,*) ' '
       line = ' '
       do n=1,nitems
         line = ' '
         line(1:len_item_name) = item_name(n)
         call chr_chucas(line)
         line(i1-2:i1-2) = ':'
         string = item_data(n)(chr_intlc(item_data(n)):len_item_data)
         i2 = chr_lenb(string) - (width-i1)
         if (i2.gt.0) then
           line(i1:width) =  string(1:i2)
           write (iunit,*) line(1:chr_lenb(line))
           line = ' '
           line(i1-2:i1-2) = '_'
           line(i1:width) =  string(i2+1:chr_lenb(string))
           write (iunit,*) line(1:chr_lenb(line))
         else
           line(i1:width) =  string
           write (iunit,*) line(1:chr_lenb(line))
         end if
       end do
       write (iunit,*) ' '

999    if (status.ne.0) then
         call spi_err(status,'SPEC_HD_PRINT',' ')
       end if
       end
*+ spec_hd_set

       subroutine spec_hd_set( id, item, data, status)
C      -----------------------------------------------
C
C Set an item in the spectrum header
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C   name of item
       character*(*)    item
C   data for item
       character*(*)    data
C Updated:
C   status return code
       integer          status
C
C The named item in the spectrum header is set to the value DATA.
C If ITEM = HISTORY or COMMENT an extra entry is automatically added
C to the header; otherwise DATA will replace an existing value of
C ITEM in the header.  If ITEM is not present in the header a new
C entry will be added to the header.
C-
       include 'spec_global.inc'
       include 'spec_header.inc'
       include 'spec_errors.inc'

C Local varaibles
       integer         i, i1, i2, n
       logical         found_item
       character*20    string, test
C Functions
       integer         chr_lenb

C check status on entry
       if (status.ne.0) return

C get header information
       call spi_get_header( id,
     *                      standard_info, item_name, item_data,
     *                      status )
       string = item
       call chr_chucas(string)
       i1 = chr_lenb(string)
       if ( string(1:i1).eq.'HISTORY' .or.
     *      string(1:i1).eq.'COMMENT' ) then
         nitems = nitems + 1
         i = nitems
       else
         found_item = .false.
         do n=1,nitems
           test = item_name(n)
           call chr_chucas(test)
           i2 = chr_lenb(test)
           if (string.eq.test) then
             found_item = .true.
             i = n
           end if
         end do
         if (.not.found_item) then
           nitems = nitems + 1
           i = nitems
         end if
       end if

C set header item
       item_name(i) = string
       item_data(i) = data

C put spectrum header
       call spi_put_header( id,
     *                      standard_info, item_name, item_data,
     *                      status )

999    if (status.ne.0) then
         call spi_err(status,'SPEC_HD_SET',' ')
       end if
       end
*+ spec_put_data

       subroutine spec_put_data( id, ic, array, ndata, status)
C      -------------------------------------------------------
C
C Specify data for spectrum ID column IC in array ARRAY
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C   column number
       integer          ic
C   data array
       real*4           array(*)
C   number of data points
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
     *         '***(SPEC_PUT_DATA) ID not allocated ID = ',id
         status = ill_alloc
         call spi_err(status,'SPEC_PUT_DATA',string)
       end if
       if (status.ne.0) goto 999

C check inputs
       if (ic.le.0 .or. ic.gt.max_ncols) then
         status = ill_column
         write(string,'(A,I3)')
     *         '***(SPEC_PUT_DATA) Illegal column number IC = ',ic
         call spi_err(status,'SPEC_PUT_DATA',string)
         goto 999
       end if

C update header items
       spec_hdi(1,id) = ndata
       ncols = max(spec_hdi(2,id),ic)
       spec_hdi(2,id) = ncols

C update data
       do n=1,ndata
         spec_data(ic,n,id) = array(n)
       end do

999    if (status.ne.0) then
         call spi_err(status,'SPEC_PUT_DATA',' ')
       end if
       end
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
C   unit number of this file
       integer          iunit
C Updated:
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
C spectrum file use the access SCRATCH.
C-
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
         call io_nxtlun( iunit, status )
         control_records(unit,id) = iunit
       else
         status = ill_do_alloc
         goto 999
       end if

999    if (status.ne.0) then
         call spi_err( status, 'spi_alloc', 'Space allocation failed')
       end if
       end
*+ spi_check_id

       subroutine spi_check_id( id, status)
C      -----------------------------------
C
C Check the validity of the spectrum identifier ID
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
C The specified internal spectrum file identifier is checked
C-
       include 'spec_global.inc'
       include 'spec_control.inc'
       include 'spec_errors.inc'


C check status on entry
       if (status.ne.0) return
       if ( id.le.0 .or. id.gt.max_buff ) then
         status = ill_id
         call spi_err(status,'SPI_CHECK_ID','Illegal identifier')
       end if
       end
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
C
C
*+ spi_err

       subroutine spi_err(status,routine,message)
C      ------------------------------------------
C
C Report an error message for spectral system
C
C Given:
C    status code to interprete
       integer         status
C    routine name
       character*(*)   routine
C    message text
       character*(*)   message
C
C The error message is written to the error device or the output
C device depending on the value of STATUS. If STATUS=0 the action
C is to return without an error messahe,if STATUS .ne. 0
C then the IOLIB routine IO_WRERR is used to output a complete
C error message, and the routine name is appended for a trace-back.
*-
C
       integer    max_attention
       parameter (max_attention = 1)

       integer                              count_attention
       common /cmd_local_attention_count/   count_attention
C
C Local variables
C CHAR_BUFF       --    character string holding output message
C LEN_R           --    length of ROUTINE
C LEN_M           --    length of MESSAGE
C LEN_CB          --    length of CHAR_BUFF
       character*120     char_buff
       integer           len_r,len_m,len_cb,istat
C
       logical                    error_file_set
       common /cmd_local_err_set/ error_file_set

       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/cmd_lang_defn.inc'
       include '/mrao/include/cmd_lang_define.inc'

C determine string lengths
       len_r = chr_lenb(routine)
       len_m = chr_lenb(message)

       if (status.eq.iolib_ok) return

C set up local error messages if not done so already
       if (.not.error_file_set) then
         istat = 0
         call io_setmsg( '/mrao/spec/source/spec_error.inc' , istat)
         error_file_set = .true.
       end if

C take differing actions depending on STATUS
       if (status.eq.usr_break) then

         if (count_attention .lt. max_attention) then
           call io_wrout(cmd_attention)
         end if
         count_attention = count_attention + 1

       else if (status.ne.0 .and. status.ne.usr_break) then

C .. construct trace back message
         char_buff='('//routine(1:len_r)//') '//message(1:len_m)
         len_cb = chr_lenb(char_buff)
         call chr_chucas(char_buff(1:len_r+1))
         call io_wrerr(status,char_buff(1:len_cb))

       else

C .. construct a simple message
         char_buff='***('//routine(1:len_r)//') '//message(1:len_m)
         call io_wrout(char_buff(1:len_cb))

       end if
       end
*+ spi_get_header

       subroutine spi_get_header( id, info, items, values, status )
C      ------------------------------------------------------------
C
C get header information from an internal spectrum file
C
       implicit   NONE
C
C Given:
C   spectrum identifier
       integer          id
C Returned:
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
C The returned header information is got from / associated with the
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
         call spi_err(status,'SPI_GET_HEADER',string)
         goto 999
       end if

C get header information
       do i=1,max_standard_info
         info(i) = spec_hdi(i,id)
       end do
       do i=1,info(3)
         items(i) = spec_hdc(i,id)(1:len_item_name)
         values(i) =
     *   spec_hdc(i,id)(1+len_item_name:len_item_name+len_item_data)
       end do

999    if (status.ne.0) then
         call spi_err(status,'SPI_GET_HEADER',' ')
       end if
       end
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
*+ spi_read_data

       subroutine spi_read_data( id, status )
C      --------------------------------------
C
C Read data from open file into am internal file -- id
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
C Data is read for the spectrum with identifier id.
C-

       include 'spec_global.inc'
       include 'spec_data.inc'
       include 'spec_control.inc'
       include 'spec_errors.inc'

C Local variables
       integer      iunit, nend, ndata, nd, ncols, nc
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
       do nd = 1,nend
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

