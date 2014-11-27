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
