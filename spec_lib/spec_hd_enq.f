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
