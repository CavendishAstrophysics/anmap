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
