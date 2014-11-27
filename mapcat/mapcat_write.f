C
C
*+ mapcat_write

       subroutine mapcat_write(imap,status)
C      ------------------------------------
C
C Write a record to the catalogue file
C
C Input:
C    Catalogue entry
       integer       imap
C
C Returned:
C    Status
       integer       status
*-

       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'

       if (status.ne.0) return
       if (imap.le.0 .or. imap.gt.max_cat_entries) then
         status = ill_catent
       else
         if (cat_unit.eq.0) then
           status = ill_catope
         else
           call io_wrfile(cat_unit,imap,current_record,
     *                 cat_blocksize/4,status)
         end if
         num_buffers = 0
       end if
       call mapcat_err(status,'mapcat_write',' ')
       end
C
C
       subroutine mapcat_write1( imap, irec, status )
C      ----------------------------------------------
C
       integer    imap, irec(*), status
C
       include '/mrao/include/chrlib_functions.inc'
       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       integer   n
       do n=1,cat_blocksize/4
          current_record(n) = irec(n)
       end do
       current_filename=current_filename(1:chr_lenb(current_filename))
       current_source=current_source(1:chr_lenb(current_source))
       current_program=current_program(1:chr_lenb(current_program))
       call mapcat_write( imap, status )
       end
