C
C
*+ mapcat_read

       subroutine mapcat_read(imap,status)
C      --------------------------------
C
C Read a record from the catalogue file
C
C Input:
C    Catalogue entry
       integer      imap
C
C Returned:
C    Status
       integer       status
C
C Internal buffering is performed to increase the access rate for this
C routine to the map catalogue.
*-

       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       integer   n, nn, words

       if (status.ne.0) return
       if (imap.le.0 .or. imap.gt.max_cat_entries) then
         status = ill_catent
       else
         if (cat_unit.eq.0) then
           status = ill_catope
         else
           if (imap.ne.current_map) then
*             do n=1,num_buffers
*               do nn=1,cat_blocksize/4
*                 current_record(nn) = buffer_records(nn,n)
*               end do
*               if (imap.eq.current_map) goto 10
*             end do
             words = cat_blocksize/4
             call io_rdfile(cat_unit,imap,current_record,
     *                      words,status)
*             num_buffers = num_buffers+1
*             if (num_buffers.gt.3) num_buffers = 1
*             do nn=1,cat_blocksize/4
*               buffer_records(nn,num_buffers) = current_record(nn)
*             end do
           end if
         end if
       end if
10     continue
       call mapcat_err(status,'mapcat_read',' ')
       end
C
C
       subroutine mapcat_read1( imap, irec, status )
C      ---------------------------------------------
C
       integer    imap, irec(*), status
C
       include '/mrao/include/chrlib_functions.inc'
       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       integer   n
       call mapcat_read( imap, status )
       current_filename=
     *     current_filename(1:chr_lenb(current_filename))//char(0)
       current_source=
     *     current_source(1:chr_lenb(current_source))//char(0)
       current_program=
     *     current_program(1:chr_lenb(current_program))//char(0)
       call mapcat_read( imap, status )
       do n=1,cat_blocksize/4
          irec(n) = current_record(n) 
       end do
       end
