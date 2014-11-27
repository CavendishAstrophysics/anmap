C
C
*+ mapcat_exist

       subroutine mapcat_exist(file,imap,status)
C      -----------------------------------------
C
C Check whether the specified file is in the catalogue
C
C Give:
C   name of file to check for existence in the catalogue
       character*(*)   file
C Returned:
C   catalogue entry (=0 if not in catalogue)
       integer         imap
C   status
       integer         status
C
C The catalogue is checked to see if the map file FILE is already
C allocated.
C
C The file name must be an exact match for this to be true.  If the
C map is already in the catalogue IMAP returns the associated catalogue
C entry, if not then IMAP is returned with the value zero.
*-

       include 'mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C counter
       integer       n
C string lengths etc
       integer       len_f, len_cf

C check status on entry
       if (status.ne.0) return

C run through the list and test each entry in turn
       len_f = chr_lenb(file)
       imap = 0
       call mapcat_open(status)
       do n = 1,max_cat_entries
         call mapcat_read(n,status)
         if (status.ne.0) goto 999
         if (current_map_status(ip_data).eq.true) then
           len_cf = chr_lenb(current_filename)
           if (operating_system.ne.'UNIX') then
             call chr_chucas(current_filename(1:len_cf))
           end if
           if (current_filename(1:len_cf).eq.file(1:len_f)) then
             imap = n
             goto 10
           end if
         end if
       end do
10     continue
999    call mapcat_err(status,'mapcat_exist',' ')

       end
