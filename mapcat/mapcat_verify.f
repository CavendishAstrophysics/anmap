C
C
*+ mapcat_verify

       subroutine mapcat_verify(status)
C      --------------------------------
C
C Verify the map catalogue
C
C Returned:
C    Status
       integer         status
C
*-

       include 'mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C counter
       integer       n
C local status variable
       integer       istat
C local string variable
       character     string*(iolen_file)

C check status on entry
       if (status.ne.0) return

C run through the list and test each entry in turn
       call mapcat_open(status)
       do n = 1,max_cat_entries
         call mapcat_read(n,status)
         if (status.ne.0) goto 999
         if (current_map_status(ip_data).eq.true) then
           istat = 0
           call io_namfil(
     *             current_filename(1:chr_lenb(current_filename)),
     *             string,0,istat)
           if (istat.ne.0) then
             call mapcat_remove(n,.false.,status)
           end if
         end if
       end do

999    call mapcat_err(status,'mapcat_verify',' ')

       end
