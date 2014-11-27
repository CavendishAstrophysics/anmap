C
C
*+ mapcat_list

       subroutine mapcat_list(ilist,nlist,iout,option,status)
C      ------------------------------------------------------
C
C List the map catalogue entries in a formated style
C
C Input:
C    list of entries to list
       integer      ilist(*)
C    number of entries in the list
       integer      nlist
C    output unit
       integer      iout
C    option
       integer      option
C Returned:
C    Status
       integer      status
C
C List the catalogue entries in a formated fashion to unit iout.
C Options are determined by option:
C     option = 1      brief format: entry, source, program, size, access
C
*-

       include 'mapcat_cat.inc'

C counters and count of catalogue entries analysed
       integer       n
C characters representing special options
       character*1   char_1, char_2, char_3

C check status on entry
       if (status.ne.0) return

C run through the list and display each entry in turn
       call mapcat_open(status)
       if (option.eq.1) then
         write(iout,50) cat_file
       else
         call io_wrout('*** Unknown option in LIST-CATALOGUE-ENTRIES')
         return
       end if
       if (nlist.gt.0) then
         do n = 1,nlist
           call mapcat_read(ilist(n),status)
           if (status.ne.0) goto 999
           if (current_map_status(ip_data).eq.true) then
             char_1 = ' '
             char_2 = ' '
             char_3 = ' '
             if (current_map_status(ip_access).eq.access_read)then
               char_1 = 'R'
             elseif (current_map_status(ip_access).eq.access_write)then
               char_1 = 'W'
             elseif (current_map_status(ip_access).eq.access_scratch)
     *         then
               char_1 = 'S'
             elseif (current_map_status(ip_access).eq.access_create)
     *         then
               char_1 = 'C'
             end if
             if (current_map_status(ip_page).eq.true) char_2 = 'T'
             if (current_map_status(ip_tape).eq.true) char_3 = 'V'

             write(iout,100)current_map,current_source,
     *                      current_program,
     *                      current_minirt(5),current_minirt(6),
     *                      char_1, char_2, char_3
           end if
         end do
       else
           call io_wrout(' ')
           call io_wrout('.. no maps in matching search expression ')
       end if
       write (iout,*) ' '
 50    format(1x/1x,'Catalogue entries for catalogue: ',A/
     *              1x,'Entry   Source                  ',
     *              '   Program         Size     Access'/
     *           1x,80('-'))
100    format(1x,i3,'     ',a24,'   ',a8,'   ',2i6,'   ',
     *        a1,' ',a1,' ',a1)

999    call mapcat_err(status,'mapcat_list',' ')

       end
