C
C
*+ mapcat_display

       subroutine mapcat_display(status)
C      ---------------------------------
C
C Display the map catalogue entries in full
C
C Returned:
C    Status
       integer      status
*-
       include '/mrao/include/iolib_functions.inc'
       include 'mapcat_cat.inc'

       character list*80
       integer   iout, n, ilist(max_cat_entries), nlist

C check status on entry
       if (status.ne.0) return

C read list of entries to display
       call io_getlst('Entries to list : ','*',list,
     *              ilist,max_cat_entries,nlist,status)
       if (status.ne.0) goto 999

C run through the list anddisplay each entry in turn
       call mapcat_open(status)
       call io_enqout(iout)
       write(iout,50)
       do n = 1,nlist
         call mapcat_read(ilist(n),status)
         if (status.ne.0) goto 999
         write(iout,100)current_map,current_source,current_filename,
     *                  current_program,current_minirt,
     *                  current_map_status
         if (io_attn(status)) goto 10
       end do
10     write (iout,*) ' '
 50    format(1x/1x,'Full Catalogue Entries Displayed ' /
     *           1x,80('-'))
100    format('  Entry : ',i3,'   Source  : ',a24/
     *           '                Filename: ',a48/
     *           '                Program : ',a8/
     *           '                UV-range: ',2i6,' : ',2i6,'  Size : ',
     *           i6,' x ',i6/
     *           '                Type    : ',i3,'  Blank : ',i10/
     *           '                Status-W: ',8i6)

999    call mapcat_err(status,'mapcat_display',' ')

       end
