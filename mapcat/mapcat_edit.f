C
C
*+ mapcat_edit

       subroutine mapcat_edit(status)
C      ------------------------------
C
C Edit the map catalogue entries
C
C Returned:
C    Status
       integer      status
*-

       include 'mapcat_cat.inc'
       include '/mrao/include/iolib_constants.inc'

       character string*18, command_line*120, st1*40, st2*40
       integer   len_cli, len_cf, len_st1, len_st2
       integer   iout, i, imap
       logical   display_entry

C check status on entry
       if (status.ne.0) return

C read entry to edit
       call io_enqcli(command_line,len_cli)
       display_entry = len_cli.eq.0
       call io_geti('Entry to edit (0=quit; -1=global-edit) : ',
     *           '0',imap,status)
       if (imap.eq.0) return
       if (imap.eq.-1)  goto 500
       if (status.ne.0) goto 999

C run through the list anddisplay each entry in turn
       call mapcat_open(status)
       call io_enqout(iout)
       call mapcat_read(imap,status)
       if (status.ne.0) goto 999
       if (display_entry) then
         write(iout,50)
         write(iout,100)current_map,current_source,current_filename,
     *                  current_program,current_minirt,
     *                  current_map_status
 50      format(1x/1x,'Full Catalogue Entries Displayed ' /
     *             1x,80('-'))
100      format('  Entry : ',i3,'   Source  : ',a24/
     *           '                Filename: ',a48/
     *           '                Program : ',a8/
     *           '                UV-range: ',2i6,' : ',2i6,'  Size : ',
     *           i6,' x ',i6/
     *           '                Type    : ',i3,'  Blank : ',i10/
     *           '                Status-W: ',8i6)
         write (iout,*) ' '
         call io_wrout('.. edit map_status word')
       end if
       do i = 1,length_map_status
         write(string,'(''Map_Status('',I1,'') : '')') i
         call io_geti(string,'*',current_map_status(i),status)
       end do
       if (display_entry) then
         call io_wrout('.. change filename of map')
       end if
       call io_getwrd('Map-filename (including directory) : ','*',
     *             current_filename,len_cf,status)
       call io_getwrd('Program-name : ','*',
     *             current_program,len_cf,status)
       call mapcat_write(imap,status)
       if (display_entry) then
         call io_wrout('.. entry replaced in catalogue')
         call io_wrout(' ')
       end if
       goto 900

500    continue

C .. apply global edit
       call io_getwrd('String-to-replace : ',' ',st1,len_st1,status)
       call io_getwrd('Replacement-string : ',' ',st2,len_st2,status)
       call mapcat_open(status)
       if (operating_system.ne.'UNIX') then
         call chr_chucas(st1)
         call chr_chucas(st2)
       end if
       do imap=1,max_cat_entries
          call mapcat_read(imap,status)
          if (operating_system.ne.'UNIX') then
            call chr_chucas(current_filename)
          end if
          call chr_chswap(current_filename,
     *                    st1(1:len_st1),st2(1:len_st2))
          call mapcat_write(imap,status)
       end do

900    continue
999    call mapcat_err(status,'mapcat_edit',' ')

       end
