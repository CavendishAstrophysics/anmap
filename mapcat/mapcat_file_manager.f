C
C
*+ mapcat_file_manager

       subroutine mapcat_file_manager(status)
C      --------------------------------------
C
C Perform management functions on temporary maps
C
C Returned:
C   Error status
       integer      status
C
C Perform a number of management functions on a filespace:
C   1) list owners of temporart files
C   2) delete expired temporary files
C-

       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/anmap_v7.5/include/anmap_sys_pars.inc'

C   local variables
C    option list and chosen option
       character*60   options(2), opt
C    file space description
       character      file_space*(iolen_file)
C    search tree for file specifications
       character      search_tree*(iolen_file)
C    current file name
       character      filename*(iolen_file)
C    length of text and unit numbers
       integer        len_text, iunit, iout
C    dates
       integer        date(3), test_date(3)
C    variables used by io_nxtfil routine
       integer        count, iobjx
C    local string variables
       character*20   st

       data options(1) /
     * 'list-owners .............. list owners of files'
     *                 /
       data options(2) /
     * 'delete-expired-files ..... delete all expired files'
     *                 /

C check status on entry
       if (status.ne.0) return

C prompt for file space
       call io_enqout(iout)
       call io_getwrd('File-space : ',root_maps_directory,
     *             file_space,len_text,status)
       st = 'ct'
       st(3:4) = file_char_sep//'*'
       call io_makfil(file_space(1:len_text),st(1:3),'pmap',search_tree,
     *             len_text)

C prompt for option
       call io_getopt('Option : ','List-Owners',options,2,opt,status)
       if (status.ne.0) goto 999

       if (chr_cmatch(opt,options(1))) then
         count = 0
         call io_opeout(iout,status)
         do while (status.eq.0)
           call io_nxtfil(search_tree(1:len_text),filename,
     *                 count,iobjx,status)
           if (status.eq.0) then
             if (operating_system.eq.'SINTRAN') then
               len_text=chr_lend(filename,';')
             else
               len_text=chr_lenb(filename)
             end if
             call chr_chctoi(filename(len_text-17:len_text-16),
     *                   test_date(1),
     *                   status)
             call chr_chctoi(filename(len_text-15:len_text-14),
     *                   test_date(2),
     *                   status)
             call chr_chctoi(filename(len_text-13:len_text-12),
     *                   test_date(3),
     *                   status)
             call opemap(iunit,filename(1:chr_lenb(filename)),'READ',
     *                   0,status)
             call rdredt(iunit,0,status)
             close (iunit)
             write(iout,10) test_date(1), test_date(2),
     *                      test_date(3), rtitle(1), rtownr
  10         format(1x,'File-',3i2.2,': ',A40,'  Owner: ',a)
           end if
         end do
         status = 0
         call io_setout(terminal_out)

       else if (chr_cmatch(opt,options(2))) then
         call util_enqdat(date,status)
         call io_getdat('Expiry-Date : ','*',date,status)
         date(3) = date(3) - 1900
         count = 0
         do while (status.eq.0)
           call io_nxtfil(search_tree(1:len_text),filename,
     *                 count,iobjx,status)
           if (status.eq.0) then
             len_text=chr_lend(filename,';')
             call chr_chctoi(filename(len_text-17:len_text-16),
     *                   test_date(1),
     *                   status)
             call chr_chctoi(filename(len_text-15:len_text-14),
     *                   test_date(2),
     *                   status)
             call chr_chctoi(filename(len_text-13:len_text-12),
     *                   test_date(3),
     *                   status)
             if ( (test_date(3)*10000+test_date(2)*100+test_date(1))
     *           .le. (date(3)*10000+date(2)*100+date(1)) ) then
               call opemap(iunit,filename(1:chr_lenb(filename)),'READ',
     *                     0,status)
               call rdredt(iunit,0,status)
               close (iunit)
               write(iout,30) rtitle(1), rtownr
  30           format(1x,'Deleting: ',A40,'  Owner: ',a)
               call io_delfil(filename(1:len_text),0,status)
             end if
           end if
         end do
         status = 0

       end if

999    call mapcat_err(status,'Catalogue-System-Manager',' ')

       end
