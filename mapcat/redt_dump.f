C
C
*+ redt_dump

       subroutine redt_dump(imap,status)
C      ---------------------------------
C
C Store the current redtape with a map
C
C Input:
C    Map catalogue entry
       integer         imap
C Returned:
C    Status
       integer         status
C
C The current contents of the redtape common blocks are loaded
C into internal store associated with map IMAP. STATUS should
C be zero on entry.
*-
       include 'mapcat_stack.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'
C
       integer   ip_redt, is_redt, x_pages, x_style, iunit, istat
       integer   len_direc,map_status(10),date(3),time(3)
       character directory*(iolen_dir)

C integer flag indicating output map should have extra redtape
       integer                    extra_redtape_on_output
       common /mapcat_local_redt/ extra_redtape_on_output

       if (status.ne.0) return

C find pointers
       call stack_access(imap,'WRITE','REDTAPE',0,is_redt,status)
       call stack_enqpnt(is_redt,ip_redt,status)
       if (status.ne.0) goto 999

C load redtape to buffer
       call dpredt(redt_array(ip_redt),status)
       call enxrdt(x_pages,x_style,status)
       if (x_pages.ge.1) then
         if (x_pages.gt.1) then
            call io_wrout(
     *           '*** WARNING: some pages of extra redtape lost')
         end if
         call dpxrdt(redt_array(ip_redt+512),1,status)
       end if
       if (status.ne.0) goto 999

C load extra redtape items
       call enminirt(current_minirt,status)

       if (chr_cmatch(current_filename,'PAGEING-FILE')) then
         call mapcat_enqdefdir(directory,status)
         len_direc = chr_lenb(directory)
C .. construct file name for paged file (time and date coded)
         current_filename = ' '
         call util_enqdat(date)
         call util_enqtim(time)
         date(3) = date(3) - 1900
         if (operating_system.eq.'SINTRAN') then
           write(current_filename,
     *           fmt='(''('',a,'')ct-'',3I2.2,''-'',3I2.2,'':pmap'')')
     *           directory(1:len_direc),date,time
         else if (operating_system.eq.'UNIX') then
           write(current_filename,
     *     fmt='(a,''/ct_'',3I2.2,''_'',3I2.2,''_'',I3.3,''.pmap'')')
     *           directory(1:len_direc),date,time,imap
         end if
         call mapcat_enqst(imap,map_status,status)
         map_status(ip_page) = true
         call mapcat_setst(imap,map_status,status)
       end if
       call mapcat_setrt(imap,current_filename,current_minirt,status)
       call mapcat_setsr(imap,current_source,current_program,status)
       call mapcat_mapopen(imap,'WRITE',iunit,status)
       if (extra_redtape_on_output.eq.1) then
         call stxrdt(1,1,status)
       end if

       call wrredt(iunit,0,status)
       call mapcat_mapclose(imap,status)
       call stack_setdat(is_redt,true,status)
       istat = 0
       call stack_io_setacc(is_redt,'CLEAR',istat)
       extra_redtape_on_output = 0
999    call mapcat_err(status,'REDT_DUMP','redtape not saved')

       end

