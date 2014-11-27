C
C
*+ map_end_alloc

       subroutine map_end_alloc(imap,map_array,status)
C      -----------------------------------------------
C
C End the allocation made to map IMAP
C
C Input:
C    Map entry
       integer       imap
C    Map data
       real*4        map_array(*)
C Returned
C    Status word
       integer       status
C
C End the allocation to map IMAP.  If the map was opened for write
C explicit IO is now performed.  This routine should always be called
C to end the access requested to a particular map whatever the requested
C access state.
*-
       include 'mapcat_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C Debugging function
       logical       cmd_dblev

C Local variables
       logical       perform_io,  report_mode
       integer       iout, istat, idate(3), map_status(10)
       integer       len_source, len_program
       character     source*80, program*20
       character     filename*(iolen_file)
       integer       is_map, ipm, ipr, mode, iunit
       real*4        zmnx(10)
       integer       izmnx(10), minirt(10)

C check status on entry
       istat = 0
       if (cmd_dblev(7)) then
         print *,'..(MAPCAT_END_ALLOC) status on entry = ',status
       elseif (cmd_dblev(5)) then
         if (status.ne.0) then
           print *,'.. Closing map after error status = ',status
         end if
       end if
       if (status.ne.0) then
         perform_io = .false.
       else
         perform_io = .true.
       end if

C check imap --- if it is not valid return (no error message)
       call mapcat_chk(imap,'NONE',istat)
       if (status.ne.0 .and. istat.ne.0) then
         istat = 0
         goto 999
       end if
       call mapcat_enqst(imap,map_status,istat)
       if (istat.ne.0) return

       if (map_status(ip_access).eq.access_scratch) then
C .. map opened as a scratch map -- remove it from catalogue and stack
         call mapcat_acc(imap,'CLEAR',istat)
         call stack_remove(imap,istat)

       else if (map_status(ip_access).eq.access_read) then
C .. map open for read -- clear all access states to this map
         call mapcat_mapclose(imap,istat)
         call mapcat_acc(imap,'CLEAR',istat)
         call stack_clracc(imap,istat)

       else if (map_status(ip_access).eq.access_create) then
C .. map open for create access (map created, no data written)
         map_status(ip_data) = true
         call mapcat_setst(imap,map_status,istat)
         if (istat.ne.0) goto 999
         call mapcat_enqrt(imap,filename,minirt,istat)
         call redt_setfile(filename,istat)
         call mapcat_enqsr(imap,source,program,istat)
         call enhist(idate,program,istat)
C ... write out redtape
         call redt_dump(imap,istat)
C ... close map
         call mapcat_mapclose(imap,istat)
C ... update the source name in the catalogue
         call mapcat_setsr(imap,source,program,istat)
         if (istat.ne.0) goto 999

       else if (map_status(ip_access).eq.access_write) then
C .. map open for write access
C .. find mode of access -- take action on error
         call stack_enqmode(imap,mode,ipm,ipr,iunit,istat)
         map_status(ip_data) = true
         call mapcat_setst(imap,map_status,istat)
         if (istat.ne.0) goto 999

         if (.not.perform_io) then
C ... if an error has occured in the calling routine get rid of this map
           call mapcat_mapclose(imap,istat)
           call mapcat_delete(imap,.false.,istat)
           call stack_remove(imap,istat)

         else
C ... routine completed OK -- update redtape and write out data
           call enminirt(minirt,istat)
           if (mode.eq.mode_sequential) then
C .... find max min from cummulative results
             call stack_enqmnx(imap,zmnx,izmnx,istat)
           else
C .... scan map for max and min and note data present
             call stack_enqmap(imap,is_map,istat)
             call stack_setdat(is_map,true,istat)
             call scnmap2(minirt,map_array(ipm),minirt(1),
     *                    zmnx,izmnx,istat)
             if (istat.ne.0) goto 999
           end if
C ... update scale factor, and text of redtape
           call stredt(minirt,3,istat)
           call stscal(zmnx,izmnx,istat)
           call mapcat_enqrt(imap,filename,minirt,istat)
           call redt_setfile(filename,istat)
           call mapcat_enqsr(imap,source,program,istat)
           call enhist(idate,program,istat)
C ... write out map data if required
           if (mode.eq.mode_direct) then
             call mapcat_mapopen(imap,'WRITE',iunit,istat)
             call wrmap(iunit,map_array(ipm),istat)
           end if
C ... write out redtape
           call redt_dump(imap,istat)
C ... close map
           call mapcat_mapclose(imap,istat)
           call mapcat_acc(imap,'CLEAR',istat)
C ... update the source name in the catalogue
           call mapcat_setsr(imap,source,program,istat)
           call stack_clracc(imap,istat)
           if (istat.ne.0) goto 999
C ... report addition of map to the catalogue
           call mapcat_enqrm(report_mode)
           call mapcat_setlres(imap)
           if (report_mode) then
             call io_enqout(iout)
             len_source  = chr_lenb(source)
             len_program = chr_lenb(program)
             write(iout,'(1x,a,i3,a,a)')
     *            '.. Catalogue entry ',imap,' allocated to ',
     *            source(1:len_source)//'-'//program(1:len_program)
           end if
C ... set the default map to our map
           call map_setdef(imap,istat)
         end if
       end if

999    call mapcat_err(istat,'MAP_END_ALLOC',' ')
       if (cmd_dblev(7)) then
         print *,'..(MAPCAT_END_ALLOC) status on exit = status'
       end if

       end
C
C
       subroutine mapcat_setlres( i )
C      ------------------------------
C
       integer    i, n
       integer    ilist(10)
       common /mapcat_lres/ ilist

       do n=10,2,-1
         ilist(n) = ilist(n-1)
       enddo
       ilist(1) = i
       end
C
C
       subroutine mapcat_initlres
C      --------------------------
C
       integer    n
       integer    ilist(10)
       common /mapcat_lres/ ilist

       do n=1,10
         ilist(n) = 0
       enddo
       end
C
C
       subroutine mapcat_enqlres( list )
C      ---------------------------------
C
       integer    list(10)
       integer    n
       integer    ilist(10)
       common /mapcat_lres/ ilist

       do n=1,10
         list(n) = ilist(n)
       enddo
       end
