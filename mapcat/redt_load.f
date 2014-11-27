*+ redt_load

       subroutine redt_load(imap,status)
C      ---------------------------------
C
C Load redtape for IMAP into the standard COMMON blocks
C
C Given:
C    Map entry
       integer      imap
C Returned:
C    Status word
       integer      status
C
C Load the redtape for map IMAP into the standard common
C blocks. Accessing redtape items is then possible provided the.
C MAPLIB common blocks are included in the user subprogram.
C Standard projection parameters are set by calling STPROJ.
C
*-
C
       include 'mapcat_stack.inc'
       include '/mrao/include/maplib_redtape.inc'
C
C Local variables
       integer   data_in_core, is_redt, ip_redt, iunit, istat
       integer   x_pages, x_style

C Check status on entry
       if (status.ne.0) return

C find pointers
       call mapcat_chk(imap,'READ',status)
       call stack_access(imap,'READ','REDTAPE',0,is_redt,status)
       call stack_enqdat(is_redt,data_in_core,status)
       call stack_enqpnt(is_redt,ip_redt,status)
       if (status.ne.0) goto 999

C load redtape
       if (data_in_core.eq.true) then
         call ldredt(redt_array(ip_redt),status)
         call enxrdt(x_pages,x_style,status)
         if (x_pages.ge.1) then
           call ldxrdt(redt_array(ip_redt+512),1,status)
         end if
         call stproj(iproj,1,usamp,skew,ramap,decmap,
     *               refdat,epoch,status)
       else
         call mapcat_mapopen(imap,'READ',iunit,status)
         call rdredt(iunit,0,status)
         call dpredt(redt_array(ip_redt),status)
         call enxrdt(x_pages,x_style,status)
         if (x_pages.ge.1) then
           call dpxrdt(redt_array(ip_redt+512),1,status)
         end if
         call mapcat_mapclose(imap,status)
         call stack_setdat(is_redt,true,status)
       end if
       if (status.ne.0) goto 999

C define current redtape in the common blocks
       current_redtape  = imap
C load additional redtape
       call mapcat_enqrec(imap,current_record,status)
999    call mapcat_err(status,'REDT_LOAD',' ')
       istat = 0
       call stack_io_setacc(is_redt,'CLEAR',istat)
C

       end
