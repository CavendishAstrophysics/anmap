**
*  Map Catalogue and Stack Handling Routines
*  =========================================
C
C P.Alexander,MRAO:  Version 3.0 July/August 1990
C Last updated PA :              2/04/91
C
*-

*$ 1) Redtape Routines
*  -------------------
C
C
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
       include '/mrao/anmap/include/mapcat_stack.inc'
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

       end
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
       include '/mrao/anmap/include/mapcat_stack.inc'
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
     *           fmt='(a,''/ct_'',3I2.2,''_'',3I2.2,''.pmap'')')
     *           directory(1:len_direc),date,time
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
C
C
*+ redt_setxrdt

       subroutine redt_setxrdt( status )
C      ---------------------------------
C
C Returned:
C   error return code
       integer    status
C
C Request next output map will be created with a page of extra redtape
C-
C integer flag indicating output map should have extra redtape
       integer                    extra_redtape_on_output
       common /mapcat_local_redt/ extra_redtape_on_output
       extra_redtape_on_output = 1
       end
C
C
*+ redt_enqcurr

       subroutine redt_enqcurr(ic_redt,status)
C      ---------------------------------------
C
C Return the current redtape
C
C Returned:
C    catalogue entry of the current redtape
        integer   ic_redt
C    status word
        integer   status
C
C The map catalogue entry of the redtape currenty loaded in the standard
C redtape buffer is returned.  The standard redtape buffer may be used
C by including the file: (library)maplib-redtape:incl
C
C-
       include '/mrao/anmap/include/mapcat_stack.inc'

       if (status.ne.0) return
       ic_redt = current_redtape
       call mapcat_err(status,'redt_enqcurr',' ')
       end
C
C
*+ redt_setfile

       subroutine redt_setfile(file,status)
C      ------------------------------------
C
C Set the current value of the file name
C
C Given:
C    file name
       character*(*)   file
C Returned:
C    status word
       integer         status
C
C The current value of the map filename is reset
C-
       include '/mrao/anmap/include/mapcat_stack.inc'

       if (status.ne.0) return
       current_filename = file
       call mapcat_err(status,'redt_setfile',' ')
       end
C
C
*+ redt_setrt

       subroutine redt_setrt(minirt,status)
C      ------------------------------------
C
C Set the current value of the mini redtape
C
C Given:
C    mini redtape
       integer         minirt(*)
C Returned:
C    status word
       integer         status
C
C The current value of the mini redtape is reset
C-
       include '/mrao/anmap/include/mapcat_stack.inc'
       integer   i

       if (status.ne.0) return
       do i=1,8
         current_minirt(i) = minirt(i)
       end do
       call mapcat_err(status,'redt_setrt',' ')
       end
C
C
*+ redt_setsr

       subroutine redt_setsr(source,program,status)
C      --------------------------------------------
C
C Define the current source name for the map file
C
C Given:
C   Source name
       character*(*)      source
C   Program name
       character*(*)      program
C Returned:
C    Status word
       integer            status
C
C Define the  source and program names for the current redtape
*-
       include '/mrao/anmap/include/mapcat_stack.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer           len_source, len_program

       if (status.ne.0) return

       len_source = chr_lenb(source)
       if (len_source.gt.0) then
         current_source = source
       end if
       len_program = chr_lenb(program)
       if (len_program.gt.0) then
         current_program = program
       end if
       call mapcat_err(status,'redt_setsr',' ')

       end
C
C
*+ redt_comp

       subroutine redt_comp(imap1,imap2,iwarn,status)
C      ----------------------------------------------
C
C To compare the redtapes for two maps for compatability.
C
C Given:
C   Map entry for first redtape
       integer     imap1
C   Map entry for second redtape
       integer     imap2
C Updated:
C   Report control indicator
C     On entry:-
C          report control  <0 no output report
C                          >0 report only >IWARN
C     On exit:-
C          warning of soft errors
C                          =0   all OK
C                           1   Map names
C                           2   1950.0 map centres
C                           3   Cell size
C                           4   UV range
       integer     iwarn
C
C Returned:
C   Status word
       integer     status
C
C The redtapes of the maps are compared and checked for compatability.
C If the initial setting of IWARN is <0 output will not be to the
C terminal, IWARN is set. IWARN=0 all warnings are output. If IWARN>0
C only erros whose code >IWARN are output. If IWARN .ne. 0 on entry
C then IWARN must be tested on exit. STATUS is solely an error return
C
*-
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       integer    nerror, iout, i, isave
       real*4     usamp1, vsamp1, usamp2, vsamp2
       real*8     ramap1, ramap2, decmap1, decmap2

       character  title1*40, title2*40

       integer    iuv1(4), iuv2(4), level_report
       logical    report, in_error

C test initial value of STATUS and set flags determined by IWARN
       if (status.ne.0) return
       report = iwarn.ge.0
       level_report = abs(iwarn)
       iwarn = 0

C save current redtape
       call redt_enqcurr(isave,status)

C recover redtape items
       call redt_load(imap1,status)
       title1 = rtitle(1)
       call iuv_load(iuv1,status)
       ramap1 = ramap
       decmap1 = decmap
       usamp1 = usamp
       vsamp1 = vsamp
       call redt_load(imap2,status)
       title2 = rtitle(1)
       call iuv_load(iuv2,status)
       ramap2 = ramap
       decmap2 = decmap
       usamp2 = usamp
       vsamp2 = vsamp
       if (status.ne.0) then
         call cmd_err(status,'redt_comp','comparison failed')
         return
       end if

C restore redtape
       if (isave.gt.0) then
         call redt_load(isave,status)
       end if

C do comparison
       nerror = 0
       call io_enqout(iout)
       if (.not.chr_cmatch(title1,title2)) then
         nerror = nerror + 1
         iwarn = 1
         if (report.and.level_report.le.1) then
           write(iout,10)
  10       format(' ***(REDT_COMPARE) Map/Source title differ')
         end if
       end if
       if (abs(ramap1-ramap2).gt.1.0D-10 .or.
     *     abs(decmap1-decmap2).gt.1.0D-10)   then
         iwarn = 2
         nerror = nerror + 1
         if (report.and.level_report.le.2) then
           write(iout,80)
80         FORMAT(' ***(REDT_COMPARE) 1950.0 map centres differ')
         end if
       end if
       in_error=.false.
       if (abs(usamp1-usamp2).gt.1.0e-4 .or.
     *     abs(vsamp1-vsamp2).gt.1.0e-4)   then
         nerror = nerror + 1
         iwarn =  3
         if (report.and.level_report.le.3) then
           write(iout,100)usamp1,vsamp1,usamp2,vsamp2
100        FORMAT(' ***(REDT_COMPARE) Map-Samplings differ'/
     *            ' ...    Map1 = ',2F12.4,'  Map2 = ',2F12.4)
         end if
       end if
       do i=1,4
         in_error=iuv1(i).ne.iuv2(i) .or. in_error
       end do
       if (in_error) then
         iwarn = 4
         nerror = nerror + 1
         if (report.and.level_report.le.4) then
           write(iout,90)iuv1,iuv2
90         FORMAT(' ***(REDT_COMPARE) UV-ranges differ'/
     *            ' ...    MAP1 = ',4I6,'  Map2 = ',4I6)
         end if
       end if
       end
C
C
*+ redt_update

       subroutine redt_update(type,ipoln,freq,name,unit,status)
C      --------------------------------------------------------
C
C Update the current redtape as required
C
C Given:
C   new map type
       character*(*)    type
C   new polarization code
       integer          ipoln
C   new map frequency
       real*4           freq
C   new name of map quantity
       character*(*)    name
C   new unit of map quantity
       character*(*)    unit
C Returned:
C   Status word
       integer          status
C
C Update the current redtape as required -- select the default value to
C not update a particular item:
C
C Parameter       Description            Default value
C ----------------------------------------------------
C   type          source type            ' '
C   ipoln         polarization code      -1
C   freq          frequency (MHz)        < 0
C   name          map quantity name      ' '
C   unit          map quantity unit      ' '
*-
       include '/mrao/anmap/include/mapcat_stack.inc'
       include '/mrao/include/chrlib_functions.inc'

       character*40      string
       integer           len_type, i1
       real*4            en_freq
       integer           en_ipoln
       character*16      en_name, en_unit

C check status on entry
       if (status.ne.0) return

C update type
       len_type = chr_lenb(type)
       len_type = max(4,len_type)
       if (len_type.gt.0) then
         i1 = chr_ilstc(current_source,'-')
         string = current_source(1:i1)
         current_source = string(1:i1)//type(1:len_type)
         call chr_chucas(current_source)
       end if
       call cmd_err(status,'redt_update',' ')

C update other redtape items
       call entype(en_freq,en_ipoln,en_name,en_unit,status)
       if (freq.gt.0.0) en_freq = freq
       if (ipoln.gt.0)  en_ipoln = ipoln
       if (chr_lenb(name).gt.0) en_name = name
       if (chr_lenb(unit).gt.0) en_unit = unit
       call sttype(en_freq,en_ipoln,en_name,en_unit,status)

       end

C
C
*$ 2) High-Level Map System Routines Access Catalogue, Stack and Data
*  ------------------------------------------------------------------
C
C
*+ map_enqdef

       subroutine map_enqdef(imap,status)
C      ----------------------------------
C
C Enquire the default map
C
C Returned:
C   Default map
       integer         imap
C   Status word
       integer         status
C
C The current value of the default map is returned
C
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
C
       if (status.ne.0) return
       imap =  default_map
       call mapcat_err(status,'map_enqdef',' ')
       end
C
C
*+ map_setdef

       subroutine map_setdef(imap,status)
C      ----------------------------------
C
C Set the default map
C
C Given:
C   Map entry to become the default map
       integer         imap
C Returned:
C   Status word
       integer         status
C
C Define the default map by catalogue entry number.  IMAP is checked
C for a valid map to use as the default map.
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
C
       if (status.ne.0) return
C check validity of IMAP
       call mapcat_chk(imap,'NONE',status)
       if (status.eq.0) then
         default_map=imap
       end if
       call mapcat_err(status,'map_setdef','Default-Map not reset')
       end
C
C
*+ map_alloc_out

       subroutine map_alloc_out(sizeu,sizev,access,imap,ip_map,status)
C      ---------------------------------------------------------------
C
C Allocate output maps required in the remainder of a routine
C
C Given:
C   Size of output map in U and V
       integer        sizeu, sizev
C   Access mode required -- DIRECT or SEQUENTIAL
       character*(*)  access
C Returned:
C   Map catalogue entry
       integer        imap
C   Map pointer
       integer        ip_map
C   Status word
       integer        status
C
C Output maps for a routine are allocated.
C
C Two access modes are supported:
C    SEQUENTIAL -- line by line; the entire image is not needed at once
C    DIRECT     -- full image in core
C
C If DIRECT access is specified then the pointer to the start of the
C allocated space is returned in ip_map, if the access mode is
C SEQUENTIAL calls to MAP_ROW_READ and MAP_ROW_WRITE will be required
C nested in the sequential access loop.
C
C SIZEU, SIZEV are the sizes of the image in the U and V direction.  If
C either is zero then the values are obtained from the current redtape.
C
C On completion a call to MAP_END_ALLOC is required to tidy the output
C and indeed write out the image if the access requested is DIRECT.
C
C PA, 11/7/90
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer     is_map, minirt(10), iunit, ip_redt, mode
       real*4      zmnx(2)
       integer     izmnx(4)

C test STATUS on entry
       if (status.ne.0) return

C find suitable output map and allocate
       call mapcat_next(imap,status)
       call mapcat_acc(imap,'WRITE',status)
       if (status.ne.0) then
         call mapcat_err(status,'map_alloc_out',
     *                   'Allocation of output maps failed')
         goto 999
       end if

C set size if not specified
       call enminiRT(minirt,status)
       if (sizeu.le.0 .or. sizev.le.0) then
         sizeu = minirt(5)
         sizev = minirt(6)
       end if

C create output map
       call redt_setfile('PAGEING-FILE',status)
       if (sizev.ne.minirt(6)) then
         minirt(6) = sizev
         minirt(3) = sizev/2
         minirt(4) = -sizev+minirt(3)+1
       end if
       if (sizeu.ne.minirt(5)) then
         minirt(5) = sizeu
         minirt(1) = -sizeu/2
         minirt(2) = sizeu+minirt(1)-1
       end if
       call stredt(minirt,3,status)
       zmnx(1) =  1.0
       zmnx(2) = -1.0
       izmnx(1)= 0
       izmnx(2)= 0
       izmnx(3)= 0
       izmnx(4)= 0
       call stscal(zmnx,izmnx,status)
       minirt(7) = 3
       call redt_setrt(minirt,status)
       call redt_dump(imap,status)

C allocate space as required:
       if (chr_cmatch(access,'DIRECT').or.
     *     chr_cmatch(access,'SEQUENTIAL')) then
C .. if access mode is DIRECT then find space
         call stack_access(imap,'WRITE','MAP',sizeu*sizev,is_map,status)
         call stack_enqpnt(is_map,ip_map,status)
         call stack_enqmode(imap,mode,ip_map,ip_redt,iunit,status)
         call stack_dpredt(ip_redt,status)
       else
         status = ill_unmode
         goto 999
       end if

999    call mapcat_err(status,'map_alloc_out','Space allocation failed')

       end
C
C
*+ map_alloc_new

       subroutine map_alloc_new(sizeu,sizev,imap,filename,status)
C      ----------------------------------------------------------
C
C Allocate and create output map, but do not provide access to it
C
C Given:
C   Size of output map in U and V
       integer        sizeu, sizev
C Returned:
C   Map catalogue entry
       integer        imap
C   File name associated with this map
       character*(*)  filename
C   Status word
       integer        status
C
C An output map for a routine is created and an entry added to the
C map catalogue.  No space is allocated in the stack and no pointer
C to the map is returned, the map file name is returned by this
C routine.   The primary use of this routine is to create a map
C so that it may be accessed by an OFFLINE process.
C
C SIZEU, SIZEV are the sizes of the image in the U and V direction.  If
C either is zero then the values are obtained from the current redtape.
C
C On completion a call to MAP_END_ALLOC is required to tidy the output
C and update the redtape as far as possible.
C
C PA, 28/3/91
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer     minirt(10)
       real*4      zmnx(2)
       integer     izmnx(4)

C test STATUS on entry
       if (status.ne.0) return

C find suitable output map and allocate
       call mapcat_next(imap,status)
       call mapcat_acc(imap,'CREATE',status)
       if (status.ne.0) then
         call mapcat_err(status,'map_alloc_new',
     *                   'Allocation of new map failed')
         goto 999
       end if

C set size if not specified
       call enminiRT(minirt,status)
       if (sizeu.le.0 .or. sizev.le.0) then
         sizeu = minirt(5)
         sizev = minirt(6)
       end if

C create output map
       call redt_setfile('PAGEING-FILE',status)
       if (sizev.ne.minirt(6)) then
         minirt(6) = sizev
         minirt(3) = sizev/2
         minirt(4) = -sizev+minirt(3)+1
       end if
       if (sizeu.ne.minirt(5)) then
         minirt(5) = sizeu
         minirt(1) = -sizeu/2
         minirt(2) = sizeu+minirt(1)-1
       end if
       call stredt(minirt,3,status)
       zmnx(1) =  1.0
       zmnx(2) = -1.0
       izmnx(1)= 0
       izmnx(2)= 0
       izmnx(3)= 0
       izmnx(4)= 0
       call stscal(zmnx,izmnx,status)
       minirt(7) = 3
       call redt_setrt(minirt,status)
       call redt_dump(imap,status)
       call mapcat_enqrt(imap,filename,minirt,status)

999    call mapcat_err(status,'map_alloc_new','Map creation failed')

       end
C
C
*+ map_alloc_toout

       subroutine map_alloc_toout(imap,access,map_array,
     *                             imapo,ip_mapo,status)
C      -------------------------------------------------
C
C Allocate map data IMAP to the new output map imapo
C
C Given:
C   Input map entry
       integer        imap
C   Access mode required -- DIRECT or SEQUENTIAL
       character*(*)  access
C Updated:
C   Map data array
       real*4         map_array(*)
C Returned:
C   Map catalogue entry to output map
       integer        imapo
C   Map pointer to output map
       integer        ip_mapo
C   Status word
       integer        status
C
C Input map data is allocated to a new output map.
C
C The only access mode currently supported is:
C    DIRECT     -- full image in core
C
C The pointer to the start of the allocated space is returned in ip_mapo.
C The map data for map IMAP will have been read into this space.
C
C On completion a call to MAP_END_ALLOC is required to tidy the output
C and write out the image.
C
C PA, 1/8/90
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer     minirt(10), iunit
       integer     is_mapi, is_mapo, ip_mapi

C check status on entry
       if (status.ne.0) return

C check existing catalogue entry
       call mapcat_chk(imap,'NONE',status)
       if (status.ne.0) goto 999

C if no access state previously defined then make sure map available
C for read
       call mapcat_read(imap,status)
       if (current_map_status(ip_access).eq.access_clear) then
         call mapcat_chk(imap,'READ',status)
       end if

C find if map is in core alread
       call stack_enqmap(imap,is_mapi,status)
       ip_mapi = -1
       if (is_mapi.gt.0) then
         call stack_enqpnt(is_mapi,ip_mapi,status)
       end if

C find suitable output map and allocate
       call redt_load(imap,status)
       if (status.ne.0) goto 999
       call mapcat_next(imapo,status)
       call mapcat_acc(imapo,'WRITE',status)
       if (status.ne.0) then
         call mapcat_err(status,'map_alloc_out',
     *                   'Allocation of output maps failed')
         goto 999
       end if

C set size
       call enminiRT(minirt,status)
C create output map
       call redt_setfile('PAGEING-FILE',status)
       minirt(7) = 3
       call redt_setrt(minirt,status)
       call redt_dump(imapo,status)

C allocate space as required:
       if (chr_cmatch(access,'DIRECT') .or.
     *     chr_cmatch(access,'SEQUENTIAL')) then

         if (is_mapi.lt.0) then
C .. IMAP data not in core allocate space and read data
           call stack_access(imapo,'WRITE','MAP',
     *                       minirt(5)*minirt(6),is_mapo,status)
           call stack_enqpnt(is_mapo,ip_mapo,status)
           call mapcat_mapopen(imap,'READ',iunit,status)
           call rdmap(iunit,map_array(ip_mapo),status)
           call mapcat_mapclose(imap,status)

         else
C .. IMAP data in core -- reallocate to imapo
           call stack_access(imapo,'ALLOCATE','MAP',imap,
     *                       is_mapo,status)
           call stack_enqpnt(is_mapo,ip_mapo,status)

         end if
       else
         status = ill_unmode
         goto 999
       end if

999    call mapcat_err(status,'map_alloc_out','Space allocation failed')

       end
C
C
*+ map_alloc_scr

       subroutine map_alloc_scr(sizeu,sizev,access,imap,ip_map,status)
C      ---------------------------------------------------------------
C
C Allocate scratch maps required in the remainder of a routine
C
C Given:
C   Size of scratch map in U and V
       integer        sizeu, sizev
C   Access mode required -- DIRECT
       character*(*)  access
C Returned:
C   Map catalogue entry
       integer        imap
C   Map pointer
       integer        ip_map
C   Status word
       integer        status
C
C Scratch maps for a routine are allocated.
C
C Two access modes are supported in general:
C    SEQUENTIAL -- line by line; the entire image is not needed at once
C    DIRECT     -- full image in core
C HOWEVER, only DIRECT access is sensible for scratch files and
C SEQUENTIAL mode is therefore faulted.
C
C If DIRECT access is specified then the pointer to the start of the
C allocated space is returned in ip_map, if the access mode is
C SEQUENTIAL calls to MAP_ROW_READ and MAP_ROW_WRITE will be required
C nested in the sequential access loop.
C
C SIZEU, SIZEV are the sizes of the image in the U and V direction.  If
C either is zero then the values are obtained from the current redtape.
C
C On completion a call to MAP_END_ALLOC is required to tidy access state.
C
C PA, 11/7/90
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer     is_map, minirt(10)

C test STATUS on entry
       if (status.ne.0) return

C find suitable output map and allocate
       call mapcat_next(imap,status)
       call mapcat_acc(imap,'SCRATCH',status)
       if (status.ne.0) then
         call mapcat_err(status,'map_alloc_out',
     *                   'Allocation of output maps failed')
         goto 999
       end if

C set size if not specified
       if (sizeu.le.0 .or. sizev.le.0) then
         call enminiRT(minirt,status)
         sizeu = minirt(5)
         sizev = minirt(6)
       end if

C allocate space as required
       if (chr_cmatch(access,'DIRECT')) then
C .. if access mode is DIRECT then find space
         call stack_access(imap,'SCRATCH','MAP',
     *                     sizeu*sizev,is_map,status)
         call stack_enqpnt(is_map,ip_map,status)
       else
         status = ill_mode
       end if

999    call mapcat_err(status,'map_alloc_scr','Space allocation failed')

       end
C
C
*+ map_alloc_in

       subroutine map_alloc_in(imap,access,map_array,ip_map,status)
C      ------------------------------------------------------------
C
C Allocate a map for input
C
C Input:
C    Map to allocate for input
       integer           imap
C    Access mode -- DIRECT or SEQUENTIAL
       character*(*)     access
C Updated:
C    Map data
       real*4            map_array(*)
C Returned:
C    Pointer to map_array and start of IMAP data
       integer           ip_map
C    Status
       integer           status
C
C The map IMAP is allocated for READ.  If the access requested is DIRECT
C then space is allocated in CORE and the map is read in (if not already)
C in core.  If the acces requested is SEQUENTIAL then no direct access to
C the map data is performed, but an allocation is made for supsequent
C calls to MAP_ROW_READ and MAP_ROW_WRITE.
C
*-
C Local Variables
C stack entry for map and redtape and file unit number
       integer    is_map, iunit, mode, ipm, ipr
C result of stack enquiry routines
       integer    data_in_core

       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

C check status on entry
       if (status.ne.0) return

C check catalogue entry
       call mapcat_chk(imap,'NONE',status)
       if (status.ne.0) goto 999

C if no access state previously defined then define it
       call mapcat_read(imap,status)
       if (current_map_status(ip_access).eq.access_clear) then
         call mapcat_chk(imap,'READ',status)
         call mapcat_acc(imap,'READ',status)
       end if

C allocate space for this map
       call redt_load(imap,status)
       if (chr_cmatch(access,'DIRECT').or.
     *     chr_cmatch(access,'SEQUENTIAL')) then
         call stack_access(imap,'READ','MAP',
     *                     current_minirt(5)*current_minirt(6),
     *                     is_map,status)
         call stack_enqdat(is_map,data_in_core,status)
         call stack_enqpnt(is_map,ip_map,status)
C .. read data into core if required
         if (data_in_core.eq.false) then
           call mapcat_mapopen(imap,'READ',iunit,status)
           call rdmap(iunit,map_array(ip_map),status)
           call mapcat_mapclose(imap,status)
         end if
         call stack_setdat(is_map,true,status)
         call stack_enqmode(imap,mode,ipm,ipr,iunit,status)
         call stack_dpredt(ipr,status)
       else
         status = ill_mode
       end if

C report any errors
999    call mapcat_err(status,'map_alloc_in','Fatal allocation error')

       end
C
C
*+ map_alloc_area

       subroutine map_alloc_area(imap,uv_range,map_array,ip_map,status)
C      ----------------------------------------------------------------
C
C Allocate a region of a map for input
C
C Input:
C    Map to allocate for input
       integer           imap
C    Range of map to read
       integer           uv_range(4)
C Updated:
C    Map data
       real*4            map_array(*)
C Returned:
C    Pointer to map_array and start of IMAP data
       integer           ip_map
C    Status
       integer           status
C
C The map IMAP is allocated for READ.  Only the specified region of the
C map is read into core.  If the region corresponds to the whole map
C then the action of the routine is idential to MAP_ALLOC_IN with
C access = 'DIRECT'.  The pointer to the map data is returned in
C IP_MAP -- this pointer is to the start of the COMPLETE map although
C only the specified region will contain sensible data.
C
C The following rules are used to determine the action of this
C routine:
C    1) only the V bounds of UV_RANGE are important as the structure
C       of the data files insists complete rows are read
C    2) if UV_RANGE exceeds the range of the map the whole map is read
C    3) if the region exceeds AREA_MAX_READ then the whole map is read
C if the whole map is read this routine is identical to MAP_ALLOC_IN
C with access='DIRECT'
*-
C Local Variables:
C stack entry for map and redtape and file unit number
       integer    is_map, iunit, ipm
C result of stack enquiry routines
       integer    data_in_core
C mini redtape
       integer    minirt(8)
C variable to test area to read
       real*4     test_area

       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

C check status on entry
       if (status.ne.0) return

C check catalogue entry
       call mapcat_chk(imap,'NONE',status)
       if (status.ne.0) goto 999

C if no access state previously defined then define it
       call mapcat_read(imap,status)
       if (current_map_status(ip_access).eq.access_clear) then
         call mapcat_chk(imap,'READ',status)
         call mapcat_acc(imap,'READ',status)
       end if

C allocate space for this map
       call redt_load(imap,status)
       call enminirt(minirt,status)
       test_area = abs( float(uv_range(4)-uv_range(3)) /
     *                  float(minirt(4)-minirt(3))     )
       if (area_max_read.le.0.000001) area_max_read = 0.5
       if (
     *       (uv_range(3).ge.minirt(3) .and.
     *        uv_range(4).le.minirt(4)     ) .or.
     *       (uv_range(3).gt.minirt(3)     ) .or.
     *       (uv_range(4).lt.minirt(4)     ) .or.
     *       (test_area.gt.area_max_read   )    ) then
         call stack_access(imap,'READ','MAP',
     *                     current_minirt(5)*current_minirt(6),
     *                     is_map,status)
         call stack_enqdat(is_map,data_in_core,status)
         call stack_enqpnt(is_map,ip_map,status)
C .. read data into core if required
         if (data_in_core.eq.false) then
           call mapcat_mapopen(imap,'READ',iunit,status)
           call rdmap(iunit,map_array(ip_map),status)
           call mapcat_mapclose(imap,status)
         end if
         call stack_setdat(is_map,true,status)
       else
         call stack_access(imap,'READ','MAP',
     *                     current_minirt(5)*current_minirt(6),
     *                     is_map,status)
         call stack_enqdat(is_map,data_in_core,status)
         call stack_enqpnt(is_map,ip_map,status)
C .. read data into core if required
         if (data_in_core.eq.false) then
           call mapcat_mapopen(imap,'READ',iunit,status)
           ipm = ip_map + (minirt(3)-uv_range(3))*minirt(5)
           minirt(3) = uv_range(3)
           minirt(4) = uv_range(4)
*           call rdarea(iunit,minirt,
*     *                 map_array(ipm),status)
           call mapcat_mapclose(imap,status)
         else
           call stack_setdat(is_map,true,status)
         end if
       end if

C report any errors
999    call mapcat_err(status,'map_alloc_area','Fatal allocation error')

       end
C
C
*+ map_getmap

       subroutine map_getmap(prompt,default,access,imap,status)
C      --------------------------------------------------------
C
C Prompt the user for a valid map identifier
C
C Given:
C   Prompt
       character*(*)      prompt
C   Default response
       character*(*)      default
C   Access required to map -- READ, WRITE
       character*(*)      access
C Returned:
C   Map entry
       integer            imap
C   Status word
       integer            status
C
C The user is prompted for a map. A default value may be specified and
C DEFAULT_MAP may be given as the default string. If the value
C given is not a valid map then the routine returns a non-zero value of
C STATUS.
C
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'
C

       if (status.ne.0) return
       call mapcat_getmap(prompt,default,access,imap,status)

C load redtape if access reuired is not NONE
       if (.not.chr_cmatch(access,'NONE')) then
         call redt_load(imap,status)
       end if

999    call mapcat_err(status,'map_getmap','Error reading map entry')

       end
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
       include '/mrao/anmap/include/mapcat_pars.inc'
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
*+ map_row_read

       subroutine map_row_read(imap,iv,map_array,ip_map,status)
C      --------------------------------------------------------
C
C Find pointer to map data during sequential access
C
C Given:
C   map catalogue entry
       integer    imap
C   row number
       integer    iv
C Updated:
C   map array data array
       real*4     map_array(*)
C Returned:
C   pointer to row in map_array
       integer    ip_map
C   status word
       integer    status
C
C Perform sequential access for map imap.  If the map is already in
C the stack then no access to the disc data file is required and a
C pointer to the start of the requested row is returned.  If the map
C is not in core then the data is read from the disc file.  A previous
C call to MAP_ALLOC_IN is required to setup the access state to the
C data file.
C
C-
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/anmap/include/mapcat_pars.inc'

C local variables defining map access mode
       integer          ipm, ipr, mode, iunit
C mini redtape
       integer          minirt(8)

C check status on entry
       if (status.ne.0) return

C find access mode to this file
       call stack_enqmode(imap,mode,ipm,ipr,iunit,status)
       call stack_ldredt(ipr,status)
       if (status.ne.0) goto 999

C decode mode and take appropriate action
       if (mode.eq.mode_direct) then
*         print *,'..DIRECT iv = ',iv
         call enminirt(minirt,status)
         ip_map = ipm+(minirt(3)-iv)*minirt(5)
*         print *,'.. ipm/ip_map = ',ipm,ip_map

       else if (mode.eq.mode_sequential) then
         print *,'.. ERROR iv = ',iv
         ip_map=ipm
*         call rdrow(iunit,iv,map_array(ip_map),status)

       end if

999    call mapcat_err(status,'map_row_read','Data not read')

       end
C
C
*+ map_row_write

       subroutine map_row_write(imap,iv,map_array,ip_map,status)
C      ---------------------------------------------------------
C
C Save data during sequential access
C
C Given:
C   map catalogue entry
       integer    imap
C   row number
       integer    iv
C   map array data array
       real*4     map_array(*)
C   pointer to row in map_array
       integer    ip_map
C Returned:
C   status word
       integer    status
C
C Perform sequential access for map imap.  The row at map_array(ip_map)
C is written to the output file.  A call to MAP_ALLOC_OUT is required
C before this routine can be used.  The max and min are maintained
C for this map.
C-

C local variables defining map access mode
       integer          ipm, ipr, mode, iunit
C mini redtape
       integer          minirt(8)
C loop counter
       integer          i
C maxmin values
       integer          izmnx(4)
       real*4           zmnx(4)

       include '/mrao/anmap/include/mapcat_pars.inc'

C check status on entry
       if (status.ne.0) return

C find access mode to this file
       call stack_enqmode(imap,mode,ipm,ipr,iunit,status)
       if (status.ne.0) goto 999

C decode mode and take appropriate action
       call stack_ldredt(ipr,status)
       call enminirt(minirt,status)
       zmnx(1) = -1.0E+30
       zmnx(2) =  1.0E+30
       do i=0,minirt(5)-1
         if (map_array(ip_map+i).gt.zmnx(1)) then
           zmnx(1) = map_array(ip_map+i)
           izmnx(1)= i+minirt(1)
           izmnx(2)= iv
         end if
         if (map_array(ip_map+i).lt.zmnx(2)) then
           zmnx(2) = map_array(ip_map+i)
           izmnx(3)= i+minirt(1)
           izmnx(4)= iv
         end if
       end do
       call stack_setmnx(imap,zmnx,izmnx,status)
       if (mode.eq.mode_sequential) then
          call wrrow(iunit,iv,map_array(ip_map),status)
       else
          ip_map = ip_map + minirt(5)
       end if

999    call mapcat_err(status,'map_row_write',' ')

       end
*$ 3) Map/Image catalogue routines
*  -------------------------------
C
C N.B. The routine MAPCAT_io_nxtfil is machine dependent
C
*+ mapcat_addtocat

       subroutine mapcat_addtocat(default_description,
     *                            prompt_state,count,status)
C      -----------------------------------------------------
C
C To prompt for and read (a) map(s) into the catalogue
C
C Given:
C    character string desribing default search route for files
       character*(*)   default_description
C    prompt state (turn prompting on/off)
       logical         prompt_state
C Returned:
C    number of maps added to the catalogue
       integer         count
C    status word
       integer         status
C
C The routine will prompt for a valid file name and open the file checking
C to see whether it is a valid map file. The map is read (along with
C the redtape) using the standard RA map reading routines in
C (library)MAPLIB. The buffer entries are updated and the current map set
C to the one just read in.
C
*-
       include '/mrao/include/system_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C local variables used to access file and redtape
       integer                  imap, ifile
       integer                  minirt(10), redtape_buffer(1024)
       integer                  x_pages, x_style
C character variables
       character*(iolen_file)   file_description, filename
       character*(iolen_dir)    default_dir
       character*80             string
       character                source*40, program*20
       integer                  len_fn, len_fd, len_d
       integer                  len_st, idate(3)
C logical variables
       logical                  check_addition
C counters
       integer                  i
C debug function
       logical                  cmd_dblev

C check status on entry
       if (status.ne.0) return

C set number of maps added to zero
       count = 0

C Prompt for filename description
       call mapcat_enqdefdir(default_dir,status)
       len_d = chr_lenb(default_dir)
       len_fd = chr_lenb(default_description)
       if (len_fd.eq.0) then
         file_description=
     *        default_dir(1:len_d)//dir_char_right//'*'
         call io_getwrd('Map file description : ','*',
     *               file_description,len_fd,status)
       else
         file_description = default_description
         len_fd = chr_lenb(file_description)
       end if

C loop for all files matching description
       count = 0
       if (cmd_dblev(6)) then
         print *,'.. file_description = ',file_description(1:len_fd)
       end if
       if (chr_lend(file_description,dir_char_right) .eq.
     *     chr_lenb(file_description) ) then
         string = dir_char_left//default_dir(1:len_d)//dir_char_right//
     *            file_description(1:len_fd)
         file_description = string
         len_fd = chr_lenb(file_description)
         if (cmd_dblev(6)) then
           print *,'.. now = ',file_description(1:len_fd)
           print *,'.. chr_lend= ',chr_lend(file_description)
           print *,'.. chr_lenb= ',chr_lenb(file_description)
         end if
       end if
       do while (status.eq.0)
         call mapcat_io_nxtfil(file_description(1:len_fd),
     *                   filename,source,count,status)
         if (status.eq.no_file) then
           if (count.eq.0) then
             call io_wrout('.. no files found matching description')
           end if
           status = 0
           goto 10
         else if (status.ne.0) then
           goto 10
         end if
         len_fn = chr_lenb(filename)
         string = ' '
         write(string,5)filename(1:len_fn)
 5       format('Include ',A,' in Map-Catalogue ? ')
         len_st = chr_lenb(string) + 1
         if (prompt_state) then
           check_addition = io_yesno(string(1:len_st),'yes',status)
         else
           check_addition = .true.
         end if
         imap = 0

         if (check_addition) then
C .. check to see if the file is already in the catalogue
           if (operating_system.ne.'UNIX') then
             call chr_chucas(filename(1:len_fn))
           end if
           call mapcat_exist(filename(1:len_fn),imap,status)
         end if

         if (check_addition .and. imap.eq.0) then
C .. Open the file as a map
           call mapcat_next(imap,status)
           call mapcat_acc(imap,'WRITE',status)
           call mapcat_setrt(imap,filename(1:len_fn),minirt,status)
           do i=1,chr_lenb(source)
             if (source(i:i).eq.file_char_sep) then
               source(i:i) = '-'
             end if
           end do
           call mapcat_setsr(imap,source,program,status)
           call mapcat_mapopen(imap,'READ',ifile,status)
           if (status.ne.0) then
              goto 999
           end if

C .. Read redtape
           call chredt(status)
           if (status.eq.0) then
             call dpredt(redtape_buffer,status)
             call enxrdt(x_pages,x_style,status)
             if (x_pages.ge.1) then
               call dpxrdt(redtape_buffer(513),1,status)
             end if
           else
             status = 0
           end if
           call rdredt(ifile,0,status)
           call enminirt(minirt,status)
           call enhist(idate,program,status)
           call mapcat_setrt(imap,filename,minirt,status)
           call mapcat_setsr(imap,source,program,status)
           call mapcat_mapclose(imap,status)
           call ldredt(redtape_buffer,status)
           call chredt(status)
           if (status.eq.0) then
             call enxrdt(x_pages,x_style,status)
             if (x_pages.ge.1) then
               call ldxrdt(redtape_buffer(513),1,status)
             end if
           else
             status = 0
           end if
           call mapcat_end(imap,status)
           if (status.eq.0) then
             string = ' '
             len_st = chr_lenb(source)
             write (string,7)source(1:len_st),imap
  7          format('Map : ',A,' in Catalogue Entry : ',I4)
             len_st = chr_lenb(string)
             call io_wrout(string(1:len_st))
             call map_setdef(imap,status)
             count = count + 1
           end if
         else if (check_addition .and. imap.ne.0) then
           string = ' '
           write (string,8)filename(1:len_fn),imap
 8         format('File : ',A,' already allocated Entry : ',I4)
           len_st = chr_lenb(string)
           call io_wrout(string(1:len_st))
           call map_setdef(imap,status)
         end if
       end do
10     continue
999    call cmd_err(status,'Add-to-Catalogue','Read operation failed')

       end
C
C
*+ mapcat_getmap

       subroutine mapcat_getmap(prompt,default,access,imap,status)
C      -----------------------------------------------------------
C
C Prompt the user for a valid map identifier
C
C Given:
C   Prompt
       character*(*)      prompt
C   Default response
       character*(*)      default
C   Access required to map
       character*(*)      access
C Returned:
C   Map entry
       integer            imap
C   Status word
       integer            status
C
C The user is prompted for a map. A default value may be specified and
C DEFAULT_MAP may be given as the default string. If the value
C given is not a valid map then the routine returns a non-zero value of
C STATUS.
C
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       integer       lg, len_pr, len_pr1, len_st, len_def, len_text

       character     text*80, string*80, def*30, defg*30, map_text*80

       logical       special

C check status flag on entry
       special=.false.
       if (status.ne.0) return

C check default string for DEFAULT_MAP
       defg = default
       call chr_chucas(defg)
       lg = chr_lenb(defg)
       if (defg(1:lg).eq.'DEFAULT_MAP'.or.defg(1:lg).eq.'DEFAULT-MAP')
     *     THEN
         imap=default_map
         if (imap.lt.10) then
           write(string,'(''Default-map='',i1)')imap
         else if (imap.lt.100) then
           write(string,'(''Default-map='',i2)')imap
         else
           write(string,'(''Default-map='',i3)')imap
         end if
         special=.true.

       else if (defg(1:lg).eq.'NEXT_MAP'.or.defg(1:lg).eq.'NEXT-MAP')
     *     then
         call mapcat_next(imap,status)
         string = ' '
         if (imap.lt.10) then
           write(string,'(i1)') imap
         else if (imap.lt.100) then
           write(string,'(i2)') imap
         else
           write(string,'(i3)') imap
         end if
         special=.true.

       end if

C if special action is required then construct new prompt
       text=' '
       len_pr=len(prompt)

       if (special) then
         len_pr=len(prompt)
         len_pr1=chr_lenb(prompt)
         len_st=chr_lenb(string)
         text = ' '
         if (chr_chalfn(prompt(len_pr1:len_pr1))) then
           write(text,'(a,'' ['',a,''] '')')prompt,string(1:len_st)

         else
           write(text,'(a,'' ['',a,''] '',a)')prompt(1:len_pr1-1),
     *           string(1:len_st),prompt(len_pr1:len_pr)

         end if

         len_pr=len_pr+len_st+4
         def = ' '
         if (imap.lt.10) then
           write(def,'(i1)')imap
         else if (imap.lt.100) then
           write(def,'(i2)')imap
         else
           write(def,'(i3)')imap
         end if

       else
         text=prompt
         def=default

       end if

C Prompt
       len_def=chr_lenb(def)
       call io_getwrd(text(1:len_pr),def(1:len_def),
     *             map_text,len_text,status)
C find map entry
       call mapcat_fndmap(map_text(1:len_text),imap,status)
C check and return on error
       call mapcat_chk(imap,access,status)

       call cmd_err(status,'GET-MAP',
     *              'Error in obtaining map description')

       end
C
C
*+ mapcat_enqdefdir

       subroutine mapcat_enqdefdir(default_directory,status)
C      -----------------------------------------------------
C
C Enquire default map directory
C
C Returned:
C    default map directory
       character*(*)    default_directory
C    status word
       integer          status
C
C The default map directory is returned.  If there has been no previous
C call to mapcat_setdefdir then the default directory for the logged on user
C is returned.
C
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer    len_dir

       if (status.ne.0) return
       len_dir = chr_lenb(current_def_dir)
       if (len_dir.gt.0) then
         default_directory = current_def_dir(1:len_dir)
       else
         call enmdir(' ',default_directory,status)
       end if
       call mapcat_err(status,'mapcat_enqdefdir',' ')

       end
C
C
*+ mapcat_enqrm

       subroutine mapcat_enqrm(report_mode)
C      ------------------------------------
C
C Enquire the report mode status
C
C Returned:
C     Report mode flag
       logical report_mode
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       report_mode = current_report
       end
C
C
*+ mapcat_enqch

       subroutine mapcat_enqch(check_mode)
C      -----------------------------------
C
C Enquire the check mode status
C
C Returned:
C     Check mode flag
       logical check_mode
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       check_mode = current_check
       end
C
C
*+ mapcat_enqrec

       subroutine mapcat_enqrec(imap,record,status)
C      --------------------------------------------
C
C Enquire the current record definition
C
C Input:
C    Map catalogue entry
       integer             imap
C Returned:
C    The current catalogue record definition
       integer             record(*)
C    Status
       integer             status
*-
       integer   i
       include '/mrao/anmap/include/mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_read(imap,status)
       do i=1,cat_blocksize/4
          record(i) = current_record(i)
       end do
       call mapcat_err(status,'mapcat_enqrec',' ')
       end
C
C
*+ mapcat_enqrt

       subroutine mapcat_enqrt(imap,filename,minirt,status)
C      ----------------------------------------------------
C
C Enquire the map filename and mini redtape for catalogue entry IMAP
C
C Input:
C    Map catalogue entry
       integer             imap
C Returned:
C    Filename for entry IMAP
       character*(*)       filename
C    Mini redtape for entry IMAP
       integer             minirt(*)
C    Status
       integer             status
*-
       integer   i
       include '/mrao/anmap/include/mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       call mapcat_read(imap,status)
       do i = 1,8
         minirt(i) = current_minirt(i)
       end do
       filename = current_filename
       call mapcat_err(status,'mapcat_enqrt',' ')
       end
C
C
*+ mapcat_enqsr

       subroutine mapcat_enqsr(imap,source,program,status)
C      ---------------------------------------------------
C
C Enquire the source name for catalogue entry IMAP
C
C Input:
C    Map catalogue entry
       integer             imap
C Returned:
C    Source name for entry IMAP
       character*(*)       source
C    Program name
       character*(*)       program
C    Status
       integer             status
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       call mapcat_read(imap,status)
       source  = current_source
       program = current_program
       call mapcat_err(status,'mapcat_enqsr',' ')
       end
C
C
*+ mapcat_enqst

       subroutine mapcat_enqst(imap,map_status,status)
C      -----------------------------------------------
C
C Enquire the map status word for catalogue entry IMAP
C
C Input:
C    Map catalogue entry
       integer             imap
C Returned:
C    Status word
       integer             map_status(*)
C    Status
       integer             status
C
C Return the map status word for the specified catalogue entry
*-
       integer   i
       include '/mrao/anmap/include/mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       call mapcat_read(imap,status)
       do i = 1,length_map_status
         map_status(i) = current_map_status(i)
       end do
       call mapcat_err(status,'mapcat_enqst',' ')
       end
C
C
*+ mapcat_enqsz

       subroutine mapcat_enqsz(number_cat_entries,status)
C      --------------------------------------------------
C
C Enquire the maximum number of catalogue entries possible
C
C Returned:
C    Number of catalogue entries
       integer      number_cat_entries
C    Status
       integer      status
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       if (status.ne.0) return
       number_cat_entries = max_cat_entries
       end
C
C
*+ mapcat_setdefdir

       subroutine mapcat_setdefdir(default_directory,status)
C      -----------------------------------------------------
C
C Set default map directory
C
C Input:
C    default map directory
       character*(*)    default_directory
C Returned:
C    status word
       integer          status
C
C The default map directory is set.  A check is made that the
C directory exists.
C
C Warning this command will affect the destination of PAGED temporary
C maps.  It is essential that this command is used with great care.
C
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'
       integer    len_dir

       if (status.ne.0) return
       len_dir = chr_lenb(default_directory)
       call io_namusr(default_directory(1:len_dir),
     *             current_def_dir,0,status)
       if (status.ne.0) then
         current_def_dir = ' '
       end if
       call mapcat_err(status,'mapcat_setdefdir',' ')

       end
C
C
*+ mapcat_setrm

       subroutine mapcat_setrm(report_mode)
C      ------------------------------------
C
C Set the report mode status
C
C Input
C     Report mode flag
       logical report_mode
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       current_report = report_mode
       end
C
C
*+ mapcat_setch

       subroutine mapcat_setch(check_mode)
C      -----------------------------------
C
C Set the check mode status
C
C Input:
C     Check mode flag
       logical check_mode
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       current_check = check_mode
       end
C
C
*+ mapcat_setrt

       subroutine mapcat_setrt(imap,filename,minirt,status)
C      ----------------------------------------------------
C
C Set the filename and mini redtape for catalogue entry IMAP
C
C Input:
C    Map catalogue entry
       integer             imap
C    Filename for entry IMAP
       character*(*)       filename
C    Mini redtape for entry IMAP
       integer             minirt(*)
C Returned:
C    Status
       integer             status
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/include/iolib_constants.inc'

       integer   i

       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       do i = 1,8
         current_minirt(i) = minirt(i)
       end do
       current_filename = filename
       if (operating_system.ne.'UNIX') then
         call chr_chucas(current_filename)
       end if
       call mapcat_write(imap,status)
       call mapcat_err(status,'mapcat_setrt',' ')
       end
C
C
*+ mapcat_setsr

       subroutine mapcat_setsr(imap,source,program,status)
C      ---------------------------------------------------
C
C Set the source name for catalogue entry imap
C
C Input:
C    Map catalogue entry
       integer             imap
C    Source for entry IMAP
       character*(*)       source
C    Program for entry IMAP
       character*(*)       program
C Returned:
C    Status
       integer             status
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       current_source  = source
       call chr_chucas(current_source)
       current_program = program
       call chr_chucas(current_program)
       call mapcat_write(imap,status)
       call mapcat_err(status,'mapcat_setsr',' ')
       end
C
C
*+ mapcat_setst

       subroutine mapcat_setst(imap,map_status,status)
C      -----------------------------------------------
C
C Set the map status word for catalogue entry IMAP
C
C Input:
C    Map catalogue entry
       integer             imap
C    Status word
       integer             map_status(*)
C Returned:
C    Status
       integer             status
C
C Sets the map status word for the specified catalogue entry
*-
       integer   i
       include '/mrao/anmap/include/mapcat_cat.inc'
       if (status.ne.0) return
       call mapcat_chk(imap,'NONE',status)
       call mapcat_open(status)
       do i = 1,length_map_status
         current_map_status(i) = map_status(i)
       end do
       call mapcat_write(imap,status)
       call mapcat_err(status,'mapcat_setst',' ')
       end
C
C
*+ mapcat_mapopen

       subroutine mapcat_mapopen(imap,access,iunit,status)
C      ---------------------------------------------------
C
C Open the physical file for a catalogue entry
C
C Input:
C    Map Catalogue entry
       integer             imap
C    Access
       character*(*)       access
C Returned:
C    Unit number
       integer             iunit
C    Status
       integer             status
C
C Open the physical file associated with map IMAP. The unit number on
C which the map is opened is returned in IUNIT.
C
*-
C include definitions to the status word entries
       include '/mrao/anmap/include/mapcat_pars.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/iolib_constants.inc'

C status word
       integer        map_status(10)
C redtape
       integer        minirt(10)
C local status
       integer        istat
C filename
       character      filename*(iolen_file)


C check status on entry
       if (status.ne.0) return

C check IMAP for validity
       call mapcat_chk(imap,'NONE',status)
       if (status.ne.0) goto 999

C read status word
       call mapcat_enqst(imap,map_status,status)

C check current status
       if (map_status(ip_open).eq.true) then
         iunit = map_status(ip_unit)
         return
       end if

C open map
       call mapcat_enqrt(imap,filename,minirt,status)
       call opemap(map_status(ip_unit),filename,access,0,status)
C update status word
       if (status.eq.0) then
         iunit = map_status(ip_unit)
         map_status(ip_open) = true
       else
         map_status(ip_open) = false
         map_status(ip_unit) = 0
       end if
       istat = 0
       call mapcat_setst(imap,map_status,istat)
       call mapcat_err(istat,'mapcat_mapopen',
     *                 'Error accessing Catalogue')

C error report
999    call mapcat_err(status,'mapcat_mapopen',
     *                 'Failed to open map file')

       end
C
C
*+ mapcat_mapclose

       subroutine mapcat_mapclose(imap,status)
C      ---------------------------------------
C
C Close the physical file associated with map IMAP
C
C Input:
C    Map catalogue entry
       integer            imap
C Returned
C    Status
       integer            status
*-
C status word
       integer   map_status(10)
C
       include '/mrao/anmap/include/mapcat_pars.inc'

C check status on entry
       if (status.ne.0) return

C check IMAP for validity
       call mapcat_chk(imap,'NONE',status)
       if (status.ne.0) goto 999

C enquire map status word
       call mapcat_enqst(imap,map_status,status)

C close physical file if open and update status word
       if (map_status(ip_open).eq.true) then
         close (map_status(ip_unit))
       end if
       if  (status.eq.0) then
         map_status(ip_open) = false
         map_status(ip_unit) = 0
       end if
       call mapcat_setst(imap,map_status,status)

C report error
999    call mapcat_err(status,'mapcat_mapclose',' ')

       end
C
C
*+ mapcat_fndmap

       subroutine mapcat_fndmap(map,imap,status)
C      -----------------------------------------
C
C Interprete the text MAP as a catalogue entry or disc file
C
C Input:
C    Map catalalogue identifier
       character*(*)       map
C Returned:
C    Map catalogue entry
       integer             imap
C    Status
       integer             status
C
C The text string MAP is scanned and interpreted either as a catalogue
C entry, as a source name or as a disc file.  If the string is a map
C name then it is added to the catalogue unless already present.  IMAP
C is returned in all cases as the catalogue entry of the specified map.
*-

       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/iolib_functions.inc'

       integer    len_map, istat, count
       integer    i
       character  source*30, search*80, string*80
       integer    len_source, len_program, len_search
       logical    selected

C check status on entry
       if (status.ne.0) return
       len_map = chr_lenb(map)
       if (operating_system.ne.'UNIX') then
         call chr_chucas(map)
       end if

C check for stack entry
       istat = 0
       call chr_chctoi(map(1:len_map),imap,istat)
       if (istat.eq.0) then
         if (imap.gt.0 .and. imap.le.max_cat_entries) then
           return
         end if
       end if

C look for characters only associtaed with a file specification
       if (chr_chkchr(file_char_type,map(1:len_map))) goto 20
       if (chr_chkchr(file_char_version,map(1:len_map))) goto 20
       if (chr_chkchr(dir_char_right,map(1:len_map))) goto 20

C look for match to source names
       call mapcat_open(status)
       count = 0
       i = 0
       selected = .false.
       do while (i.lt.max_cat_entries .and. .not.selected)
         i = i + 1
         call mapcat_read(i,status)
         len_source  = chr_lenb(current_source)
         len_program = chr_lenb(current_program)
         search = current_source(1:len_source)//'-'//current_program
         call chr_chucas(search)
         len_search = chr_lenb(search)
         if (current_map_status(ip_data).eq.false) goto 10
         if (chr_cmatch(map(1:len_map),search(1:len_search))) then
           count = count + 1
           imap = i
           if (count.eq.1) then
             source = current_source
           end if
           if (count.eq.2) then
             string = 'Select '//source(1:chr_lenb(source))//' ? '
             selected = io_yesno(string(1:chr_lenb(string)),
     *                  'yes',status)
             if (.not.selected) then
               string =
     *         'Select '//current_source(1:chr_lenb(current_source))//
     *         ' ? '
               selected = io_yesno(string(1:chr_lenb(string)),
     *         'yes',status)
             end if
             if (selected) then
               source = current_source
               count = 1
             end if
           else if (count.gt.2) then
             string =
     *       'Select '//current_source(1:chr_lenb(current_source))//
     *       ' ? '
             selected = io_yesno(string(1:chr_lenb(string)),
     *       'yes',status)
             if (selected) then
               source = current_source
               count = 1
             end if
           end if
         end if
 10      continue
       end do
       if (count.eq.1) then
         return
       end if
       if (count.gt.1) then
         status = ill_mapamb
         return
       end if

C look for match to file names
20     continue
       call mapcat_addtocat(map(1:len_map),.true.,count,status)
       if (count.gt.0) then
         call map_enqdef(imap,status)
       else if (count.eq.0) then
         status = ill_mapno
       end if

       end
C
C
*+mapcat_io_nxtfil

       subroutine mapcat_io_nxtfil (name, file, source, count, status)
C      ------------------------------------------------------------
C
C  Finds the next map file matching a given filename.
C
C  Given:
C      NAME      char*(*)    input filename pattern
C      COUNT     integer     current count of matching files
C
C  Returned:
C      FILE      char*(*)    next matching filename
C      SOURCE    char*(*)    source name
C      COUNT     integer     current count of matching files
C      STATUS    integer     status value
C
C  Returns the next map file from a list of disc files matching NAME.
C  The found filename is returned in FILE, and the current count of
C  matching files in COUNT.  The STATUS value should be zero on entry,
C  and is normally unchanged.  If the list is exhausted, the returned
C  status will be non-zero (NO_FILE), and COUNT will contain the number
C  of files in the list.  The list will be re-initialised if the routine
C  is entered with a new value for the input file name, or with COUNT equal
C  to zero.  The file type must match one of the standard file types defining
C  a valid map-type file.
C
C  (DJT, 18 May 87)
C  (PA, 3/3/92)
*-
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer       maxt
       parameter    (maxt = 20)
       character*(*) name, file, source
       character     lsource*(iolen_file),
     *               dir*(iolen_dir)
       character*4   type_list(maxt), type
       integer       iobjx, count, status, it
       logical       test
       data  type_list / 'MAP ','CLN ',
     *                   'IMAP','ICLN',
     *                   'QMAP','QCLN',
     *                   'UMAP','UCLN',
     *                   'BEAM','BSET',
     *                   'MI  ','PERC',
     *                   'CHI ','RESI',
     *                   'CCMP','APER',
     *                   'COS ','SIN ',
     *                   'AMP ','PHI '
     *                 /

       if (status.ne.0) return

1      call io_nxtfil( name, file, iobjx, count, status )
       call io_brkfil( file, dir, lsource, type )
       source =
     *   lsource(1:chr_lenb(lsource))//'_'//type(1:chr_lenb(type))
       if (status.eq.0) then
         test = .false.
         call chr_chucas(type)
         do it=1,maxt
           test = test .or. type.eq.type_list(it)
         end do
         if (.not.test) goto 1
       endif

       end
C
C
C
*+ mapcat_chk

       subroutine mapcat_chk(imap,access,status)
C      -----------------------------------------
C
C Check a catalogue entry for the specified status
C
C Input:
C    Map catalogue entry
       integer                  imap
C    Access required
       character*(*)            access
C Returned:
C    Status
       integer                  status
C
C The specified catalogue entry is checked for the requested access. An
C error is generated if the catalogue entry is invalid or not available
C for the access requested.  This routine generates an error message in
C addition to setting status on exit.
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (status.ne.0) return

       if (imap.le.0 .or. imap.gt.max_cat_entries) then
         status = ill_catent
         return
       end if
       call mapcat_read(imap,status)
       if ( (chr_cmatch(access,'READ') .or.
     *       chr_cmatch(access,'WRITE')) .and.
     *       status.eq.0                   ) then
         if (chr_cmatch(access,'WRITE') .and. status.eq.0) then
           if (current_map_status(ip_access).eq.access_read) then
             status = ill_catuse
           end if
           if (current_map_status(ip_data).eq.true) then
             status = ill_catwrt
           end if
         else if (chr_cmatch(access,'READ') .and. status.eq.0) then
           if (current_map_status(ip_access).eq.access_write) then
             status = ill_catuse
           end if
           if (current_map_status(ip_data).eq.false) then
             status = ill_catrd
           end if
         end if
       end if

       call mapcat_err(status,'MAP-CATALOGUE','Unable to access MAP')

       end
C
C
*+ mapcat_acc

       subroutine mapcat_acc(imap,access,status)
C      -----------------------------------------
C
C Set the access state to a map catalogue entry
C
C Input:
C     Map catalogue entry
        integer            imap
C     Access
        character*(*)      access
C Returned:
C     Status
        integer            status
C
C Set the access state to the specified catalogue entry.  This ensures
C The catalogue entry is not opened more than once for read/write
C simultaneously.
*-

       integer   map_status(10)

       include '/mrao/anmap/include/mapcat_pars.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (status.ne.0) return
       call mapcat_enqst(imap,map_status,status)
       if (status.eq.0) then
         if (map_status(ip_access).eq.access_clear) then
            if (chr_cmatch(access,'READ')) then
              map_status(ip_access) = access_read
            else if (chr_cmatch(access,'WRITE')) then
              map_status(ip_access) = access_write
            else if (chr_cmatch(access,'SCRATCH')) then
              map_status(ip_access) = access_scratch
            else if (chr_cmatch(access,'CREATE')) then
              map_status(ip_access) = access_create
            else if (chr_cmatch(access,'CLEAR')) then
              map_status(ip_access) = access_clear
            else
              status = ill_catacc
            end if
         else if (chr_cmatch(access,'CLEAR')) then
            map_status(ip_access) = access_clear
         else
            status = ill_catuse
         end if
         call mapcat_setst(imap,map_status,status)
       end if
       call mapcat_err(status,'mapcat_acc','Access Failed')
       end
C
C
*+ mapcat_end

       subroutine mapcat_end(imap,status)
C      ----------------------------------
C
C End the access state to a map catalogue entry
C
C Input:
C     Map catalogue entry
        integer            imap
C Returned:
C     Status
        integer            status
C
C End the access state to the specified catalogue entry. Physical files
C are closed if they are still open.
*-

       integer   map_status(10)

       include '/mrao/anmap/include/mapcat_pars.inc'

       if (status.ne.0) return
       call mapcat_enqst(imap,map_status,status)
       if (status.eq.0) then
         if (map_status(ip_open).eq.true) then
           call mapcat_mapclose(imap,status)
         end if
         map_status(ip_access) = access_clear
         map_status(iP_data)   = true
         call mapcat_setst(imap,map_status,status)
       end if
       call mapcat_err(status,'mapcat_end',' ')
       end
C
C
*+ mapcat_open

       subroutine mapcat_open(status)
C      -----------------------------
C
C Open the map catalogue file if required
C
C Returned:
C     Status
       integer      status
C
*-
       include '/mrao/include/iolib_constants.inc'
C
C Local variables
       character loc_user*(iolen_user), loc_file*(iolen_file)
       integer   len_lf, loc_mode, loc_termno
       logical   exists, dir_exists
       integer   imap, pages_on_file, idir, n, nn
       integer   len_cf

       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (status.ne.0) return
       len_cf = chr_lenb(cat_file)
       if (len_cf.eq.0) then
         cat_file = default_cat_file
         current_cat_file = 0
         len_cf = chr_lenb(cat_file)
       end if
       call io_enqexe(loc_user,loc_mode,loc_termno)
       loc_user = '~/mrao'
       call io_makfil(loc_user,cat_file(1:len_cf),'mctg',
     *             loc_file,len_lf)
       inquire (file=loc_file(1:len_lf), exist=exists)
       if (.not.exists) then
         pages_on_file = max_cat_entries*cat_blocksize/(512.0*4.0)
         call io_crefil(loc_file(1:len_lf),pages_on_file,
     *                  .false.,1,status)
         do n=1,num_buffers
           current_map = 0
           do nn=1,cat_blocksize/4
              buffer_records(nn,n) = current_record(nn)
           end do
         end do
         call stack_init(-1,0,-1,0,status)
       end if
       if (cat_unit.eq.0) then
         call io_operan(cat_unit,loc_file(1:len_lf),
     *               'WC',cat_blocksize,0,status)
         call io_makfil(loc_user,cat_dir_file,'mctg',
     *               loc_file,len_lf)
         inquire (file=loc_file(1:len_lf), exist=dir_exists)
         do n=1,num_buffers
           current_map = 0
           do nn=1,cat_blocksize/4
              buffer_records(nn,n) = current_record(nn)
           end do
         end do
         call stack_init(-1,0,-1,0,status)
         if (dir_exists) then
           call io_opefil(idir,loc_file(1:len_lf),'READ',0,status)
           do n=1,10
             read (idir,*) defined_cat_file(n)
             read (idir,'(A)') list_cat_file(n)
           end do
           close (idir)
         end if
       end if
       if (.not.exists) then
         do imap = 1, max_cat_entries
           current_filename = ' '
           current_source   = ' '
           current_program  = ' '
           current_map      = imap
           current_map_status(ip_data)   = false
           current_map_status(ip_open)   = false
           current_map_status(ip_unit)   = 0
           current_map_status(ip_access) = access_clear
           call io_wrfile(cat_unit,imap,current_record,
     *                 cat_blocksize/4,status)
         end do
         if (status.ne.0) goto 999
         call io_wrout('.. Map-Catalogue created and initialised')
         call io_wrout('.. Use Add-To-Catalogue to define maps/images')
       end if

999    call mapcat_err(status,'mapcat_open','Fatal Stack-System Error')

       end
C
C
*+ mapcat_close

       subroutine mapcat_close(status)
C      -----------------------------
C
C Close the map catalogue file
C
C Returned:
C     Status
       integer      status, idir, n
C
*-
       include '/mrao/include/iolib_constants.inc'
C
       character loc_user*(iolen_user), loc_file*(iolen_file)
       integer   len_lf, loc_mode, loc_termno

       include '/mrao/anmap/include/mapcat_cat.inc'

       if (status.ne.0) return
       if (cat_unit.gt.0) then
         close (cat_unit)
         cat_unit = 0
         call io_enqexe(loc_user,loc_mode,loc_termno)
         loc_user = '~/mrao'
         call io_makfil(loc_user,cat_dir_file,'mctg',loc_file,len_lf)
         call io_opefil(idir,loc_file(1:len_lf),'WRITE',0,status)
         do n=1,10
           write (idir,*) defined_cat_file(n)
           write (idir,'(A)') list_cat_file(n)
         end do
         close (idir)
       end if

       call mapcat_err(status,'mapcat_close','Fatal Stack-System Error')

       end
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

       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       integer   n, nn

       if (status.ne.0) return
       if (imap.le.0 .or. imap.gt.max_cat_entries) then
         status = ill_catent
       else
         if (cat_unit.eq.0) then
           status = ill_catope
         else
           if (imap.ne.current_map) then
             do n=1,num_buffers
               do nn=1,cat_blocksize/4
                 current_record(nn) = buffer_records(nn,n)
               end do
               if (imap.eq.current_map) goto 10
             end do
             call io_rdfile(cat_unit,imap,current_record,
     *                   cat_blocksize/4,status)
             num_buffers = num_buffers+1
             if (num_buffers.gt.3) num_buffers = 1
             do nn=1,cat_blocksize/4
               buffer_records(nn,num_buffers) = current_record(nn)
             end do
           end if
         end if
       end if
10     continue
       call mapcat_err(status,'mapcat_read',' ')
       end
C
C
*+ mapcat_write

       subroutine mapcat_write(imap,status)
C      ------------------------------------
C
C Write a record to the catalogue file
C
C Input:
C    Catalogue entry
       integer       imap
C
C Returned:
C    Status
       integer       status
*-

       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'

       if (status.ne.0) return
       if (imap.le.0 .or. imap.gt.max_cat_entries) then
         status = ill_catent
       else
         if (cat_unit.eq.0) then
           status = ill_catope
         else
           call io_wrfile(cat_unit,imap,current_record,
     *                 cat_blocksize/4,status)
         end if
         num_buffers = 0
       end if
       call mapcat_err(status,'mapcat_write',' ')
       end
C
C
*+ mapcat_next

       subroutine mapcat_next(next_map,status)
C      ---------------------------------------
C
C Return the next free map entry
C
C Returned:
C   Next free map entry
       integer          next_map
C   Status word
       integer          status
C
C The next available catalogue entry is found. If the catalogue is
C full then the status value is set to ILL_NXTMAP.
*-
       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'

       integer      imap

       if (status.ne.0) return
       next_map = -1
       do imap = 1,max_cat_entries
         call mapcat_read(imap,status)
         if (status.eq.0) then
           if (current_map_status(ip_data).eq.false) then
             if (current_map_status(ip_access).eq.access_clear) then
                next_map = imap
                goto 100
             end if
           end if
         end if
       end do
100    continue
       if (next_map.eq.-1) then
         status = ill_nxtmap
       end if
       call mapcat_err(status,'mapcat_next','Unable to find entry')
       end
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

       include '/mrao/anmap/include/mapcat_cat.inc'
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
C
C
*+ mapcat_delete

       subroutine mapcat_delete(imap,prompt_state,status)
C      --------------------------------------------------
C
C Delete a map referenced in the catalogue (including file)
C
C Input:
C    Map entry
       integer       imap
C    Flag controlling prompting
       logical       prompt_state
C Returned:
C    Status
       integer       status
*-
       include '/mrao/anmap/include/mapcat_pars.inc'
       include '/mrao/include/iolib_constants.inc'

       character*(iolen_file)    filename
       integer                   minirt(10), iprompt, map_status(10)

       if (status.ne.0) return
       call mapcat_enqrt(imap,filename,minirt,status)
       call mapcat_enqst(imap,map_status,status)
       map_status(ip_access) = access_clear
       map_status(ip_data)   = false
       map_status(ip_open)   = false
       map_status(ip_unit)   = 0
       map_status(ip_page)   = false
       call mapcat_setst(imap,map_status,status)
       if (prompt_state) then
         iprompt = 2
       else
         iprompt = 0
       end if
       call io_delfil(filename,iprompt,status)
       call mapcat_err(status,'mapcat_delete',' ')

       end
C
C
*+ mapcat_err

       subroutine mapcat_err(status,routine,message)
C      ---------------------------------------------
C
C Report an error message
C
C Given:
C   status
       integer    status
C   name of the current routine
       character  routine*(*)
C   additional message text
       character  message*(*)
C
C The error message is written to the error device or the output
C device depending on the value of STATUS. If STATUS=0 the action
C is to return without an error messahe,if STATUS .ne. 0
C then the IOLIB routine io_wrerr is used to output a complete
C error message, and the routine name is appended for a trace-back.
*-
       integer    istat
       logical    error_file_set
       common    /mapcat_local_err_set/ error_file_set

       include   '/mrao/include/iolib_errors.inc'

       if (status.eq.iolib_ok) return

C set up local error messages if not done so already
       if (.not.error_file_set) then
         istat = 0
         call io_setmsg( '(anmap)mapcat-errors:incl', istat)
         error_file_set = .true.
       end if

C output any message
       call cmd_err( status, routine, message )

       end
*$ 5) Stack Allocation Routines
*  ----------------------------

*+ stack_chk

       subroutine stack_chk(is,status)
C      -------------------------------
C
C Checks that the specified stack entry is valid
C
C Input:
C    Stack entry
       integer      is
C Returned:
C    Status
       integer      status
C
C Check that the specified stack entry is valid.  If the entry is not
C valid status is returned with a non-zero value of ILL_STKENT. No
C error message is produced by this routine.
C
*-

       include '/mrao/anmap/include/mapcat_stack.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'

       if (status.ne.0) return
       if (is.le.0 .or. is.gt.
     * (number_map_entries+number_redtape_entries)
     *    ) then
         is = ill_stkent
       end if
       end
C
C
*+ stack_enqdat

       subroutine stack_enqdat(is,data_code,status)
C      --------------------------------------------
C
C Enquire the data code for the stack entry
C
C Input:
C    Stack entry
       integer             is
C Returned
C    Data code
       integer             data_code
C    Status
       integer             status
C
C Enquire whether data is present in catalogue entry IMAP.  If data is
C present DATA_CODE is returned with the value 1 -- TRUE and 0 -- FALSE
C if data is not present.
C
*-
       include '/mrao/anmap/include/mapcat_stack.inc'

       if (status.ne.0) return
       call stack_chk(is,status)
       if (status.ne.0) then
         call mapcat_err(status,'stack_enqdat',' ')
       else
         data_code = stack_status(ip_data,is)
       end if
       end
C
C
*+ stack_enqmap

       subroutine stack_enqmap(imap,is_map,status)
C      -------------------------------------------
C
C Find stack entry for map IMAP if one exists
C
C Input:
C    Map catalogue entry
       integer        imap
C Returned
C    Stack entry
       integer        is_map
C    Status
       integer        status
C
C The stack entry of map IMAP is returned if the map is paged into the
C stack.  If map IMAP is not paged in then the returned value of
C IS_MAP is less than zero.
C
*-
       include '/mrao/anmap/include/mapcat_stack.inc'

       integer   is

C check status on entry
       if (status.ne.0) return

C look through the allocated maps and return the redtape and the map
C pointers
       is_map  = -1
       do is = 1,number_stack_entries
         if (stack_status(ip_type,is).eq.type_map) then
           if (stack_status(ip_allocated,is).eq.imap) then
             if (is_map.eq.-1) then
               is_map = is
             end if
           end if
         end if
       end do

       call mapcat_err(status,'stack_enqmap',' ')

       end
C
C
*+ stack_enqmnx

       subroutine stack_enqmnx(imap,zmnx,izmnx,status)
C      -----------------------------------------------
C
C Enquire the statistics for active map IMAP
C
C Input:
C    Map catalogue entry
       integer        imap
C Returned:
C    Real data (max, min)
       real*4         zmnx(*)
C    Integer data (max position, min position)
       integer        izmnx(*)
C    Status
       integer        status
C
C The statistics are returned for map IMAP.
C If the map is not "open" for any access then an error is returned via
C the status word (ill_active).
*-
       include '/mrao/anmap/include/mapcat_stack.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'

       integer   ib, ib0

C check status on entry
       if (status.ne.0) return

C find entry in active list of maps
       ib0 = -1
       do ib=1,max_active_maps
         if (active_list(1,ib).eq.imap) then
           ib0 = ib
         end if
       end do
       if (ib0.eq.-1) then
          status = ill_active
          goto 999
       end if

       zmnx(1)  = active_zmnx(1,ib0)
       izmnx(1) = active_izmnx(1,ib0)
       izmnx(2) = active_izmnx(2,ib0)
       zmnx(2)  = active_zmnx(2,ib0)
       izmnx(3) = active_izmnx(3,ib0)
       izmnx(4) = active_izmnx(4,ib0)

999    call mapcat_err(status,'stack_enqmnx',' ')

       end
C
C
*+ stack_enqmode

       subroutine stack_enqmode(imap,mode,ipm,ipr,iunit,status)
C      --------------------------------------------------------
C
C Return access mode for the specified map
C
C Input:
C    Map catalogue entry
       integer        imap
C Returned
C    Access mode
       integer        mode
C    Pointer to data
       integer        ipm
C    Pointer to redtape
       integer        ipr
C    Unit number if map is open for sequential access
       integer        iunit
C    Status
       integer        status
C
C The access mode and associated information is returned for map IMAP.
C If the map is not "open" for any access then an error is returned via
C the status word (ill_access).
*-
       include '/mrao/anmap/include/mapcat_stack.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'

       integer   ib, ib0

C check status on entry
       if (status.ne.0) return

C find entry in active list of maps
       ib0 = -1
       do ib=1,max_active_maps
         if (active_list(1,ib).eq.imap) then
           ib0 = ib
         end if
       end do
       if (ib0.eq.-1) then
          status = ill_active
          goto 999
       end if

       mode = active_list(2,ib0)
       if (mode.eq.mode_sequential) then
         iunit = active_list(3,ib0)
         ipm = active_list(4,ib0)
         ipr = active_list(5,ib0)
       else
         ipm = stack_status(ip_pointer,active_list(6,ib0))
         ipr = active_list(5,ib0)
       end if

999    call mapcat_err(status,'stack_enqmode',' ')

       end
C
C
*+ stack_setmnx

       subroutine stack_setmnx(imap,zmnx,izmnx,status)
C      -----------------------------------------------
C
C Update the statistics for active map IMAP
C
C Input:
C    Map catalogue entry
       integer        imap
C    Real data (max, min)
       real*4         zmnx(*)
C    Integer data (max position, min position)
       integer        izmnx(*)
C Returned
C    Status
       integer        status
C
C The statistics for active map IMAP are updated.
C If the map is not "open" for any access then an error is returned via
C the status word (ill_active).
*-
       include '/mrao/anmap/include/mapcat_stack.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'

       integer   ib, ib0

C check status on entry
       if (status.ne.0) return

C find entry in active list of maps
       ib0 = -1
       do ib=1,max_active_maps
         if (active_list(1,ib).eq.imap) then
           ib0 = ib
         end if
       end do
       if (ib0.eq.-1) then
          status = ill_active
          goto 999
       end if

       if (zmnx(1).gt.active_zmnx(1,ib0)) then
         active_zmnx(1,ib0) = zmnx(1)
         active_izmnx(1,ib0) = izmnx(1)
         active_izmnx(2,ib0) = izmnx(2)
       end if
       if (zmnx(2).gt.active_zmnx(2,ib0)) then
         active_zmnx(2,ib0) = zmnx(2)
         active_izmnx(3,ib0) = izmnx(3)
         active_izmnx(4,ib0) = izmnx(4)
       end if

999    call mapcat_err(status,'stack_setmnx',' ')

       end
C
C
*+ stack_enqpnt

       subroutine stack_enqpnt(is,ip,status)
C      -------------------------------------
C
C Enquire the pointer to the data array for the stack entry
C
C Input:
C    Stack entry
       integer             is
C Returned
C    Pointer
       integer             ip
C    Status
       integer             status
*-
       include '/mrao/anmap/include/mapcat_stack.inc'

       if (status.ne.0) return
       call stack_chk(is,status)
       if (status.ne.0) then
         call mapcat_err(status,'stack_enqpnt',' ')
       else
         ip = stack_status(ip_pointer,is)
       end if
       end
C
C
*+ stack_io_setacc

       subroutine stack_io_setacc(is,access,status)
C      -----------------------------------------
C
C Set the access state code for stack entry is
C
C Input:
C    Stack entry
       integer          is
C    Access
       character*(*)    access
C Returned:
C    Status word
       integer          status
C
C Set the access state to a stack entry.  If the requested access is
C "CLEAR" then the associated map is also removed from the active list.
*-

       include '/mrao/anmap/include/mapcat_stack.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (status.ne.0) return

       if (is.ge.1 .and. is.le.number_stack_entries) then
         if (chr_cmatch(access,'READ')) then
            stack_status(ip_access,is) = access_read
         else if (chr_cmatch(access,'WRITE')) then
            stack_status(ip_access,is) = access_write
         else if (chr_cmatch(access,'SCRATCH')) then
            stack_status(ip_access,is) = access_scratch
         else if (chr_cmatch(access,'CLEAR')) then
            stack_status(ip_access,is) = access_clear
         else
            status = ill_stkacc
         end if
       else
          status = ill_stkent
       end if
       call stack_doset(is,ip_access,status)
       call mapcat_err(status,'STACK_io_setacc','Access not reset')

       end
C
C
*+ stack_setdat

       subroutine stack_setdat(is,data_code,status)
C      --------------------------------------------
C
C Set the data code for stack entry is
C
C Input:
C    Stack entry
       integer          is
C    Data code
       integer          data_code
C Returned:
C    Status word
       integer          status
C
*-

       include '/mrao/anmap/include/mapcat_stack.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'

       if (status.ne.0) return

       if (is.ge.1 .and. is.le.number_stack_entries) then
         stack_status(ip_data,is) = data_code
       else
          status = ill_stkent
       end if
       call stack_doset(is,ip_data,status)
       call mapcat_err(status,'STACK_SETDAT','Data code not reset')

       end
C
C
*+ stack_access

       subroutine stack_access(imapi,access,data_type,size,is,status)
C      --------------------------------------------------------------
C
C Perform access function on the stack
C
C Input:
C    Map entry or file ID code
       integer          imapi
C    Access state (READ, WRITE, SCRATCH, CLEAR, ALLOCATE)
       character*(*)    access
C    Type of data
       character*(*)    data_type
C    Size of imap (words) or map data to allocate to
       integer          size
C
C Returned or Given:
C    Stack entry (returned) ; Unit number (given)
       integer          is
C Returned:
C    Status code
       integer          status
C
C Space in the internal data arrays is found and allocated.
C
C Status should be zero on entry
*-
C
C Local variables
       integer          n, np, type, mode, imap
       integer          i, ii, j, ib, ib0, ic, ic0
       logical          allocated, re_allocate
C debug variables
       logical          cmd_dblev
C
C stack status word definition
       include '/mrao/anmap/include/mapcat_stack.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C test status on entry and define allocation
       if (cmd_dblev(7)) then
         print *,'..(STACK_ACCESS) access requested: ',imapi,' ',access
       end if
       if (status.ne.0) return
       imap = imapi
       allocated = .false.

C check type
       type = type_unknown
       if (chr_cmatch(data_type,'MAP')) type=type_map
       if (chr_cmatch(data_type,'REDTAPE')) type=type_redtape
       if (type.eq.type_unknown) then
         status = ill_stktyp
         goto 999
       end if

C test for re-allocation of an existing map in core access=ALLOCATE
       re_allocate = .false.
       if (chr_cmatch(access,'ALLOCATE')) then
         imap = size
         re_allocate = .true.
       end if

C test imap for validity
       call mapcat_chk(imap,'NONE',status)
       if (status.ne.0) goto 999

C find entry in list of active maps if the data type is map
       if (type.eq.type_map) then
         ib0=-1
         do ib=max_active_maps,1,-1
           if (active_list(1,ib).eq.0) then
             ib0=ib
           end if
         end do
         if (ib0.lt.0) then
           status = ill_active
           goto 999
         end if
       end if

C look for existing allocation
       do n = 1,number_stack_entries
         if (.not.allocated) then
           if (stack_status(ip_type,n).eq.type) then
             if (stack_status(ip_allocated,n).eq.imap) then
               allocated = .true.
               is = n
               np = stack_status(ip_interlock,n)
             end if
           end if
         end if
       end do

C if re-allocation required IMAP must have been in core:
       if (re_allocate .and. (.not.allocated) ) then
         status = ill_realloc
         return
       end if

       if (.not.allocated .and.
     *     .not.chr_cmatch(access,'SEQUENTIAL')) then
C .. if no existing allocation find space for the file unless the
C    requested access is purely sequential
         if (cmd_dblev(10)) then
           print *,'..(STACK_ACCESS) About to call STACK_FNDSPC'
           if (io_yesno('..(DEBUG) Display stack ? ','no',status)) then
             call stack_display('STACK',status)
           end if
         end if
         mode = mode_direct
         call stack_fndspc(size,type,is,np,status)
C .. clear space
         do i = is,is+np-1
           do ii = i,i+stack_status(ip_interlock,i)-1
             do j = 1,length_stack_status - 3
               stack_status(j,ii) = 0
             end do
           end do
         end do
C .. allocate space
         call stack_io_setacc(is,access,status)
         if (status.eq.0) then
           do i = is,is+np-1
             stack_status(ip_data,i) = false
             stack_status(ip_interlock,i) = np
             stack_status(ip_allocated,i) = imap
           end do
         end if
         if (cmd_dblev(10)) then
           print *,'..(STACK_ACCESS) Allocated space'
           print *,'..(STACK_ACCESS) is/np = ',is,np
           if (io_yesno('..(DEBUG) Display stack ? ','no',status)) then
             call stack_display('STACK',status)
           end if
         end if

       else if (allocated) then
C .. set the access state -- change access to READ if map in core and
C    sequential access requested
         if (chr_cmatch(access,'SEQUENTIAL')) then
           call stack_io_setacc(is,'READ',status)
         else if (chr_cmatch(access,'ALLOCATE')) then
           imap = imapi
           call stack_io_setacc(is,'WRITE',status)
           do i=is,is+np-1
             stack_status(ip_allocated,i) = imap
           end do
         else
           call stack_io_setacc(is,access,status)
         end if
         mode = mode_direct

       else
C .. sequential access required define pageing buffer
         mode=mode_sequential
         active_list(3,ib0) = is
         ic0 = -1
         do ic=number_map_buffers,1,-1
           if (buffer_status(ic).eq.0) then
             ic0 = ic
           end if
         end do
         if (ic0.lt.0) then
           status = ill_buffers
           goto 999
         end if
         buffer_status(ic0) = 1
         active_list(4,ib0) = number_map_entries*size_map_entry +
     *                        (ic0-1)*size_map_buffer + 1
         active_list(6,ib0) = ic0
         active_zmnx(1,ib0) = -1.0E+30
         active_zmnx(2,ib0) =  1.0E+30
       end if

       if (type.eq.type_map) then
         active_list(1,ib0) = imap
         active_list(2,ib0) = mode
         active_list(5,ib0) = number_redtape_entries*size_redtape_entry+
     *                        (ib0-1)*512 + 1
         if (mode.eq.mode_direct) then
           active_list(6,ib0) = is
         end if
       end if

999    call mapcat_err(status,'stack_access',' ')
       end
C
C
*+ stack_fndspc

       subroutine stack_fndspc(size,type,is,np,status)
C      -----------------------------------------------
C
C Find space in the stack for the requested internal file
C
C Input:
C    Size of internal file requested
       integer        size
C    Type of internal file
       integer        type
C Returned:
C    Start stack entry for internal file
       integer        is
C    Number of stack entries from is
       integer        np
C    Status
       integer        status
C
C Space is found in the stack array for the requested internal file.
C No allocation is made by this routine.
*-

       include '/mrao/anmap/include/mapcat_stack.inc'
       include '/mrao/anmap/include/mapcat_errors.inc'
       include '/mrao/include/iolib_constants.inc'

C arrays to hold "scores"
       integer    stack_test(max_number_stack_entries)
       integer    buff_test(max_number_stack_entries)
C mini redtape and filename
       integer    minirt(10)
       character  filename*(iolen_file)
C counters etc.
       integer    i, ii, i1, i2, n
C length of buffer
       integer    len_buff
C number of buffers
       integer    nb
C best buffer
       integer    nb_best, buff_best
C debug information
       logical    cmd_dblev

       if (status.ne.0) return

C type specific initialisation
       if (cmd_dblev(7)) then
         print *,'..(STACK_FNDSPC) type/size = ',type,size
       end if
       if (type.eq.type_redtape) then
         i1 = 1
         i2 = number_redtape_entries
         len_buff = 1
       else if (type.eq.type_map) then
         i1 = number_redtape_entries + 1
         i2 = number_redtape_entries + number_map_entries
         len_buff = size/size_map_entry
         if ((size-size_map_entry*len_buff) .gt. 0) then
           len_buff = len_buff+1
         end if
         if (len_buff.gt.12) then
           len_buff = 16
         else if (len_buff.gt.8) then
           len_buff = 12
         else if (len_buff.gt.4) then
           len_buff = 8
         else if (len_buff.gt.2) then
           len_buff = 4
         end if
       else if (type.eq.type_work) then
         i1 = number_redtape_entries + 1
         i2 = number_redtape_entries + number_work_entries
         len_buff = size/size_work_entry
         if ((size-size_work_entry*len_buff) .gt. 0) then
           len_buff = len_buff+1
         end if
       end if
       if (cmd_dblev(7)) then
         print *,'..(STACK_FNDSPC) i1/2 & len_buff = ',i1,i2,len_buff
       end if

C construct test array of "scores" for each stack entry
       do i = i1,i2
         stack_test(i) = -1000000000
         if (stack_status(ip_access,i).eq.access_clear) then
           if (stack_status(ip_data,i).eq.false) then
             stack_test(i) = 0
           else
             call mapcat_enqrt(stack_status(ip_allocated,i),
     *                         filename,minirt,status)
             stack_test(i) = minirt(5)*minirt(6)
           end if
         end if
       end do

C search the "scores" array and construct "scores" for each buffer
       nb = 0
       do i = i1,i2-len_buff+1
         nb = nb + 1
         buff_test(nb) = 0
         do ii = i,i+len_buff-1
           buff_test(nb) = buff_test(nb) + stack_test(ii)
         end do
       end do

C find best buffer
       nb_best = -1
       buff_best = -100000000
       do n = 1,nb
         if (abs(buff_test(n)).lt.abs(buff_best)) then
           buff_best = buff_test(n)
           nb_best = n
         end if
       end do
       if (nb_best.gt.0) then
         is = (nb_best-1) + i1
         np = len_buff
       else
         status = ill_stkalloc
       end if
       call mapcat_err(status,'stack_fndspc','Unable to find Space')
       if (cmd_dblev(7)) then
         print *,'..(STACK_FNDSPC) is/np = ',is,np
       end if
       end
C
C
*+ stack_clracc

       subroutine stack_clracc(imap,status)
C      ------------------------------------
C
C Clear the access states to map imap
C
C Given:
C     Map catalogue entry
        integer      imap
C Returned:
C     Status
        integer      status
C
C The access state to map IMAP is cleared.  If the map was paged in
C then a record of its presence in the stack will remain for possible
C future reference.
C
*-
       include '/mrao/anmap/include/mapcat_stack.inc'

       integer   i, j

       if (status.ne.0) return
       do i = 1,number_stack_entries
         if (stack_status(ip_allocated,i).eq.imap) then
           stack_status(ip_access,i) = access_clear
         end if
       end do
       do i = 1,max_active_maps
         if (active_list(1,i).eq.imap) then
           if (active_list(2,i).eq.mode_sequential) then
             buffer_status(active_list(6,i)) = 0
           end if
           do j=1,6
            active_list(j,i) = 0
           end do
           active_zmnx(1,i) = -1.0E+30
           active_zmnx(2,i) =  1.0E+30
         end if
       end do
       call mapcat_err(status,'stack_clracc',' ')
       end
C
C
*+ stack_init

       subroutine stack_init(nmaps,smap,nbuffers,sbuffer,status)
C      ----------------------------------------------------------
C
C Initialise the stack
C
C Given:
C     number of and size of map stack entries
        integer      nmaps
        integer      smap
C     number of and size of buffer entries
        integer      nbuffers
        integer      sbuffer
C
C Returned:
C     Status
        integer      status
C
C Initialise the map stack. Two functions are performed by the routine:
C    (a) define the total number of map slots and buffers and sizes
C    (b) initialise all the pointers and map_status block
C
C If the number of maps specified in the call to this routine is less
C than zero then only function (b) is carried out -- in this way the
C stack can be re-initialised to the same size as in the initial
C definition.
*-
       include '/mrao/anmap/include/mapcat_stack.inc'

       integer   i, j, is, ip, irange

       if (status.ne.0) return

       if (nmaps.gt.0) then
         number_map_entries   = nmaps
         number_stack_entries = number_redtape_entries +
     *                          number_map_entries +
     *                          number_work_entries
         size_map_entry       = smap
         size_work_entry      = smap
         number_map_buffers   = nbuffers
         size_map_buffer      = sbuffer
       end if
       do i=1,max_active_maps
         active_list(1,i) = 0
       end do
       do i=1,number_map_buffers
         buffer_status(i) = 0
       end do
       is = 0
       ip = 1
       do i = 1,number_redtape_entries
         is = is + 1
         do j = 1,length_stack_status - 3
           stack_status(j,is) = 0
         end do
         stack_status(ip_type,is)     = type_redtape
         stack_status(ip_pointer,is)  = ip
         stack_status(ip_size,is)     = size_redtape_entry
         ip = ip + stack_status(ip_size,is)
       end do
       ip = 1
       do i = 1,number_map_entries
         is = is + 1
         do j = 1,length_stack_status - 3
           stack_status(j,is) = 0
         end do
         stack_status(ip_type,is)     = type_map
         stack_status(ip_pointer,is)  = ip
         stack_status(ip_size,is)     = size_map_entry
         ip = ip + stack_status(ip_size,is)
       end do
       ip = 1
       if (number_work_entries.ge.1) then
         irange = number_work_entries
         do i = 1,irange
           is = is + 1
           do j = 1,length_stack_status - 3
             stack_status(j,is) = 0
           end do
           stack_status(ip_type,is)     = type_work
           stack_status(ip_pointer,is)  = ip
           stack_status(ip_size,is)  = size_work_entry
           ip = ip + stack_status(ip_size,is)
         end do
       end if
       call mapcat_err(status,'stack_init',
     *                 'Stack Initialisation Failed')
       end
C
C
*+ stack_doset

       subroutine stack_doset(is,ip,status)
C      ------------------------------------
C
C Update all INTERLOCKED stack_status entries
C
C Input:
C    Stack entry
       integer          is
C    Stack_status word entry
       integer          ip
C Returned
C    Status
       integer          status
C
C For maps larger than the nominal size of a map in the map stack more
C than one slot of the stack will be allocated to this map.  This
C routine ensures that all stack entries allocated to the same map have
C consistent stack_status work entries.
*-
       integer          i
       include '/mrao/anmap/include/mapcat_stack.inc'

       if (status.ne.0) return
       do i = is,is+stack_status(ip_interlock,is)-1
         stack_status(ip,i) = stack_status(ip,is)
       end do
       call mapcat_err(status,'stack_doset',' ')

       end
C
C
*+ stack_remove

       subroutine stack_remove(imap,status)
C      ------------------------------------
C
C Remove allocation of stack to imap if any
C
C Input:
C    Map allocation to remove
       integer          imap
C Returned
C    Status
       integer          status
C
C All record of map IMAP being allocated space in the stack is removed.
C If IMAP is active then the reference to active map IMAP is also removed.
C
*-
       integer          i, j
       include '/mrao/anmap/include/mapcat_stack.inc'

       if (status.ne.0) return
       do i = 1,number_stack_entries
         if (stack_status(ip_allocated,i).eq.imap) then
           stack_status(ip_allocated,i) = false
           stack_status(ip_data,i) = false
           stack_status(ip_access,i) = access_clear
         end if
       end do
       do i = 1,max_active_maps
         if (active_list(1,i).eq.imap) then
           if (active_list(2,i).eq.mode_sequential) then
             buffer_status(active_list(6,i)) = 0
           end if
           do j=1,6
             active_list(j,i) = 0
           end do
           active_zmnx(1,i) = -1.0E+30
           active_zmnx(2,i) =  1.0E+30
         end if
       end do
       call mapcat_err(status,'stack_remove',' ')

       end
C
C
*+ stack_ldredt

       subroutine stack_ldredt(ipr,status)
C      -----------------------------------
C
C Load the standard common blocks with the redtape stored at IPR
C
C Given:
C   redtape address in redt_array
       integer     ipr
C Returned:
C   error return
       integer     status
C
*-
       include '/mrao/anmap/include/mapcat_stack.inc'

       call ldredt(redt_array(ipr),status)
       call mapcat_err(status,'stack_ldredt',' ')

       end
C
C
*+ stack_dpredt

       subroutine stack_dpredt(ipr,status)
C      -----------------------------------
C
C Sump the standard common blocks into the redtape array stored at IPR
C
C Given:
C   redtape address in redt_array
       integer     ipr
C Returned:
C   error return
       integer     status
C
*-
       include '/mrao/anmap/include/mapcat_stack.inc'

       call dpredt(redt_array(ipr),status)
       call mapcat_err(status,'stack_ldredt',' ')

       end
C
C
*+ stack_display

       subroutine stack_display(option,status)
C      ---------------------------------------
C
C Display stack allocation or active map information
C
C Given:
C    Option to display (STACK, ACTIVE, SIZES)
       character option*(*)
C Returned:
C    Status
       integer      status
*-

       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/anmap/include/mapcat_stack.inc'

       integer   iout, n, i

C check status on entry
       if (status.ne.0) return

C find output unit number
       call io_enqout(iout)

C Either display STACK entries of ACTIVE maps
       if (chr_cmatch(option,'STACK')) then
         write(iout,50)
         do n = 1,number_stack_entries
           write(iout,100) n,
     *          (stack_status(i,n), i=1,length_stack_status)
         end do
         write (iout,*) ' '
 50      format(1x/' Entry   Status-Word '/1x,80('-'))
100      format('  ',i3,'     ',10i7)

       else if (chr_cmatch(option,'ACTIVE')) then
         write(iout,150)
         do n = 1,max_active_maps
           write(iout,200) n,
     *          (active_list(i,n), i=1,length_active_list)
         end do
         write (iout,250) number_map_buffers,
     *                   (buffer_status(i), i=1,number_map_buffers)
         write(iout,*)' '
150      format(1x/' Entry   Active-Word '/1x,80('-'))
200      format('  ',i3,'     ',10i7)
250      format(1x/' Number of buffers = ',I4/
     *             ' Buffer-status     = ',15I2)

       else if (chr_cmatch(option,'SIZES')) then
         write(iout,300)max_number_stack_entries,
     *                  max_active_maps
         write(iout,400)'Redtape    ',number_redtape_entries,
     *         size_redtape_entry
         write(iout,400)'Maps       ',number_map_entries,size_map_entry
         write(iout,400)'Map-buffers',number_map_buffers,size_map_buffer
         write(iout,400)'Work       ',number_work_entries,
     *         size_work_entry
300      format(1x/' Maximum number of stack entries : ',I4/
     *             ' Maximum number of active maps   : ',I4/
     *          1x/    ' Type       ',' Number ','     Size ')
400      format(1x,a11,i5,i12)
         write(iout,*)' '

       end if
999    call mapcat_err(status,'stack_display',' ')

       end
*$ Sub-system to perform Map-catalogue maintenance
*  -----------------------------------------------
C
C Last updated Paul Alexander 2/3/92
C SUNOS Version 1.0
C
*+ mapcat_sys

       subroutine mapcat_sys(initial_command,map_array,status)
C      -------------------------------------------------------
C
C Provide monitor and display facilities to the catalogue and stack
C
C Input:
C    Initial command string
       character*(*)          initial_command
C    Map space
       real*4                 map_array(*)
C Returned:
C    Status
       integer                status
C
C Provide a number of monitor and command facilities for the catalogue
C and stack system.
C
*-
       include '/mrao/anmap/include/anmap_sys_pars.inc'
       include '/mrao/anmap/include/mapcat_pars.inc'
       include '/mrao/include/iolib_constants.inc'

C define list of commands
       integer          ic_help
       parameter       (ic_help = 1 )
       integer          ic_add_to_cat
       parameter       (ic_add_to_cat = ic_help       + 1)
       integer          ic_rem_fr_cat
       parameter       (ic_rem_fr_cat = ic_add_to_cat + 1)
       integer          ic_del_map
       parameter       (ic_del_map    = ic_rem_fr_cat + 1)
       integer          ic_renam_cat
       parameter       (ic_renam_cat  = ic_del_map    + 1)
       integer          ic_export_map
       parameter       (ic_export_map = ic_renam_cat  + 1)
       integer          ic_examine
       parameter       (ic_examine    = ic_export_map + 1)
       integer          ic_list_cat
       parameter       (ic_list_cat   = ic_examine    + 1)
       integer          ic_del_temp
       parameter       (ic_del_temp   = ic_list_cat   + 1)
       integer          ic_def_map
       parameter       (ic_def_map    = ic_del_temp   + 1)
       integer          ic_set_mode
       parameter       (ic_set_mode   = ic_def_map    + 1)
       integer          number_std_commands
       parameter       (number_std_commands = ic_set_mode)
       integer          ic_def_dir
       parameter       (ic_def_dir    = ic_set_mode   + 1)
       integer          ic_init_cat
       parameter       (ic_init_cat   = ic_def_dir    + 1)
       integer          ic_init_stack
       parameter       (ic_init_stack = ic_init_cat   + 1)
       integer          ic_dis_cat
       parameter       (ic_dis_cat    = ic_init_stack + 1)
       integer          ic_dis_stack
       parameter       (ic_dis_stack  = ic_dis_cat    + 1)
       integer          ic_dis_active
       parameter       (ic_dis_active = ic_dis_stack  + 1)
       integer          ic_dis_sizes
       parameter       (ic_dis_sizes  = ic_dis_active + 1)
       integer          ic_dis_opts
       parameter       (ic_dis_opts   = ic_dis_sizes + 1)
       integer          ic_clr_alloc
       parameter       (ic_clr_alloc  = ic_dis_opts   + 1)
       integer          ic_edit_cat
       parameter       (ic_edit_cat   = ic_clr_alloc  + 1)
       integer          ic_edit_stack
       parameter       (ic_edit_stack = ic_edit_cat + 1  )
       integer          ic_prompt
       parameter       (ic_prompt     = ic_edit_stack + 1)
       integer          ic_report
       parameter       (ic_report     = ic_prompt + 1)
       integer          ic_check
       parameter       (ic_check      = ic_report  + 1)
       integer          ic_area
       parameter       (ic_area       = ic_check   + 1)
       integer          ic_dir_def
       parameter       (ic_dir_def    = ic_area    + 1)
       integer          ic_dir_cha
       parameter       (ic_dir_cha    = ic_dir_def + 1)
       integer          ic_dir_rem
       parameter       (ic_dir_rem    = ic_dir_cha + 1)
       integer          ic_dir_lis
       parameter       (ic_dir_lis    = ic_dir_rem + 1)
       integer          ic_ver_cat
       parameter       (ic_ver_cat    = ic_dir_lis + 1)
       integer          ic_file_man
       parameter       (ic_file_man   = ic_ver_cat + 1)
       integer          ic_get
       parameter       (ic_get        = ic_file_man+ 1)
       integer          number_commands
       parameter       (number_commands = ic_get)
       character*80     liscom(number_commands)

       data             liscom(ic_help)       /
     *  'help ...................... obtain help on the map catalogue' /
       data             liscom(ic_add_to_cat) /
     *  'add-to-catalogue .......... add map(s) to the catalogue'      /
       data             liscom(ic_rem_fr_cat) /
     *  'remove-from-catalogue ..... remove a map from the catalogue'  /
       data             liscom(ic_examine)    /
     *  'examine-catalogue ......... examine the map catalogue'        /
       data             liscom(ic_list_cat)   /
     *  'list-catalogue ............ list (a portion of) the catalogue'/
       data             liscom(ic_renam_cat)  /
     *  'rename-catalogue-entry .... rename source in catalogue entry' /
       data             liscom(ic_export_map) /
     *  'export-permanent-map ...... set permanent file name for a map'/
       data             liscom(ic_del_map)    /
     *  'delete-catalogue-entry .... delete a catalogued map from disc'/
       data             liscom(ic_del_temp)   /
     *  'delete-temporary-maps ..... delete temporary maps'/
       data             liscom(ic_def_map)   /
     *  'set-default-map ........... set the default map'/
       data             liscom(ic_set_mode)   /
     *  'advanced-mode ............. set mode to standard/advanced'    /
       data             liscom(ic_def_dir)    /
     *  'default-map-directory ..... set the default map directory'    /
       data             liscom(ic_init_cat)   /
     *  're-initialise-catalogue ... re-initialiase the catalogue'     /
       data             liscom(ic_init_stack) /
     *  're-initialise-stack ....... re-initialise the internal store' /
       data             liscom(ic_dis_cat)    /
     *  'display-catalogue-entry ... display catalogue entry in full'  /
       data             liscom(ic_dis_stack)  /
     *  'display-stack-entry ....... display stack entry in full'      /
       data             liscom(ic_dis_active) /
     *  'display-active-maps ....... display active maps and buffers ' /
       data             liscom(ic_dis_sizes) /
     *  'display-sizes ............. display sizes/numbers in stack  ' /
       data             liscom(ic_dis_opts) /
     *  'display-options ........... display current option settings ' /
       data             liscom(ic_clr_alloc)  /
     *  'clear-allocation .......... remove any allocation to an entry'/
       data             liscom(ic_edit_cat)   /
     *  'edit-catalogue-entry ...... change a catalogue entry'         /
       data             liscom(ic_edit_stack) /
     *  'edit-stack-entry .......... change a stack entry'             /
       data             liscom(ic_prompt)     /
     *  'prompt-state .............. change prompt state'              /
       data             liscom(ic_report)     /
     *  'report-mode ............... change the report mode'           /
       data             liscom(ic_check)      /
     *  'check-temporary-maps ...... change checking on EXIT'          /
       data             liscom(ic_area)      /
     *  'set-maximum-area-to-read .. set area at which whole map read' /
       data             liscom(ic_dir_def)   /
     *  'define-directory .......... define catalogue file for use'    /
       data             liscom(ic_dir_cha)   /
     *  'change-directory .......... change catalogue file to use'     /
       data             liscom(ic_dir_rem)   /
     *  'remove-directory .......... remove a catalogue from the list' /
       data             liscom(ic_dir_lis)   /
     *  'list-directories .......... list catalogue files defined'     /
       data             liscom(ic_ver_cat)   /
     *  'verify-catalogue .......... verify catalogue file entries'    /
       data             liscom(ic_file_man)   /
     *  'file-manager .............. perform filespace management'     /
       data             liscom(ic_get)       /
     *  'get ....................... get map-catalogue information'    /


C local variables
       integer        n, nc, imap, ilist(256), iout, i, map_status(10)
       integer        minirt(10), icomm, nlist, list_1, list_2
       character      command*40, source*40, program*20,
     *                old_filename*(iolen_file),
     *                new_filename*(iolen_file),
     *                default_directory*(iolen_dir),
     *                command_line*120,
     *                string*80
       integer        len_init, len_com, len_st, len_dir, len_cli,
     *                length
       logical        exit_on_completion, mode_advanced, prompt_state,
     *                report_mode, check_mode

C functions
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

       common /mapcat_sys_save_mode/ mode_advanced

C Initialisation of routine:
C check status on entry
       if (status.ne.0) return

C check for immediate exit
       call io_enqcli(command_line,len_cli)
       len_init = chr_lenb(initial_command)
       exit_on_completion = len_init.gt.0 .or. len_cli.gt.0
       mode_advanced      = mode_advanced .or. exit_on_completion
       prompt_state       = .true.

C Read in command:
1000   continue
       if (mode_advanced) then
         nc = number_commands
       else
         nc = number_std_commands
       end if
       status = 0
       if (len_init.eq.0) then
         call cmd_scncmd(.false.)
         call cmd_getcmd('Map-Catalogue> ',liscom,nc,icomm,status)
         call cmd_scncmd(.true.)
         if (icomm.eq.0) return
         call mapcat_err(status,'Map-Catalogue-System',' ')
         command = liscom(icomm)
         len_com = chr_lenw(command)
       else
         command = initial_command
         len_com = chr_lenw(command)
       end if

C Decode command:
       if (chr_cmatch(command(1:len_com),liscom(ic_help))) then
         call io_enqcli(command_line,len_cli)
         call hlp_setfil( mapcat_helpfile, status )
         call hlp_system( ' ', ' ', .true., .true., status)

       else if (chr_cmatch(command(1:len_com),
     *                     liscom(ic_add_to_cat)))then
         call mapcat_addtocat(' ',prompt_state,count,status)

       else if (chr_cmatch(command(1:len_com),
     *                     liscom(ic_rem_fr_cat)))then
         do i=1,256
           ilist(i) = i
         end do
         call io_getlst('Catalogue-index-list : ',' ',
     *               string,ilist,256,n,status)
         do i=1,n
           call mapcat_remove(ilist(i),.true.,status)
         end do

       else if (chr_cmatch(command(1:len_com),
     *                     liscom(ic_renam_cat)))then
         call mapcat_getmap('Catalogue-entry : ','Default-Map','NONE',
     *                   imap,status)
         call mapcat_chk(imap,'READ',status)
         call mapcat_enqsr(imap,source,program,status)
         len_st = chr_lenb(source)
         call io_getwrd('New-source-name : ',source(1:len_st),
     *               source,len_st,status)
         call mapcat_setsr(imap,source(1:len_st),program,status)

       else if (chr_cmatch(command(1:len_com),
     *                     liscom(ic_export_map)))then
         call mapcat_getmap('Catalogue-entry : ','Default-Map','NONE',
     *                   imap,status)
         call mapcat_chk(imap,'READ',status)
         call mapcat_enqsr(imap,source,program,status)
         call mapcat_enqrt(imap,old_filename,minirt,status)
         call mapcat_enqst(imap,map_status,status)
         new_filename = source
         len_st = chr_lenb(source)
         i = chr_ilstc(source(1:len_st),'-')
         if (((len_st-i).le.4) .and. (i.lt.len_st)) then
           new_filename(i:i) = file_char_type
         end if
         len_st = chr_lenb(new_filename)
         do i=1,len_st
           if (new_filename(i:i).eq.'-') then
             new_filename(i:i) = file_char_sep
           end if
         end do
         call io_getfil('Permanent-file-name : ',
     *                  new_filename(1:len_st),
     *                  new_filename,status)
         len_st = chr_lenb(new_filename)
         string =
     *    old_filename( 1:chr_ilstc(old_filename,dir_char_right) )//
     *    new_filename(chr_ilstc(new_filename,dir_char_right)+1:len_st)
         new_filename = string
         call io_rnmfil(old_filename,new_filename,2,status)
         call mapcat_setrt(imap,new_filename,minirt,status)
         map_status(ip_page) = false
         call mapcat_setst(imap,map_status,status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_del_map)))then
         if (prompt_state) then
           call io_wrout('.. WARNING this command will delete your map')
           call io_wrout('.. press ESC to abort this command')
         end if
         call mapcat_getmap('Catalogue-entry : ','default-map',
     *                   'NONE',imap,status)
         call mapcat_chk(imap,'READ',status)
         call mapcat_delete(imap,prompt_state,status)
         call stack_remove(imap,status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_del_temp)))then
         call mapcat_deltemp(prompt_state,'no',status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_def_map)))then
         call mapcat_getmap('Catalogue-entry : ','default-map',
     *                   'NONE',imap,status)
         call map_setdef(imap,status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_examine)))then
         call mapcat_exlist(ilist,nlist,status)
         call io_enqout( iout )
         call mapcat_list(ilist,nlist,iout,1,status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_list_cat)))then
         call io_enqout(iout)
         call io_geti('Range to list low : ','1',list_1,status)
         call io_geti('Range to list high : ','256',list_2,status)
         nlist = 0
         list_1 = max(1,list_1)
         list_2 = max(1,list_2)
         list_1 = min(256,list_1)
         list_2 = min(256,list_2)
         do n = list_1,list_2
           nlist = nlist+1
           ilist(nlist) = n
         end do
         call mapcat_list(ilist,nlist,iout,1,status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_def_dir)))then
         call mapcat_enqdefdir(default_directory,status)
         call io_getwrd('Default-Directory : ','*',default_directory,
     *               len_dir,status)
         call mapcat_setdefdir(default_directory(1:len_dir),status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_set_mode)))then
         mode_advanced = io_onoff('Advanced mode (on/off) : ',
     *                            'off',status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_prompt)))then
         prompt_state = io_onoff('Prompt mode (on/off) : ','on',status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_report)))then
         report_mode = io_onoff('Report mode (on/off) : ','off',status)
         call mapcat_setrm(report_mode)

       else if (chr_cmatch(command(1:len_com),liscom(ic_check)))then
         check_mode = io_onoff('Check mode (on/off) : ','off',status)
         call mapcat_setch(check_mode)

       else if (chr_cmatch(command(1:len_com),liscom(ic_area)))then
         call mapcat_setarea(status)

       else if (mode_advanced) then
         if (chr_cmatch(command(1:len_com),liscom(ic_init_cat)))then
           call mapcat_init(status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_init_stack)))then
           call stack_init(-1,0.0,-1,0.0,status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_dis_cat)))then
           call mapcat_display(status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_dis_stack)))then
           call stack_display('STACK',status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_dis_active)))then
           call stack_display('ACTIVE',status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_dis_sizes)))then
           call stack_display('SIZES',status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_dis_sizes)))then
           call mapcat_disopts(prompt_state,status)

         else if (chr_cmatch(command(1:len_com),
     *            liscom(ic_clr_alloc)))then
           do i=1,256
             ilist(i) = i
           end do
           call io_getlst('Catalogue-index-list : ',' ',
     *                 string,ilist,256,n,status)
           do i=1,n
             imap = ilist(i)
             if (imap.ne.0) then
               call mapcat_enqst(imap,map_status,status)
               map_status(ip_access) = access_clear
               map_status(ip_open) = 0
               map_status(ip_unit) = 0
               call mapcat_setst(imap,map_status,status)
             end if
           end do

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_edit_cat)))then
           call mapcat_edit(status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_edit_stack)))then
           call stack_edit(status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_dir_def)))then
           new_filename = ' '
           call io_getwrd('Catalogue-file : ',' ',
     *                 new_filename, length, status)
           call mapcat_dir_define(new_filename,status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_dir_cha)))then
           new_filename = ' '
           call io_getwrd('Catalogue-file : ',' ',
     *                 new_filename, length, status)
           call mapcat_dir_select(new_filename,status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_dir_rem)))then
           new_filename = ' '
           call io_getwrd('Catalogue-file : ',' ',
     *                 new_filename, length, status)
           call mapcat_dir_remove(new_filename,status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_dir_lis)))then
           call mapcat_dir_list(status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_ver_cat)))then
           call mapcat_verify(status)

         else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_file_man)))then
           call mapcat_file_manager(status)

         else if (chr_cmatch(command(1:len_com),liscom(ic_get)))then
           call mapcat_get(ilist,status)

         else
           call io_wrout('*** unknown Catalogue-System command')

         end if
       else
         call io_wrout('*** unknown Catalogue-System command')

       end if

C Take action on completion of command:
999    call mapcat_err(status,'Map-Catalogue-System',' ')
       if (exit_on_completion) return
       goto 1000

       end
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
       include '/mrao/anmap/include/mapcat_cat.inc'

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

       include '/mrao/anmap/include/mapcat_cat.inc'
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
C
C
*+ mapcat_remove

       subroutine mapcat_remove(imap,check_status,status)
C      --------------------------------------------------
C
C Remove an entry from the map catalogue
C Input:
C    Map entry
       integer      imap
C    Check status of catalogue entry before removing it
       logical      check_status
C Returned:
C    Status
       integer      status
C
C Remove an entry from the map catalogue, the disc file will remain.
C If check_status is set to true then the entry must be a permanent
C map file. If set to false this check is not performed.
*-
       include '/mrao/anmap/include/mapcat_pars.inc'

       integer   map_status(10)

       if (status.ne.0) return
       call mapcat_end(imap,status)
       call mapcat_enqst(imap,map_status,status)
       if (check_status) then
         if (map_status(ip_page).eq.true) then
           call io_wrout(
     *     '***(CATALOGUE) Catalogue entry is not a permanent file')
           call io_wrout(
     *     '***(CATALOGUE) Use Export-Permanent-Map or Delete-Map-File')
           return
         end if
       end if
       map_status(ip_access) = access_clear
       map_status(ip_data)   = false
       map_status(ip_open)   = false
       map_status(ip_unit)   = 0
       map_status(ip_page)   = false
       call mapcat_setst(imap,map_status,status)
       call stack_remove(imap,status)
       end
C
C
*+ mapcat_deltemp

       subroutine mapcat_deltemp(prompt_state,default,status)
C      ------------------------------------------------------
C
C Delete temporary maps
C
C Input:
C    flag controling prompting
       logical         prompt_state
C    default string (yes or no)
       character*(*)   default
C    Status
       integer         status
C
*-

       include '/mrao/anmap/include/mapcat_cat.inc'

C strings
       character*80  string
C counter
       integer       n
C string lengths etc
       integer       len_string, len_source, len_program
       integer       chr_lenb
C logical functions
       logical       io_yesno

C check status on entry
       if (status.ne.0) return

C run through the list and test each entry in turn
       call mapcat_open(status)
       do n = 1,max_cat_entries
         call mapcat_read(n,status)
         if (status.ne.0) goto 999
         if (current_map_status(ip_data).eq.true) then
           if (current_map_status(ip_page).eq.true) then
             len_source  = chr_lenb(current_source)
             len_program = chr_lenb(current_program)
             write (string,100) current_map,
     *                          current_source(1:len_source),
     *                          current_program(1:len_program)
             len_string = chr_lenb(string)
             if (prompt_state) then
               if (io_yesno(string(1:len_string),default,status)) then
                 call mapcat_delete(n,.false.,status)
                 call stack_remove(n,status)
               end if
             else
               call mapcat_delete(n,.false.,status)
               call stack_remove(n,status)
             end if
           end if
         end if
       end do

100    format('Delete map ',i3,' [',a,'-',a,'] : ')
999    call mapcat_err(status,'mapcat_deltemp',' ')

       end
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

       include '/mrao/anmap/include/mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'

C counter
       integer       n
C local status variable
       integer       istat


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

       include '/mrao/anmap/include/mapcat_cat.inc'

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
       include '/mrao/anmap/include/anmap_sys_pars.inc'

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
       st(3:3) = file_char_sep
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
             if (operating_system.eq.'io_system') then
               len_text=chr_lend(filename,';')
             else if (operating_system.eq.'UNIX') then
               len_text=chr_lenb(filename)
             end if
             call chr_chctoi(filename(len_text-17:len_text-16),test_date(1),
     *                   status)
             call chr_chctoi(filename(len_text-15:len_text-14),test_date(2),
     *                   status)
             call chr_chctoi(filename(len_text-13:len_text-12),test_date(3),
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
         call io_setout(1)

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
             call chr_chctoi(filename(len_text-17:len_text-16),test_date(1),
     *                   status)
             call chr_chctoi(filename(len_text-15:len_text-14),test_date(2),
     *                   status)
             call chr_chctoi(filename(len_text-13:len_text-12),test_date(3),
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
C
C
*+ stack_edit

       subroutine stack_edit(status)
C      -----------------------------
C
C Edit the statck entries
C
C Returned:
C    Status
       integer      status
*-

       include '/mrao/anmap/include/mapcat_stack.inc'

       character string*25
       integer   i, is, itype

C check status on entry
       if (status.ne.0) return

C read entry to edit
       itype = 0
       call io_geti('Type (0=stack, 1=active, 2=buffer) : ','*',
     *           itype,status)
       if (itype.eq.0) then
         call io_geti('Entry to edit (0=quit) : ','0',is,status)
         if (is.le.0 .or. is.gt.number_stack_entries) return
         if (status.ne.0) goto 999
         call io_wrout(' ')
         call io_wrout('.. edit stack_status word')
         do i = 1,length_stack_status
           write(string,'(''  Stack_Status('',I1,'') : '')') i
           call io_geti(string(1:20),'*',stack_status(i,is),status)
         end do
         call io_wrout('.. entry replaced')
         call io_wrout(' ')

       else if (itype.eq.1) then
         call io_geti('Entry to edit (0=quit) : ','0',is,status)
         if (is.le.0 .or. is.gt.max_active_maps) return
         if (status.ne.0) goto 999
         call io_wrout(' ')
         call io_wrout('.. edit active_list word')
         do i = 1,length_active_list
           write(string,'(''  Active_List('',I1,'') : '')') i
           call io_geti(string(1:19),'*',active_list(i,is),status)
         end do
         call io_wrout('.. entry replaced')
         call io_wrout(' ')

       else if (itype.eq.2) then
         do i = 1,number_map_buffers
           write(string,'(''  Buffer_Status('',I1,'') : '')') i
           call io_geti(string(1:21),'*',buffer_status(i),status)
         end do

       end if
999    call mapcat_err(status,'stack_edit',' ')

       end
C
C
*+ mapcat_dir_define

       subroutine mapcat_dir_define(filename,status)
C      ---------------------------------------------
C
C Define a new directory
C
C Input:
C   file name of the map catalogue
       character*(*)      filename
C Returned
C   status word
       integer            status
C
C Define the file name of the map catalogue.  The file may or may
C not exist.  If it does not exist it will be initialised.
C It is not possible to check if the file has the correct format for
C a catalogue file.
*-
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/anmap/include/mapcat_cat.inc'

       integer   n, length
       logical   found

       if (status.ne.0) return
       found = .false.
       do n=1,10
         if (defined_cat_file(n).eq.1) then
           if (chr_fmatch(filename,list_cat_file(n))) found = .true.
         end if
       end do
       if (.not.found) then
         do n=1,10
           if (.not.found .and. defined_cat_file(n).eq.0) then
             call io_makfil(' ',filename,'mctg',list_cat_file(n),length)
             defined_cat_file(n) = 1
             found = .true.
           end if
         end do
       end if
       if (.not.found) then
         call cmd_wrerr('MAP-CATALOGUE',
     *                  'No more sub-directories allowed')
       end if
       call mapcat_err(status,'mapcat_dir_define',
     *              'Unable to define new catalogue directory')

       end
C
C
*+ mapcat_dir_select

       subroutine mapcat_dir_select(filename,status)
C      ---------------------------------------------
C
C Select a new directory
C
C Input:
C   file name of the map catalogue
       character*(*)      filename
C Returned
C   status word
       integer            status
C
C Select one of the currently defined map catalogue files.
*-
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/anmap/include/mapcat_cat.inc'

       integer   n
       logical   found

       if (status.ne.0) return
       if (chr_lenb(filename).eq.0 .or.
     *     chr_fmatch(filename,default_cat_file)) then
         call mapcat_close(status)
         current_cat_file = 0
         cat_file = default_cat_file
         call mapcat_open(status)
         return
       end if
       found = .false.
       do n=1,10
         if (defined_cat_file(n).eq.1) then
           if (chr_fmatch(filename,list_cat_file(n))) then
             found = .true.
             call mapcat_close(status)
             current_cat_file = n
             cat_file = list_cat_file(n)
           end if
         end if
       end do
       if (found) then
         call mapcat_open(status)
       else
         call cmd_wrerr('MAP-CATALOGUE','No such directory')
       end if

       call mapcat_err(status,'mapcat_dir_select',
     *              'Unable to select new catalogue file')

       end
C
C
*+ mapcat_dir_remove

       subroutine mapcat_dir_remove(filename,status)
C      ---------------------------------------------
C
C Remove the catalogue directory from the defined list
C
C Input:
C   file name of the map catalogue
       character*(*)      filename
C Returned
C   status word
       integer            status
C
C Removes one of the currently defined map catalogue files.
*-
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/anmap/include/mapcat_cat.inc'

       integer   n
       logical   found

       if (status.ne.0) return
       found = .false.
       do n=1,10
         if (defined_cat_file(n).eq.1) then
           if (chr_fmatch(filename,list_cat_file(n))) then
             found = .true.
             if (current_cat_file.eq.n) then
               call mapcat_close(status)
               current_cat_file = 0
               cat_file = default_cat_file
               call mapcat_open(status)
             end if
             defined_cat_file(n) = 0
           end if
         end if
       end do
       if (.not.found) then
         call cmd_wrerr('MAP-CATALOGUE','No such directory')
       end if

       call mapcat_err(status,'mapcat_dir_select',
     *              'Unable to select new catalogue file')

       end
C
C
*+ mapcat_dir_list

       subroutine mapcat_dir_list(status)
C      ----------------------------------
C
C List the map catalogue directory
C
C Returned
C   status word
       integer            status
C
*-
       include '/mrao/anmap/include/mapcat_cat.inc'

       integer   n, iout

       if (status.ne.0) return
       call io_enqout(iout)
       write(iout,10)
 10    format(1x/1x,'Map-Catalogues Defined: '/
     *           1x,'----------------------- '/1x)
       write(iout,'(1X,A)') default_cat_file
       do n=1,10
         if (defined_cat_file(n).eq.1) then
           write(iout,'(1X,A)') list_cat_file(n)
         end if
       end do
       write (iout,20) cat_file
 20    format(1x/1x,'Map-Catalogue Selected: ',A/1x)

       call mapcat_err(status,'mapcat_dir_list',' ')

       end
C
C
*+ mapcat_setarea

       subroutine mapcat_setarea( status )
C      -----------------------------------
C
C Set the maximum area beyond which the WHOLE map is read
C
C Returned:
C   error status
       integer   status
C-
       include '/mrao/anmap/include/mapcat_cat.inc'
       call io_getr('Maximum fractional map-area to read WHOLE map : ',
     *           '0.5',area_max_read,status)
       call mapcat_err(status,'mapcat_setarea',' ')
       end
C
C
*+ mapcat_disopts

       subroutine mapcat_disopts(prompt_state,status)
C      ----------------------------------------------
C
C Display current options settings
C
C Given:
C   current prompt state
       logical    prompt_state
C Returned:
C   status value
       integer    status
C
C-

       include '/mrao/anmap/include/mapcat_cat.inc'

C local variables
       integer   iout

C check status on entry
       if (status.ne.0) return
C find output unit
       call io_enqout(iout)

C report options
       call io_wrout(' ')
       call io_wrout('Catalogue-System Options')
       call io_wrout('------------------------')
       if (prompt_state) then
         call io_wrout('Prompting for confirmation      :   On')
       else
         call io_wrout('Prompting for confirmation      :   Off')
       end if
       if (current_report) then
         call io_wrout('Report entry of new map         :   On')
       else
         call io_wrout('Report entry of new map         :   Off')
       end if
       if (current_check) then
         call io_wrout('Checking TEMPORARY maps on exit :   On')
       else
         call io_wrout('Checking TEMPORARY maps on exit :   Off')
       end if
       write(iout,10) area_max_read
10     format(1x,   'Maximum sub-area to read        : ',F6.2)
       write(iout,20) default_map
20     format(1x,   'Default map-catalogue entry     : ',I6)
       call io_wrout(' ')

       call mapcat_err(status,'mapcat_disopts',' ')
       end
C
C
*+ mapcat_exlist

       subroutine mapcat_exlist(ilist,nlist,status)
C      --------------------------------------------
C
C Find list of catalogue entries acording to a seach criterion
C
C Returned:
C    list of catalogue entries
       integer     ilist(*)
C    length of list
       integer     nlist
C    status
       integer     status
C
C-

       include '/mrao/anmap/include/mapcat_cat.inc'

C Local variables
C   options
       integer           nopt
       parameter        (nopt = 4)
       character*80      options(nopt), option
       data options(1) /
     * 'index-list ............. search by catalogue entry number '
     *                /
       data options(2) /
     * 'source-name ............ search by full catalogue source name'
     *                /
       data options(3) /
     * 'file-name .............. search by disc file name of entry'
     *                /
       data options(4) /
     * 'program-name ........... search by program name of entry'
     *                /

C counters
       integer           i
C strings and length of strings
       integer           len_source, len_program, len_st1, len_st2,
     *                   len_file
       character*80      st1, st2

C functions
       include '/mrao/include/chrlib_functions.inc'


C check status on entry
       if (status.ne.0) return

C find search options
       call io_getopt('Search-type : ','Index-List',options,nopt,
     *             option,status)

       if (chr_cmatch(option,options(1))) then
         do i=1,256
           ilist(i) = i
         end do
         call io_getlst('Catalogue-index-list : ',' ',
     *               st1,ilist,256,nlist,status)

       else if (chr_cmatch(option,options(2))) then
         st1 = ' '
         st2 = ' '
         call io_getwrd('Source-name : ',' ',st1,len_st1,status)
         if (status.ne.0) goto 999
         nlist = 0
         do i = 1,max_cat_entries
           call mapcat_read(i,status)
           if (status.ne.0) goto 999
           if (current_map_status(ip_data).eq.true) then
             len_source  = chr_lenb(current_source)
             len_program = chr_lenb(current_program)
             write (st2,'(A,''-'',A)')
     *                          current_source(1:len_source),
     *                          current_program(1:len_program)
             len_st2 = chr_lenb(st2)
             if (chr_cmatch(st1(1:len_st1),st2(1:len_st2))) then
               nlist = nlist+1
               ilist(nlist) = i
             end if
           end if
         end do

       else if (chr_cmatch(option,options(3))) then
         st1 = ' '
         call io_getwrd('File-name : ',' ',st1,len_st1,status)
         if (status.ne.0) goto 999
         nlist = 0
         do i = 1,max_cat_entries
           call mapcat_read(i,status)
           if (status.ne.0) goto 999
           if (current_map_status(ip_data).eq.true) then
             len_file = chr_lenb(current_filename)
             if (chr_cmatch(st1(1:len_st1),
     *                  current_filename(1:len_file))) then
               nlist = nlist+1
               ilist(nlist) = i
             end if
           end if
         end do

       else if (chr_cmatch(option,options(4))) then
         st1 = ' '
         st2 = ' '
         call io_getwrd('Program-name : ',' ',st1,len_st1,status)
         if (status.ne.0) goto 999
         nlist = 0
         do i = 1,max_cat_entries
           call mapcat_read(i,status)
           if (status.ne.0) goto 999
           if (current_map_status(ip_data).eq.true) then
             st2 = current_program
             len_st2 = chr_lenb(st2)
             if (chr_cmatch(st1(1:len_st1),st2(1:len_st2))) then
               nlist = nlist+1
               ilist(nlist) = i
             end if
           end if
         end do

       end if

999    call mapcat_err(status,'mapcat_exlist',' ')
       end
C
C
*+ mapcat_get

       subroutine mapcat_get(ilist,status)
C      -----------------------------------
C
C Enquire the value of the named quantity and return as a parameter
C
C Given:
C   work space for lists
       integer          ilist(*)
C Returned:
C   error status return
       integer          status
C
C The value of the requested quantity is returned in the named command
C langauge parameter.  The options provided by this routine are concerned
C with access to information specific to the map catalogue and maps
C themselves.
C
C The following quantities may be enquired:
C   NEXT-MAP        -        next free catalogue entry
C   DEFAULT-MAP     -        current value of default map
C   MAP-DIRECTORY   -        default map directory
C   MAP-CATALOGUE   -        current catalogue file
C   MAP-LIST        -        list of map catalgue entries
C   SOURCE-NAME     -        source name for catalogue entry
C   FILE-NAME       -        file name for catalogue entry
C   PROGRAM         -        program for catalogue entry
C   UV-RANGE        -        UV-range for catalogue entry
C
*-
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/anmap/include/mapcat_cat.inc'
C
C variables used locally
       character*30   quantity, name_param
       character*80   value
       character*4    number
       integer        len_q, len_p, len_v
       integer        imap, nlist
       integer        n, i1, i2
C define the quantites to be enquired
       integer        number_quantities
       parameter     (number_quantities = 9)
       character*20   quantities(number_quantities)
       data           quantities(1)/'NEXT-MAP           '/
       data           quantities(2)/'DEFAULT-MAP        '/
       data           quantities(3)/'MAP-DIRECTORY      '/
       data           quantities(4)/'MAP-CATALOGUE      '/
       data           quantities(5)/'MAP-LIST           '/
       data           quantities(6)/'SOURCE-NAME        '/
       data           quantities(7)/'FILE-NAME          '/
       data           quantities(8)/'PROGRAM            '/
       data           quantities(9)/'UV-RANGE           '/

C check status on entry
       if (status.ne.0) return

C find quantity to enquire and parameter name for result
       call io_getwrd('Quantity : ','DEFAULT-MAP',
     *                 quantity,len_q,status)
       name_param = '%'//quantity(1:len_q)
       call io_getwrd('Parameter-name : ','*',
     *                name_param,len_p,status)
       if (status.ne.0) goto 999

C search quantity list and take action
       value = ' '
C .. NEXT-MAP
       if (chr_cmatch(quantity(1:len_q),quantities(1))) then
         call mapcat_next(imap,status)
         call chr_chitoc(imap,value,len_v)

C .. DEFAULT-MAP
       else if (chr_cmatch(quantity(1:len_q),quantities(2))) then
         call map_enqdef(imap,status)
         call chr_chitoc(imap,value,len_v)

C .. MAP-DIRECTORY
       else if (chr_cmatch(quantity(1:len_q),quantities(3))) then
         call enmdir(' ',value,status)

C .. MAP-CATALOGUE
       else if (chr_cmatch(quantity(1:len_q),quantities(4))) then
         value = cat_file

C .. MAP-LIST
       else if (chr_cmatch(quantity(1:len_q),quantities(5))) then
         call mapcat_exlist(ilist,nlist,status)
         value = ' '
         do n=1,nlist
           call chr_chitoc(ilist(n),number,i2)
           i1 = chr_intlc(number)
           len_v = chr_lenb(value)
           if (n.eq.1) then
             value = number(i1:i2)
           else
             value = value(1:len_v)//','//number(i1:i2)
           end if
         end do

C .. SOURCE-NAME
       else if (chr_cmatch(quantity(1:len_q),quantities(6))) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0) then
           call mapcat_open(status)
           call mapcat_read(imap,status)
           if (status.eq.0) then
             value = current_source
           end if
         end if

C .. FILE-NAME
       else if (chr_cmatch(quantity(1:len_q),quantities(7))) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0) then
           call mapcat_open(status)
           call mapcat_read(imap,status)
           if (status.eq.0) then
             value = current_filename
           end if
         end if

C .. PROGRAM
       else if (chr_cmatch(quantity(1:len_q),quantities(8))) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0) then
           call mapcat_open(status)
           call mapcat_read(imap,status)
           if (status.eq.0) then
             value = current_program
           end if
         end if

C .. UV-RANGE
       else if (chr_cmatch(quantity(1:len_q),quantities(9))) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0) then
           call mapcat_open(status)
           call mapcat_read(imap,status)
           if (status.eq.0) then
             value = ' '
             do n=1,4
               call chr_chitoc(current_minirt(n),number,i2)
               i1 = chr_intlc(number)
               len_v = chr_lenb(value)
               if (n.eq.1) then
                 value = number(i1:i2)
               else
                 value = value(1:len_v)//','//number(i1:i2)
               end if
             end do
           end if
         end if


C .. unknown quantity
       else
         call io_wrout('*** Unknown quantity in CATALOGUE GET')

       end if

C define parameter
       len_v = chr_lenb(value)
       call cmd_setparam(name_param(1:len_p),value(1:len_v),status)

C errors action
999    call cmd_err(status,'CATALOGUE: GET','in map catalogue system')

       end
C
C
*+ mapcat_init

       subroutine mapcat_init(status)
C      ------------------------------
C
C Initialise the map catalogue
C
C Returned:
C    Status
       integer     status
*-
       integer   imap
       include '/mrao/anmap/include/mapcat_cat.inc'

       if (status.ne.0) return
       call mapcat_open(status)
       do imap = 1, max_cat_entries
         current_filename = ' '
         current_source   = ' '
         current_program  = ' '
         current_map      = imap
         current_map_status(ip_data)   = false
         current_map_status(ip_open)   = false
         current_map_status(ip_unit)   = 0
         current_map_status(ip_access) = access_clear
         call mapcat_write(imap,status)
       end do
       call mapcat_err(status,'mapcat_init',' ')
       end

