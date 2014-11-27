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
C   HEADER          -        Header information from specified map
*-
       include '/mrao/include/chrlib_functions.inc'
       include 'mapcat_cat.inc'
C
C variables used locally
       character*60   opts(10), opt
       character*80   value
       character*20   number
       integer        imap, nlist, ln, lv
       integer        n, i1, i2

       opts(1) = 'next-map ........... next free map'
       opts(2) = 'default-map ........ default map catalogue entry'
       opts(3) = 'map-directory ...... default map/images directory'
       opts(4) = 'map-catalogue ...... name of map catalogue'
       opts(5) = 'map-list ........... list of maps'
       opts(6) = 'source-name ........ source name for catalogue entry'
       opts(7) = 'file-name .......... file name for catalogue entry'
       opts(8) = 'program ............ file name for catalogue entry'
       opts(9) = 'uv-range ........... UV-range of specified map'
       opts(10)= 'header ............. header info for specified map'

C check status on entry
       if (status.ne.0) return

C find quantity to enquire and parameter name for result
       call io_getopt( 'Get-option (?=list) : ','default-map',
     *                  opts, 10, opt, status )

       if (status.ne.0) goto 999

C search quantity list and take action
       value = ' '
       if (chr_cmatch(opt,'next-map')) then
         call mapcat_next(imap,status)
         call chr_chitoc(imap,number,ln)
         call cmd_setlocal('%next-map',number(1:ln),status)

       else if (chr_cmatch(opt,'default-map')) then
         call map_enqdef(imap,status)
         call chr_chitoc(imap,number,ln)
         call cmd_setlocal('%default-map',number(1:ln),status)

       else if (chr_cmatch(opt,'map-directory')) then
         call cmd_setlocal('%default-directory',current_def_dir,status)

       else if (chr_cmatch(opt,'map-catalogue')) then
         call cmd_setlocal('%map-catalogue',cat_file,status)

       else if (chr_cmatch(opt,'map-list')) then
         call mapcat_exlist(ilist,nlist,status)
         value = ' '
         do n=1,nlist
           call chr_chitoc(ilist(n),number,i2)
           i1 = chr_intlc(number)
           lv = chr_lenb(value)
           if (n.eq.1) then
             value = number(i1:i2)
           else
             value = value(1:lv)//','//number(i1:i2)
           end if
         end do
         call cmd_setlocal('%map-list',value(1:lv),status)

       else if (chr_cmatch(opt,'source-name')) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0) then
           call mapcat_open(status)
           call mapcat_read(imap,status)
           if (status.eq.0) then
             call cmd_setlocal('%source-name',current_source,status)
           end if
         end if

       else if (chr_cmatch(opt,'file-name')) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0) then
           call mapcat_open(status)
           call mapcat_read(imap,status)
           if (status.eq.0) then
             call cmd_setlocal('%file-name',current_filename,status)
           end if
         end if

       else if (chr_cmatch(opt,'program')) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0) then
           call mapcat_open(status)
           call mapcat_read(imap,status)
           if (status.eq.0) then
             call cmd_setlocal('%file-name',current_program,status)
           end if
         end if

       else if (chr_cmatch(opt,'uv-range')) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0) then
           call mapcat_open(status)
           call mapcat_read(imap,status)
           if (status.eq.0) then
             value = ' '
             do n=1,4
               call chr_chitoc(current_minirt(n),number,i2)
               i1 = chr_intlc(number)
               lv = chr_lenb(value)
               if (n.eq.1) then
                 value = number(i1:i2)
               else
                 value = value(1:lv)//','//number(i1:i2)
               end if
             end do
           end if
         end if
         lv = chr_lenb(value)
         call cmd_setlocal('%uv-range',value(1:lv),status)

       else if (chr_cmatch(opt,'header')) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0 .and. status.eq.0) then
           call io_getwrd('Header-item : ','frequency',opt,lv,status)
           call mc_enqhdc( imap, opt(1:lv), value, status )
           call cmd_setlocal( '%'//opt(1:lv), value, status )
         endif
       end if

C errors action
999    call cmd_err(status,'CATALOGUE: GET','in map catalogue system')

       end

