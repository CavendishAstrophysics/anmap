*$ Sub-system to perform Map-catalogue maintenance
*  -----------------------------------------------
C
C Last updated Paul Alexander 2/3/92
C SUNOS Version 1.0
C
*+ mapcat_sys

       subroutine mapcat_sys(interp,map_array,results,status)
C      ------------------------------------------------------
C
C Provide monitor and display facilities to the catalogue and stack
C
C Input:
C    Interpreter pointer
       integer                interp
C    Map space
       real*4                 map_array(*)
C    Results string
       character*(*)          results
C Returned:
C    Status
       integer                status
C
C Provide a number of monitor and command facilities for the catalogue
C and stack system.
C
*-
       include '/mrao/anmap_v7.5/include/anmap_sys_pars.inc'
       include 'mapcat_pars.inc'
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
       integer          ic_def_dir
       parameter       (ic_def_dir    = ic_def_map    + 1)
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
       integer          ic_enq
       parameter       (ic_enq        = ic_get     + 1)
       integer          number_commands
       parameter       (number_commands = ic_enq)
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
       data             liscom(ic_enq)       /
     *  'enq ....................... enquire map-catalogue data'       /

C local variables
       integer        n, imap, ilist(256), iout, i, map_status(10)
       integer        minirt(10), icomm, nlist, list_1, list_2, count
       character      command*40, source*40, program*20,
     *                old_filename*(iolen_file),
     *                new_filename*(iolen_file),
     *                default_directory*(iolen_dir),
     *                string*80
       integer        len_com, len_st, len_dir, length, istat
       logical        prompt_state,
     *                report_mode, check_mode

C functions
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C check status on entry
       if (status.ne.0) return

C Read in command:

       call cmd_getcmd('Map-Catalogue> ',liscom,
     *                  number_commands,icomm,status)
       command = liscom(icomm)
       len_com = chr_lenw(command)

C Decode command:
       if (chr_cmatch(command(1:len_com),
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
         call io_makfil(' ',string,'map',new_filename,istat)
         string = ' '
         istat = 0
         call io_namfil(old_filename,string,0,istat)
         old_filename = string
         string = ' '
         istat = 0
         call io_namfil(new_filename,string,0,istat)
         new_filename = string
         call io_rnmfil(old_filename,new_filename,0,status)
         call mapcat_setrt(imap,new_filename,minirt,status)
         map_status(ip_page) = false
         call mapcat_setst(imap,map_status,status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_del_map)))then
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

       elseif (chr_cmatch(command(1:len_com),
     *         liscom(ic_init_cat)))then
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
     *                       liscom(ic_ver_cat)))then
           call mapcat_verify(status)

       else if (chr_cmatch(command(1:len_com),
     *                       liscom(ic_file_man)))then
           call mapcat_file_manager(status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_get)))then
           call mapcat_get(ilist,status)

       else if (chr_cmatch(command(1:len_com),liscom(ic_enq)))then
           call mapcat_enq(results,status)

       else
         call io_wrout('*** unknown Catalogue-System command')

       end if

C Take action on completion of command:
999    call mapcat_err(status,'Map-Catalogue-System',' ')

       end
C
C
       subroutine mapcat_enq(results,status)
C      -------------------------------------
C
C Enquire data for the map-catalogue system
C
C Updated:
C   results string
       character*(*)     results
C   error status
       integer           status
C
C-

       include '/mrao/include/chrlib_functions.inc'
       include 'mapcat_cat.inc'
C
C variables used locally
       character*60   opts(10), opt
       character*80   value
       character*20   number
       integer        imap, nlist, ln, lv
       integer        n, i1, i2, ilist(256)

       opts(1) = 'next-map ........... next free map'
       opts(2) = 'default-map ........ default map catalogue entry'
       opts(3) = 'map-directory ...... default map/images directory'
       opts(4) = 'map-catalogue ...... name of map catalogue'
       opts(5) = 'map-list ........... list of maps'
       opts(6) = 'source-name ........ source name for catalogue entry'
       opts(7) = 'file-name .......... file name for catalogue entry'
       opts(8) = 'program ............ file name for catalogue entry'
       opts(9) = 'uv-range ........... UV-range of specified map'

C check status on entry
       if (status.ne.0) return

C find quantity to enquire and parameter name for result
       call io_getopt( 'Get-option (?=list) : ','default-map',
     *                  opts, 9, opt, status )

       if (status.ne.0) goto 999

C search quantity list and take action
       value = ' '
       results = ' '
       if (chr_cmatch(opt,'next-map')) then
         call mapcat_next(imap,status)
         call chr_chitoc(imap,results,ln)

       else if (chr_cmatch(opt,'default-map')) then
         call map_enqdef(imap,status)
         call chr_chitoc(imap,results,ln)

       else if (chr_cmatch(opt,'map-directory')) then
         results = current_def_dir

       else if (chr_cmatch(opt,'map-catalogue')) then
         results = cat_file

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
         results = value(1:lv)

       else if (chr_cmatch(opt,'source-name')) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0) then
           call mapcat_open(status)
           call mapcat_read(imap,status)
           if (status.eq.0) then
             results = current_source
           end if
         end if

       else if (chr_cmatch(opt,'file-name')) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0) then
           call mapcat_open(status)
           call mapcat_read(imap,status)
           if (status.eq.0) then
             results = current_filename
           end if
         end if

       else if (chr_cmatch(opt,'program')) then
         call io_geti('Map-entry : ','0',imap,status)
         if (imap.gt.0) then
           call mapcat_open(status)
           call mapcat_read(imap,status)
           if (status.eq.0) then
             results = current_program
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
         results = value(1:lv)

       end if

C errors action
999    call cmd_err(status,'CATALOGUE: ENQ','in map catalogue system')


       end
