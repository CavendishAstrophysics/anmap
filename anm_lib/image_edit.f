*+ image_edit

       subroutine image_edit( interp, cdata, map_array, status )
C      ---------------------------------------------------------
C
C Sub-system to edit an image
C
C Updated:
C    command interpreter data structure
       integer       interp(*)
C    command language data structure
       integer       cdata(*)
C    map data array
       real*4        map_array(*)
C Returned:
C    error status
       integer       status
C
C A command line driven sub-system to edit an image.  Facilities are
C provided to set regions of an image to a defined value, copy
C parts of an image and reset values in an image.
C
C P. Alexander, MRAO, Cambridge
C Version 2.0. December 1993
C-

       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C Local variables
C ---------------
C  command line interpretation
       integer            number_commands, icom
       parameter         (number_commands = 30)
       character*80       liscom(number_commands), 
     *                    option_list(12), option, sm_prompt
C  catalogue entries
       integer            imap, imapo, iedit, ip_map, ip_edit
       character          string*40
C  size and range on the mask
       integer            nx, ny, u1, u2, v1, v2
       integer            minirt(8), uvr(4), to_uvr(4), from_uvr(4)
       equivalence       (minirt(1), uvr(1))
       equivalence       (minirt(1), u1)
       equivalence       (minirt(2), u2)
       equivalence       (minirt(3), v1)
       equivalence       (minirt(4), v2)
       equivalence       (minirt(5), nx)
       equivalence       (minirt(6), ny)
C  map functions
       integer            iuvmap2
C  redtape information
       integer            map_minirt(8)
C  counters
       integer            i, j, iu, iv
       integer            uv_pix(2), to_pix(2), from_pix(2)
       equivalence       (uv_pix(1), iu)
       equivalence       (uv_pix(2), iv)
C  variables used in defining mask
       real*4             uv_pos(2), to_pos(2), rad1, rad2,
     *                    SD, mean, blank, fixed, blank_new,
     *                    scale, zero, x, y, gate_1, gate_2
C  type of replacement
      integer             replacement_mode, add_mode, edit_mode
C  function definition
      character*256       function
      integer             lf
C  dump array for redtape
      integer             rt(1024)

C  save all control information for editing
       common /save_image_edit/ imap, iedit, ip_edit, replacement_mode,
     *                          add_mode, blank, fixed, SD, mean,
     *                          scale, zero, map_minirt, edit_mode,
     *                          gate_1, gate_2
       common /char_image_edit/ sm_prompt

C  functions
       real*4             imed_value, image_polcorr

C define commands
       data liscom(1) /
     *  'set-map .................. set map to edit'
     *                /
       data liscom(2) /
     *  'create-map ............... create a new output map'
     *                /
       data liscom(3)/
     *  'update-map ............... open a map for update'
     *                /
       data liscom(4) /
     *  'save-edits ............... save the current edits'
     *                /
       data liscom(5) /
     *  'discard-edits ............ end editing without saving'
     *                /
       data liscom(6) /
     *  'set-pixel ................ set pixel value'
     *                /
       data liscom(7) /
     *  'set-region ............... set values in a UV-region'
     *                /
       data liscom(8) /
     *  'set-circle ............... set values in a circular region'
     *                /
       data liscom(9) /
     *  'set-annulus .............. set values in an annulus'
     *                /
       data liscom(10)/
     *  'set-row .................. set values in a row'
     *                /
       data liscom(11)/
     *  'set-column ............... set values in a column'
     *                /
       data liscom(12)/
     *  'set-full-image ........... set values in complete image'
     *                /
       data liscom(13)/
     *  'set-source ............... set source name in output'
     *                /
       data liscom(14)/
     *  'set-program .............. set program name in output'
     *                /
       data liscom(15)/
     *  'copy-pixel ............... copy pixel value on image'
     *                /
       data liscom(16)/
     *  'copy-region .............. copy UV-region on image'
     *                /
       data liscom(17)/
     *  'copy-row ................. copy row on image'
     *                /
       data liscom(18)/
     *  'copy-column .............. copy column on image'
     *                /
       data liscom(19)/
     *  'copy-from-image .......... copy region from another image'
     *                /
       data liscom(20)/
     *  'select-value-mode ........ specify the replacement value'
     *                /
       data liscom(21)/
     *  'select-noise-mode ........ specify replacement to be noise'
     *                /
       data liscom(22)/
     *  'select-scale-mode ........ specify replacement to be scaling'
     *                /
       data liscom(23)/
     *  'select-blanking-mode ..... specify replacement is blanking'
     *                /
       data liscom(24)/
     *  'select-filter-mode ....... specify mode to be filter'
     *                /
       data liscom(25)/
     *  'define-blank ............. define value for "blank"'
     *                /
       data liscom(26)/
     *  'reset-blank-values ....... reset values of blank pixels'
     *                /
       data liscom(27)/
     *  'apply-ricean-correction .. apply correction for ricean errors'
     *                /
       data liscom(28)/
     *  'mask-map ................. use edits as a mask'
     *                /
       data liscom(29)/
     *  'function ................. define a function in a region'
     *                /
       data liscom(30)/
     *  'edit-options ............. specify options to edit-image'
     *                /

C check status on entry
       if (status.ne.0) return
       call ldredt( rt, status )
       rad1 = 0.0
       rad2 = 10000.0
       uv_pos(1) = 0.0
       uv_pos(2) = 0.0
       call cmd_getcmd( 'Edit-image> ',liscom,number_commands,
     *                  icom,status)

       if (status.ne.0) then
         call cmd_err(status,'Edit-Image',' ')
         cdata(1) = 100
         return
       end if

C decode command
       if (chr_cmatch('set-map',liscom(icom))) then
         if (chr_lenb(sm_prompt).gt.0) then
           call mapcat_getmap(sm_prompt(1:chr_lenb(sm_prompt)+1),
     *                        'default_map',
     *                        'READ',imap,status)
         else
           call mapcat_getmap('Catalogue-entry : ','default_map',
     *                        'READ',imap,status)
         endif

C .. save any existing edits and allocate a new output map
         if (iedit.ne.-1) then
           call map_end_alloc( iedit, map_array, status )
         end if
         call map_alloc_toout(imap,'DIRECT',map_array,
     *                        iedit,ip_edit,status)
         call redt_load(imap,status)
         call enminirt( map_minirt, status )
         call ennull( blank, status )

       elseif (chr_cmatch('update-map',liscom(icom))) then
         call mapcat_getmap('Map-entry : ','default_map',
     *                      'READ',imap,status)
C .. save any existing edits and allocate a new output map
         if (iedit.ne.-1) then
           call map_end_alloc( iedit, map_array, status )
         end if
         call map_alloc_in(imap,'DIRECT',map_array,ip_edit,status)
         iedit = imap
         call redt_load(imap,status)
         call enminirt( map_minirt, status )
         call ennull( blank, status )
         call mapcat_acc(iedit,'CLEAR',status)
         call mapcat_acc(iedit,'WRITE',status)

       else if (chr_cmatch('create-map',liscom(icom))) then
         if (iedit.ne.-1) then
           call map_end_alloc( iedit, map_array, status )
         end if         
         call io_getni('UV-range for image : ','*',map_minirt,4,status)
         if (map_minirt(1).gt.map_minirt(2)) then
           i = map_minirt(1)
           map_minirt(1) = map_minirt(2)
           map_minirt(2) = i
         endif
         if (map_minirt(4).gt.map_minirt(3)) then
           i = map_minirt(4)
           map_minirt(4) = map_minirt(3)
           map_minirt(3) = i
         endif
         map_minirt(5) = map_minirt(2) - map_minirt(1) + 1
         map_minirt(6) = map_minirt(3) - map_minirt(4) + 1
         call nwredt(map_minirt(5), map_minirt(6), 3, 0, status )
         call adredt( 'CREATED', 'EDITIMAGE', status )
         call io_getstr( 'Source-name : ',' ',string,status )
         call adredt( 'SOURCE', string, status )
         call stmapc( 1.0D+0,1.0D+0,1950.0D+0,1950.0D+0,string,status )
         call stmapj( 2,1.0D+0,0.0D+0,1950.0D+0,0.0D+0,status )
         call sttype( 0.0,-1,'NONE','NONE',status )
         if (map_minirt(5).gt.0 .and. map_minirt(6).gt.0) then
           call map_alloc_out(map_minirt(5),map_minirt(6),'DIRECT',
     *                        iedit,ip_edit,status)
           call enminirt(map_minirt,status)
           call mapcat_setsr(iedit,string,'EDITIMAGE',status)
         endif
         call ennull( blank, status )

       else if (chr_cmatch('save-edits',liscom(icom))) then
         if (iedit.gt.0) then
           call stnull( blank, status )
           call map_end_alloc( iedit, map_array, status )
           iedit = -1
         endif

       else if (chr_cmatch('discard-edits',liscom(icom))) then
         i = -10
         call map_end_alloc( iedit, map_array, i )
         iedit = -1

       else if (chr_cmatch('set-pixel',liscom(icom))) then
         call plot_getpos2(map_minirt,'UV-position : ','*',
     *                    uv_pos,status)
         uv_pix(1) = nint(uv_pos(1))
         uv_pix(2) = nint(uv_pos(2))
         if (status.eq.0) then
            i = iuvmap2(map_minirt,uv_pix)
            map_array(ip_edit+i-1)=imed_value(map_array(ip_edit+i-1))
         end if
         call cmd_err( status,'Set-Pixel','Failed' )

       else if (chr_cmatch('set-region',liscom(icom))) then
         do i=1,8
           minirt(i) = map_minirt(i)
         enddo
         call plot_getuv2(map_minirt,'UV-region : ','*',uvr,status)
         if (status.eq.0) then
           do iv=uvr(3),uvr(4),-1
             do iu=uvr(1),uvr(2)
               i = iuvmap2(map_minirt,uv_pix)
               map_array(ip_edit+i-1)=imed_value(map_array(ip_edit+i-1))
             end do
           end do
         end if
         call cmd_err( status,'Set-Region','Failed' )

       else if (chr_cmatch('set-circle',liscom(icom))) then
         uv_pos(1) = 0.0
         uv_pos(2) = 0.0
         call plot_getpos2(map_minirt,'UV-centre : ','*',uv_pos,status)
         call io_getr('Radius (pixels) : ','*',rad1,status)
         if (status.eq.0) then
           do iv=map_minirt(3),map_minirt(4),-1
             do iu=map_minirt(1),map_minirt(2)
               i = iuvmap2(map_minirt,uv_pix)
               if ( ((uv_pix(1)-uv_pos(1))**2+(uv_pix(2)-uv_pos(2))**2)
     *              .le.(rad1*rad1)) then
                 map_array(ip_edit+i-1)=
     *             imed_value(map_array(ip_edit+i-1))
               end if
             end do
           end do
         end if
         call cmd_err( status,'Set-Circle','Failed' )

       else if (chr_cmatch('set-annulus',liscom(icom))) then
         uv_pos(1) = 0.0
         uv_pos(2) = 0.0
         call plot_getpos2(map_minirt,'UV-centre : ','*',uv_pos,status)
         call io_getr('Inner-Radius (pixels) : ','*',rad1,status)
         call io_getr('Outer-Radius (pixels) : ','*',rad2,status)
         if (status.eq.0) then
           do iv=map_minirt(3),map_minirt(4),-1
             do iu=map_minirt(1),map_minirt(2)
               i = iuvmap2(map_minirt,uv_pix)
               if (
     *            ( ((uv_pix(1)-uv_pos(1))**2+(uv_pix(2)-uv_pos(2))**2)
     *               .ge.(rad1*rad1) ) .and.
     *            ( ((uv_pix(1)-uv_pos(1))**2+(uv_pix(2)-uv_pos(2))**2)
     *               .le.(rad2*rad2) ) ) then
                 map_array(ip_edit+i-1) =
     *              imed_value(map_array(ip_edit+i-1))
               end if
             end do
           end do
         end if
         call cmd_err( status,'Set-Annulus','Failed' )

       else if (chr_cmatch('set-row',liscom(icom))) then
         uv_pos(1) = 0.0
         uv_pos(2) = 0.0
         call plot_getrow2(map_minirt,'UV-row : ','*',
     *                    uv_pos(2),status)
         uv_pix(2) = nint(uv_pos(2))
         if (status.eq.0) then
           do iu=map_minirt(1),map_minirt(2)
               i = iuvmap2(map_minirt,uv_pix)
               map_array(ip_edit+i-1) =
     *            imed_value(map_array(ip_edit+i-1))
           end do
         end if
         call cmd_err( status,'Set-Row','Failed' )

       else if (chr_cmatch('set-column',liscom(icom))) then
         uv_pos(1) = 0.0
         uv_pos(2) = 0.0
         call plot_getcol2(map_minirt,'UV-column : ','*',
     *                    uv_pos(1),status)
         uv_pix(1) = nint(uv_pos(1))
         if (status.eq.0) then
           do iv=map_minirt(3),map_minirt(4),-1
               i = iuvmap2(map_minirt,uv_pix)
               map_array(ip_edit+i-1) =
     *            imed_value(map_array(ip_edit+i-1))
           end do
         end if
         call cmd_err( status,'Set-Column','Failed' )

       else if (chr_cmatch('set-full-image',liscom(icom))) then
         do i=1,map_minirt(5)*map_minirt(6)
            map_array(ip_edit+i-1)=imed_value(map_array(ip_edit+i-1))
         end do
         call cmd_err( status,'Set-Full-Image','Failed' )

       else if (chr_cmatch('set-source',liscom(icom))) then
         string = ' '
         call io_getstr('Source : ',' ',string,status)
         call adredt('SOURCE',string,status)
         call cmd_err( status,'Set-Source','Failed' )

       else if (chr_cmatch('set-program',liscom(icom))) then
         string = ' '
         call io_getstr('Program : ',' ',string,status)
         call adredt('CREATED',string,status)
         call cmd_err( status,'Set-Program','Failed' )

       else if (chr_cmatch('copy-pixel',liscom(icom))) then
         call plot_getpos2(map_minirt,'From-UV-position : ','*',
     *                    uv_pos,status)
         call plot_getpos2(map_minirt,'To-UV-position : ','*',
     *                    to_pos,status)
         uv_pix(1) = nint(uv_pos(1))
         uv_pix(2) = nint(uv_pos(2))
         to_pix(1) = nint(to_pos(1))
         to_pix(2) = nint(to_pos(2))
         if (status.eq.0) then
            i = iuvmap2(map_minirt,uv_pix)
            j = iuvmap2(map_minirt,to_pix)
            map_array(ip_edit+i-1) =
     *            map_array(ip_edit+j-1)
         end if
         call cmd_err( status,'Copy-Pixel','Failed' )

       else if (chr_cmatch('copy-region',liscom(icom))) then
         do i=1,8
           minirt(i) = map_minirt(i)
         enddo
         call plot_getuv2(map_minirt,'From-UV-region : ','*',uvr,status)
         call plot_getpos2(map_minirt,'To-BLC : ','*',to_pos,status)
         to_pix(1) = nint(to_pos(1))
         to_pix(2) = nint(to_pos(2))
         to_uvr(1) = to_pix(1)
         to_uvr(2) = to_pix(1) + uvr(2) - uvr(1)
         to_uvr(4) = to_pix(2)
         to_uvr(3) = to_pix(2) + uvr(3) - uvr(4)
         if (status.eq.0) then
           do iv=to_uvr(4),to_uvr(3)
             do iu=to_uvr(1),to_uvr(2)
               i = iuvmap2(map_minirt,uv_pix)
               from_pix(1) = iu - to_uvr(1) + uvr(1)
               from_pix(2) = iv - to_uvr(4) + uvr(4)
               j = iuvmap2(map_minirt,from_pix)
               map_array(ip_edit+i-1) = map_array(ip_edit+j-1)
             end do
           end do
         end if
         call cmd_err( status,'Copy-Region','Failed' )

       else if (chr_cmatch('copy-row',liscom(icom))) then
         do i=1,8
           minirt(i) = map_minirt(i)
         enddo
         call plot_getrow2(map_minirt,'From-row : ','*',
     *                    uv_pos(2),status)
         call plot_getrow2(map_minirt,'To-row : ','*',
     *                    to_pos(2),status)
         uv_pix(1) = nint(uv_pos(1))
         uv_pix(2) = nint(uv_pos(2))
         to_pix(1) = nint(to_pos(1))
         to_pix(2) = nint(to_pos(2))
         if (status.eq.0) then
           do iu=uvr(1),uvr(2)
              to_pix(1) = iu
              i = iuvmap2(map_minirt,to_pix)
              j = iuvmap2(map_minirt,uv_pix)
              map_array(ip_edit+i-1) = map_array(ip_edit+j-1)
           end do
         end if
         call cmd_err( status,'Copy-Row','Failed' )

       else if (chr_cmatch('copy-column',liscom(icom))) then
         do i=1,8
           minirt(i) = map_minirt(i)
         enddo
         call plot_getcol2(map_minirt,'From-column : ','*',
     *                    uv_pos(1),status)
         call plot_getcol2(map_minirt,'To-column : ','*',
     *                    to_pos(1),status)
         uv_pix(1) = nint(uv_pos(1))
         uv_pix(2) = nint(uv_pos(2))
         to_pix(1) = nint(to_pos(1))
         to_pix(2) = nint(to_pos(2))
         if (status.eq.0) then
           do iv=uvr(3),uvr(4),-1
              to_pix(2) = iv
              i = iuvmap2(map_minirt,to_pix)
              j = iuvmap2(map_minirt,uv_pix)
              map_array(ip_edit+i-1) = map_array(ip_edit+j-1)
           end do
         end if
         call cmd_err( status,'Copy-Column','Failed' )

       else if (chr_cmatch('copy-from-image',liscom(icom))) then
         call dpredt( rt, status )
         call mapcat_getmap('Map-entry : ','default_map',
     *                      'READ',i,status)
         call map_alloc_in(i,'DIRECT',map_array,ip_map,status)
         call enminirt( minirt, status )
         call map_end_alloc(i,map_array,status)
         do i=1,4
           from_uvr(i) = minirt(i)
         enddo
         call plot_getuv2(minirt,'From-UV-region : ','*',
     *                    from_uvr,status)
         call plot_getpos2(map_minirt,'To-BLC : ','*',to_pos,status)
         to_pix(1) = nint(to_pos(1))
         to_pix(2) = nint(to_pos(2))
         to_uvr(1) = to_pix(1)
         to_uvr(2) = to_pix(1) + from_uvr(2) - from_uvr(1)
         to_uvr(4) = to_pix(2)
         to_uvr(3) = to_pix(2) + from_uvr(3) - from_uvr(4)
         if (status.eq.0) then
           do iv=to_uvr(3),to_uvr(4),-1
             do iu=to_uvr(1),to_uvr(2)
               i = iuvmap2(map_minirt,uv_pix)
               from_pix(1) = iu - to_uvr(1) + from_uvr(1)
               from_pix(2) = iv - to_uvr(4) + from_uvr(4)
               j = iuvmap2(minirt,from_pix)
               map_array(ip_edit+i-1) = map_array(ip_map+j-1)
             end do
           end do
         end if
         call ldredt( rt, status )
         call cmd_err( status,'Copy-From-Image','Failed' )

       else if (chr_cmatch('select-value-mode',liscom(icom))) then
         call io_getr('Replacement-value : ','*',fixed,status)
         if (status.eq.0) then
           replacement_mode = 2
           add_mode = 0
         endif
         call cmd_err( status,'Select-Value-Mode','Failed' )

       else if (chr_cmatch('select-scale-mode',liscom(icom))) then
         call io_getr('Scale : ','*',scale,status)
         call io_getr('Zero-level : ','*',zero,status)
         if (status.eq.0) then
           replacement_mode = 3
           add_mode = 0
         endif
         call cmd_err( status,'Select-Scale-Mode','Failed' )

       else if (chr_cmatch('select-noise-mode',liscom(icom))) then
         call io_getr('Noise-mean : ','*',mean,status)
         call io_getr('Noise-SD : ','*',SD,status)
         if (status.eq.0) then
           replacement_mode = 1
           add_mode = 1
         endif
         call util_rninit( 0 )
         call cmd_err( status,'Select-Noise-Mode','Failed' )

       else if (chr_cmatch('select-blanking-mode',liscom(icom))) then
         if (status.eq.0) then
           replacement_mode = 0
           add_mode = 0
         endif
         call cmd_err( status,'Select-Blanking-Mode','Failed' )

       else if (chr_cmatch('select-filter-mode',liscom(icom))) then
         option_list(1) ='linear . linear filter'
         option_list(2) ='log .... natural logarithm filter'
         option_list(3) ='ln ..... natural logarithm filter'
         option_list(4) ='log10 .. base10 logarithm filter'
         option_list(5) ='lg ..... base10 logarithm filter'
         option_list(6) ='exp .... exponential filter'
         option_list(7) ='10 ..... power-of-ten filter'
         option_list(8) ='sqrt ... square root filter'
         option_list(9) ='1/x .... reciprocal'
         option_list(10)='x^2 .... square filter'
         option_list(11)='binary . binary filter'
         option_list(12)='gate ... gating filter'
         call io_getopt('Filter-option (?=help) : ','linear',
     *                   option_list,11,option,status)
         if (chr_cmatch('linear',option)) then
            replacement_mode = 4
            add_mode = 0
            call io_getr('Scale-factor : ','1.0',scale,status)
            call io_getr('Zero-level : ','0.0',zero,status)
         elseif (chr_cmatch('log10',option) .or.
     *           chr_cmatch('lg',option)) then
            replacement_mode = 4
            add_mode = 2
         elseif (chr_cmatch('log',option) .or.
     *           chr_cmatch('ln',option)) then
            replacement_mode = 4
            add_mode = 1
         elseif (chr_cmatch('exp',option)) then
            replacement_mode = 4
            add_mode = 3
         elseif (chr_cmatch('10',option)) then
            replacement_mode = 4
            add_mode = 4
         elseif (chr_cmatch('sqrt',option)) then
            replacement_mode = 4
            add_mode = 5
         elseif (chr_cmatch('x^2',option)) then
            replacement_mode = 4
            add_mode = 6
         elseif (chr_cmatch('1/x',option)) then
            replacement_mode = 4
            add_mode = 7
         elseif (chr_cmatch('binary',option)) then
            replacement_mode = 4
            add_mode = 20
            call io_getr('Gate-low : ','0.0',gate_1,status)
            call io_getr('Gate-high : ','0.0',gate_2,status)
         elseif (chr_cmatch('gate',option)) then
            replacement_mode = 4
            add_mode = 21
            call io_getr('Gate-low : ','0.0',gate_1,status)
            call io_getr('Gate-high : ','0.0',gate_2,status)
            fixed = blank
            call io_getr('Replacement [blank] : ',' ',fixed,status)
         endif
         call cmd_err( status,'Select-Filter-Mode','Failed' )

       else if (chr_cmatch('define-blank',liscom(icom))) then
         call io_getr('Blank-value : ','*',blank_new,status)
         do i=1,map_minirt(5)*map_minirt(6)
           if (map_array(ip_edit+i-1).eq.blank) then
             map_array(ip_edit+i-1) = blank_new
           endif
         enddo
         blank = blank_new
         call cmd_err( status,'Define-Blank','Failed' )

       else if (chr_cmatch('reset-blank-values',liscom(icom))) then
         call io_getr('replacement-value : ','*',blank_new,status)
         do i=1,map_minirt(5)*map_minirt(6)
           if (map_array(ip_edit+i-1).eq.blank) then
             map_array(ip_edit+i-1) = blank_new
           endif
         enddo
         call cmd_err( status,'Reset-Blank-Values','Failed' )

       elseif (chr_cmatch('apply-ricean-correction',liscom(icom))) then
         call io_getr('Noise-level : ','0.0',y,status)
         if (status.eq.0 .and. y.gt.0.0) then
           do i=1,map_minirt(5)*map_minirt(6)
             if ( map_array(ip_edit+i-1).ne.blank .and. 
     *            map_array(ip_edit+i-1).gt.0.0 .and.
     *            status.eq.0 ) then
                x = map_array(ip_edit+i-1)/y
                map_array(ip_edit+i-1)=y*image_polcorr(x,status)
             endif
           end do
         endif
         call cmd_err( status,'Apply-Ricean-Correction','Failed' )

       else if (chr_cmatch('mask-map',liscom(icom))) then
         call mapcat_getmap('Map-to-mask : ','default_map',
     *                    'READ',i,status)
         call map_alloc_toout(i,'DIRECT',map_array,
     *                        imapo,ip_map,status)
         call enminirt(minirt,status)
         do iv=min(map_minirt(4),minirt(4)),
     *         max(map_minirt(3),minirt(3)), -1
           do iu=max(map_minirt(1),minirt(1)),
     *           min(map_minirt(2),minirt(2))
             i = iuvmap2(minirt,uv_pix)
             j = iuvmap2(map_minirt,uv_pix)
             map_array(ip_map+i-1) = 
     *         map_array(ip_map+i-1) * map_array(ip_edit+j-1)
           enddo
         enddo
         call map_end_alloc(imapo,map_array,status)
         call cmd_err( status,'Mask-Map','Failed' )

       else if (chr_cmatch('function',liscom(icom))) then
         call io_getwrd('Function (f(x,y)) : ','sin(x)*sin(y)',
     *                  function,lf,status)
         function = function(1:lf)//char(0)
         do i=1,4
           uvr(i) = map_minirt(i)
         enddo
         call plot_getuv2(map_minirt,'UV-region : ','*',uvr,status)
         i = 0
         call io_geti('Mode (0=replace; 1=add; 2=multiply) : ','0',
     *                i,status)
         if (i.lt.0) then
           i = 0
         elseif (i.gt.2) then
           i = 2
         endif
         if (status.eq.0) then
           call eval_array(function,i,map_minirt(1),map_minirt(3),
     *                     uvr(1),uvr(2),uvr(3),uvr(4),
     *                     map_minirt(5),map_minirt(6),
     *                     map_array(ip_edit))
         endif

       else if (chr_cmatch('edit-options',liscom(icom))) then
         option_list(1)='auto-discard-on .. auto discard on error on'
         option_list(2)='auto-discard-off . auto discard on error off'
         option_list(3)='set-map-prompt ... prompt for set map command'
         call io_getopt('Edit-option (?=help) : ','auto-discard-on',
     *                   option_list,3,option,status)
         if (chr_cmatch('auto-discard-on',option)) then
            edit_mode = 1
         elseif (chr_cmatch('auto-discard-off',option)) then
            edit_mode = 0
         elseif (chr_cmatch('set-map-prompt',option)) then
            call io_getstr('Set-Map-Prompt : ','Catalogue-Entry',
     *                     sm_prompt,status)
            if (status.ne.0) then
               sm_prompt = ' '
            endif
         endif
         call cmd_err( status,'Edit-Options','Failed' )

       end if
       cdata(1) = 100
       call dpredt( rt, status )
       if (status.ne.0 .and.  edit_mode.eq.1) then
         call map_end_alloc( iedit, map_array, status )
         iedit = -1
         edit_mode = 0
         sm_prompt = ' '
       endif

       end
C
C
       real*4 function imed_value( val )
C      ---------------------------------
C
C Return a replacement value for an image pixel
C
C Given:
C   value at pixel
       real*4       val
C-

C control data for the editing
       integer                  imap, iedit, ip_edit, replacement_mode,
     *                          add_mode, map_minirt(8), edit_mode
       real*4                   blank, fixed, SD, mean, scale, zero,
     *                          gate_1, gate_2

       common /save_image_edit/ imap, iedit, ip_edit, replacement_mode,
     *                          add_mode, blank, fixed, SD, mean,
     *                          scale, zero, map_minirt, edit_mode,
     *                          gate_1, gate_2

       real*4                   max_val, max_val10
       real*8                   util_rnnorm

       max_val = log(1.0E35)
       max_val10 = log10(1.0E35)

       if (replacement_mode.eq.4) then
         if (val.eq.blank) then
            imed_value = val
         else
           if (add_mode.eq.0) then
              imed_value = (val-zero)*scale
           elseif (add_mode.eq.1) then
              if (val.le.0.0) then
                 imed_value = blank
              else
                 imed_value = log(val)
              endif
           elseif (add_mode.eq.2) then
              if (val.le.0.0) then
                 imed_value = blank
              else
                 imed_value = log10(val)
              endif
           elseif (add_mode.eq.3) then
              if (val.le.max_val) then
                imed_value = exp(val)
              else
                imed_value = blank
              endif
           elseif (add_mode.eq.4) then
              if (val.le.max_val10) then
                imed_value = 10.0**(val)
              else
                imed_value = blank
              endif
           elseif (add_mode.eq.5) then
              if (val.le.0.0) then
                 imed_value = blank
              else
                 imed_value = sqrt(val)
              endif
           elseif (add_mode.eq.6) then
              imed_value = imed_value * imed_value
           elseif (add_mode.eq.7) then
              if (val.eq.0.0) then
                 imed_value = blank
              else
                 imed_value = 1.0/val
              endif
           elseif (add_mode.eq.20) then
              if (val.lt.gate_1 .or. val.gt.gate_2) then
                 imed_value = 0.0
              else
                 imed_value = 1.0
              endif
           elseif (add_mode.eq.21) then
              if (val.lt.gate_1 .or. val.gt.gate_2) then
                 imed_value = fixed
              else
                 imed_value = val
              endif

           endif
         endif

       elseif (replacement_mode.eq.3) then
          imed_value = (val-zero)*scale

       else
         if (replacement_mode.eq.2) then
           imed_value = fixed
         elseif (replacement_mode.eq.1) then
           imed_value = util_rnnorm(mean,SD)
         else
           imed_value = blank
         endif
         if (add_mode.eq.1) imed_value = imed_value + val
       endif

       end









