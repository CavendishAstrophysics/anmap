*+ graphic_get_grline

       subroutine graphic_get_grline( gopt, ntb, ib, tb, s )
C      -----------------------------------------------------
C
C Read a description for graphics line options from the user
C
       include '../include/plt_buffer_defn.inc'
C
C Updated:
C   graphics line options structure
       integer         gopt(*)
C Given:
C   number of text buffers
       integer         ntb
C Updated:
C   index to text buffers
       integer         ib(ntb)
C   text buffers
       character*(ltb) tb(ntb)
C   error status
       integer         s
C
C A series of options are presented to the user to modify the graphics
C line options for the grline structure
C-

       include '../include/plt_basic_defn.inc'
       include '../include/plt_grline_defn.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C local variables
       integer                  l, lf, ls, lcli
       integer                  nopt
       character*60             option(28), opt, string*80
       character*(iolen_file)   file
       character*256            function

C check status on entry
       if ( s.ne.0 ) return

C copy the structure to the standard definition
       do l = 1,len_grline
         grline(l) = gopt(l)
       enddo

C get default options if not initialised
       if (grline_status.eq.0) then
         call graphic_default_grline( grline, 3, s )
       endif

C setup sub-options for this command
       option(1) ='reset-style ...... reset style options'
       option(2) ='reset-file ....... reset file name(s) & columns'
       option(3) ='file ............. set file name for X & Y axes'
       option(4) ='list ............. set list names for X & Y axes'
       option(5) ='function ......... set function to plot'
       option(6) ='x-file ........... set file name for X axis'
       option(7) ='y-file ........... set file name for Y axis'
       option(8) ='x-column ......... specify the X-column in x-file'
       option(9) ='y-column ......... specify the Y-column in y-file'
       option(10)='x-offset ......... specify absolute offset in X'
       option(11)='y-offset ......... specify absolute offset in X'
       option(12)='x-scale .......... specify scaling in X'
       option(13)='y-scale .......... specify scaling in Y'
       option(14)='x-auto-scale ..... auto-scale in X to line 1'
       option(15)='y-auto-scale ..... auto-scale in Y to line 1'
       option(16)='x-error-bar ...... request error bars in X'
       option(17)='y-error-bar ...... request error bars in Y'
       option(18)='x-limits ......... request limits in X'
       option(19)='y-limits ......... request limits in Y'
       option(20)='log-scale ........ select log scale in X & Y'
       option(21)='x-log-scale ...... select log scale in X'
       option(22)='y-log-scale ...... select log scale in Y'
       option(23)='line-style ....... set line-style for line'
       option(24)='error-style ...... set style for error bars'
       option(25)='symbol-type ...... (de-)select marking data points'
       option(26)='line-type ........ set line type (none/std./hist.)'
       option(27)='symbol-style ..... set text style for symbols'
       option(28)='key-display ...... add this line to key (if shown)'
       nopt = 28

*       print *,'get option'
 1     call io_getopt( 'Option (?=list) : ','reset-style',option,nopt,
     *                 opt,s)
*       print *,'decoding'

C take action on options
       if (chr_cmatch(opt,'reset-style')) then
          call graphic_default_grline(grline,1,s)

       elseif (chr_cmatch(opt,'reset-file')) then
          call graphic_default_grline(grline,2,s)

       elseif (chr_cmatch(opt,'file')) then
          if (grline_x_file.ne.0) then
            call graphic_enq_text(ntb,ib,tb,grline_x_file,file,s)
          else
            file = 'data.dat'
          endif
          call io_getwrd('File-name : ',file(1:chr_lenb(file)),
     *                   file,lf,s)
          if (s.eq.0) then
            if (grline_x_file.ne.0) then
              call graphic_delete_text(ntb,ib,tb,grline_x_file,s)
            endif
            if (grline_y_file.ne.0) then
              call graphic_delete_text(ntb,ib,tb,grline_y_file,s)
            endif
          endif
          call graphic_put_text(ntb,ib,tb,file(1:lf),l,s)
          if (s.eq.0) then
            grline_x_file = l
            grline_y_file = l
          endif

       elseif (chr_cmatch(opt,'function')) then
          if (grline_func_opt.eq.grline_func_defined .and.
     *        grline_func_defn.ne.0) then
            call graphic_enq_text(ntb,ib,tb,
     *                            grline_func_defn,function,s)
          else
            function = ' ' 
          endif
          call io_getwrd('Function (in quotes) : ','*',
     *                   function,lf,s)
          if (s.eq.0) then
            if (grline_func_defn.ne.0) then
              call graphic_delete_text(ntb,ib,tb,grline_func_defn,s)
            endif
          endif
          call graphic_put_text(ntb,ib,tb,function(1:lf),
     *                          grline_func_defn,s)
          if (s.eq.0) then
            grline_func_opt = grline_func_defined
          endif

       elseif (chr_cmatch(opt,'list')) then
          if (grline_list_opt.eq.grline_list_defined .and.
     *        grline_list_defn.ne.0) then
            call graphic_enq_text(ntb,ib,tb,
     *                            grline_list_defn,function,s)
          else
            function = ' ' 
          endif
          call io_getwrd('X and Y lists (in quotes) : ','*',
     *                   function,lf,s)
          if (s.eq.0) then
            if (grline_list_defn.ne.0) then
              call graphic_delete_text(ntb,ib,tb,grline_list_defn,s)
            endif
          endif
          call graphic_put_text(ntb,ib,tb,function(1:lf),
     *                          grline_list_defn,s)
          if (s.eq.0) then
            grline_list_opt = grline_list_defined
          endif

       elseif (chr_cmatch(opt,'x-file')) then
          if (grline_x_file.ne.0) then
            call graphic_enq_text(ntb,ib,tb,grline_x_file,file,s)
          else
            file = 'data.dat'
          endif
          call io_getwrd('X-file-name : ',file(1:chr_lenb(file)),
     *                   file,lf,s)
          if (s.eq.0) then
            if (grline_x_file.ne.0) then
              call graphic_delete_text(ntb,ib,tb,grline_x_file,s)
            endif
          endif
          call graphic_put_text(ntb,ib,tb,file(1:lf),l,s)
          if (s.eq.0) then
            grline_x_file = l
          endif

       elseif (chr_cmatch(opt,'y-file')) then
          if (grline_y_file.ne.0) then
            call graphic_enq_text(ntb,ib,tb,grline_y_file,file,s)
          elseif (grline_x_file.ne.0) then
            call graphic_enq_text(ntb,ib,tb,grline_x_file,file,s)
          else
            file = 'data.dat'
          endif
          call io_getwrd('Y-file-name : ',file(1:chr_lenb(file)),
     *                   file,lf,s)
          if (s.eq.0) then
            if (grline_y_file.ne.0) then
              call graphic_delete_text(ntb,ib,tb,grline_y_file,s)
            endif
          endif
          call graphic_put_text(ntb,ib,tb,file(1:lf),l,s)
          if (s.eq.0) then
            grline_y_file = l
          endif

       elseif (chr_cmatch(opt,'x-column')) then
          call io_geti('X-column : ','*',grline_x_col,s)

       elseif (chr_cmatch(opt,'y-column')) then
          call io_geti('Y-column : ','*',grline_y_col,s)

       elseif (chr_cmatch(opt,'x-offset')) then
          call io_getr('Offset-in-x : ','*',grline_offset_x,s)

       elseif (chr_cmatch(opt,'y-offset')) then
          call io_getr('Offset-in-y : ','*',grline_offset_y,s)

       elseif (chr_cmatch(opt,'x-scale')) then
          call io_getr('Scale-x : ','*',grline_scale_x,s)

       elseif (chr_cmatch(opt,'y-scale')) then
          call io_getr('Scale-y : ','*',grline_scale_y,s)

       elseif (chr_cmatch(opt,'x-auto-scale')) then
          grline_auto_x = io_onoff('X-auto-scaling (on/off) : ',
     *                             'off',s)

       elseif (chr_cmatch(opt,'log-scale')) then
          if (io_onoff('X-log-scaling (on/off) : ','off',s)) then
            grline_x_log = .true.
            grline_y_log = .true.
          endif

       elseif (chr_cmatch(opt,'x-log-scale')) then
          grline_x_log = io_onoff('X-log-scaling (on/off) : ',
     *                             'off',s)

       elseif (chr_cmatch(opt,'y-log-scale')) then
          grline_y_log = io_onoff('Y-log-scaling (on/off) : ',
     *                             'off',s)

       elseif (chr_cmatch(opt,'y-auto-scale')) then
          grline_auto_y = io_onoff('Y-auto-scaling (on/off) : ',
     *                             'off',s)

       elseif (chr_cmatch(opt,'line-style')) then
          call graphic_get_line_opt(grline_line_opt,s)

       elseif (chr_cmatch(opt,'error-style')) then
          call io_getr('Size of error-top-x : ','*',grline_ex_top,s)
          call io_getr('Size of error-top-y : ','*',grline_ey_top,s)
          call graphic_get_line_opt(grline_error_opt,s)

       elseif (chr_cmatch(opt,'symbol-style')) then
          call graphic_get_text_opt(grline_text_opt,s)

       elseif (chr_cmatch(opt,'symbol-type')) then
          call io_geti('Line-display (0=none >0=symbol) : ',
     *                 '*',grline_symbol,s)

       elseif (chr_cmatch(opt,'line-type')) then
          call io_geti('Line-display (0=none 1=line 2=histogram) : ',
     *                 '*',grline_type,s)

       elseif (chr_cmatch(opt,'y-error-bar')) then
          if (io_yesno('Display-y-error-bar ? ','no',s)) then
            grline_ey_opt = 1
            if (grline_list_opt.eq.grline_list_defined) then
               call io_getwrd('Y-error-bar : ',
     *              file(1:chr_lenb(file)),file,lf,s)
            else
            if (grline_ey_file.ne.0) then
              call graphic_enq_text(ntb,ib,tb,grline_ey_file,file,s)
            elseif (grline_y_file.ne.0) then
              call graphic_enq_text(ntb,ib,tb,grline_y_file,file,s)
            elseif (grline_x_file.ne.0) then
              call graphic_enq_text(ntb,ib,tb,grline_x_file,file,s)
            else
              file = 'data.dat'
            endif
            call io_getwrd('File-y-error-bar : ',
     *                     file(1:chr_lenb(file)),file,lf,s)
            call io_geti('Column-error-bar-y : ','3',grline_ey_col,s)
            endif
            if (s.eq.0) then
              if (grline_ey_file.ne.0) then
                call graphic_delete_text(ntb,ib,tb,grline_ey_file,s)
              endif
            endif
            call graphic_put_text(ntb,ib,tb,file(1:lf),l,s)
            if (s.eq.0) then
              grline_ey_file = l
            endif
          else
            grline_ey_opt = 0
          endif

       elseif (chr_cmatch(opt,'x-error-bar')) then
          if (io_yesno('Display-x-error-bar ? ','no',s)) then
            grline_ex_opt = 1
            if (grline_list_opt.eq.grline_list_defined) then
               call io_getwrd('X-error-bar : ',
     *              file(1:chr_lenb(file)),file,lf,s)
            else
            if (grline_ex_file.ne.0) then
              call graphic_enq_text(ntb,ib,tb,grline_ex_file,file,s)
            elseif (grline_x_file.ne.0) then
              call graphic_enq_text(ntb,ib,tb,grline_x_file,file,s)
            else
              file = 'data.dat'
            endif
            call io_getwrd('File-x-error-bar : ',
     *                     file(1:chr_lenb(file)),file,lf,s)
            call io_geti('Column-error-bar-y : ','3',grline_ex_col,s)
            endif
            if (s.eq.0) then
              if (grline_ex_file.ne.0) then
                call graphic_delete_text(ntb,ib,tb,grline_ex_file,s)
              endif
            endif
            call graphic_put_text(ntb,ib,tb,file(1:lf),l,s)
            if (s.eq.0) then
              grline_ex_file = l
            endif
          else
            grline_ex_opt = 0
          endif

       elseif (chr_cmatch(opt,'y-limits')) then
          if (io_yesno('Display-y-limits ? ','no',s)) then
            grline_ey_opt = 2
            if (grline_list_opt.eq.grline_list_defined) then
               call io_getwrd('Y-limit-list : ',
     *              file(1:chr_lenb(file)),file,lf,s)
            else
            if (grline_ey_file.ne.0) then
              call graphic_enq_text(ntb,ib,tb,grline_ey_file,file,s)
            elseif (grline_y_file.ne.0) then
              call graphic_enq_text(ntb,ib,tb,grline_y_file,file,s)
            elseif (grline_x_file.ne.0) then
              call graphic_enq_text(ntb,ib,tb,grline_x_file,file,s)
            else
              file = 'data.dat'
            endif
            call io_getwrd('File-y-error-bar : ',
     *                     file(1:chr_lenb(file)),file,lf,s)
            call io_geti('1st-Column-y-limit : ','3',
     *                    grline_ey_col,s)
            endif
            if (s.eq.0) then
              if (grline_ey_file.ne.0) then
                call graphic_delete_text(ntb,ib,tb,grline_ey_file,s)
              endif
            endif
            call graphic_put_text(ntb,ib,tb,file(1:lf),l,s)
            if (s.eq.0) then
              grline_ey_file = l
            endif
          else
            grline_ey_opt = 0
          endif

       elseif (chr_cmatch(opt,'x-limits')) then
          if (io_yesno('Display-x-limits ? ','no',s)) then
            grline_ex_opt = 2
            if (grline_list_opt.eq.grline_list_defined) then
               call io_getwrd('X-limit-list : ',
     *              file(1:chr_lenb(file)),file,lf,s)
            else
            if (grline_ex_file.ne.0) then
              call graphic_enq_text(ntb,ib,tb,grline_ex_file,file,s)
            elseif (grline_x_file.ne.0) then
              call graphic_enq_text(ntb,ib,tb,grline_x_file,file,s)
            elseif (grline_y_file.ne.0) then
              call graphic_enq_text(ntb,ib,tb,grline_y_file,file,s)
            else
              file = 'data.dat'
            endif
            call io_getwrd('File-x-error-bar : ',
     *                     file(1:chr_lenb(file)),file,lf,s)
            call io_geti('1st-Column-x-limit : ','3',
     *                    grline_ex_col,s)
            endif
            if (s.eq.0) then
              if (grline_ex_file.ne.0) then
                call graphic_delete_text(ntb,ib,tb,grline_ex_file,s)
              endif
            endif
            call graphic_put_text(ntb,ib,tb,file(1:lf),l,s)
            if (s.eq.0) then
              grline_ex_file = l
            endif
          else
            grline_ex_opt = 0
          endif

       elseif (chr_cmatch(opt,'key-display')) then
          call io_geti(
     *         'Key-option (0=off 1=file-name 2=supplied-text) : ',
     *         '*',grline_key_opt,s)
          if (grline_key_opt.eq.2) then
            call io_getwrd('Key-string (''in quotes'') : ',
     *                     ' ',string,ls,s)
            if (s.eq.0) then
              if (grline_key_text.ne.0) then
                 call graphic_delete_text(ntb,ib,tb,grline_key_text,s)
              endif
            endif
            call graphic_put_text(ntb,ib,tb,string(1:ls),
     *                            grline_key_text,s)
          endif
       endif
       call io_enqcli(file,lcli)
       if (lcli.gt.0) goto 1

C copy structure to output
       if (s.eq.0) then
         if (grline_status.eq.0) grline_status = 1
         do l=1,len_grline
           gopt(l) = grline(l)
         enddo
       endif
999    continue
       call cmd_err( s, 'graphic_get_grline', ' ')

       end





