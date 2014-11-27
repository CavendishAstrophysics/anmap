C
C
*+ plot_style

       subroutine plot_style(status)
C      -----------------------------
C
C Set the various style options for the display
C
C Updated:
C   errors status
       integer    status
C
C This routine implements the set-style options for the map-display
C sub-system.
*-
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C options list
       integer       max_options
       parameter    (max_options = 9)
       character*50  option_list(max_options), option
       integer       max_sub_options
       parameter    (max_sub_options = 10)
       character*50  sub_option_list(max_sub_options), sub_option

C local variables
       integer       i

       data option_list /'title ....... set title options',
     *                   'frame ....... set frame options',
     *                   'contours .... set contour options',
     *                   'grid ........ set grid type',
     *                   'symbol ...... set symbol style',
     *                   'vectors ..... set style for vectors',
     *                   'annotations . set options for annotations',
     *                   'scale-bar ... set scale-bar options',
     *                   'reset ....... reset style options'/

C check status on entry
       if (status.ne.0) return

C prompt for option
       call io_getopt('Option (?=help) : ',
     *             'RESET',option_list,max_options,option,status)
       call chr_chucas(option)
       if (status.ne.0) then
         call cmd_err(status,'SET-STYLE',' ')
         return
       end if

       if (chr_cmatch('title',option(1:chr_lenb(option)))) then
         sub_option_list(1) = 'on .............. enable title on plot'
         sub_option_list(2) = 'off ............. disable title on plot'
         sub_option_list(3) = 'text-style ...... options for title text'
         call io_getopt('Sub-Option (?=list) : ',
     *                  'on',sub_option_list,3,sub_option,status)
         if (chr_cmatch('on',sub_option)) then
           title_opt = .true.
         elseif (chr_cmatch('off',sub_option)) then
           title_opt = .false.
         else
           call graphic_get_text_opt(title_text_style,status)
         endif

       elseif (chr_cmatch('frame',option(1:chr_lenb(option)))) then
         sub_option_list(1) = 'on .............. enable frame on plot'
         sub_option_list(2) = 'off ............. disable frame on plot'
         sub_option_list(3) = 'text-style ...... options for frame text'
         sub_option_list(4) = 'line-style ...... options for frame line'
         sub_option_list(5) = 'space ........... space around the frame'
         sub_option_list(6) = 'pip-size ........ pip size'
         sub_option_list(7) = 'pip-x-offset .... pip lable DEC offset'
         sub_option_list(8) = 'pip-y-offset .... pip lable RA offset'

         call io_getopt('Sub-Option (?=list) : ',
     *                  'on',sub_option_list,8,sub_option,status)
         if (chr_cmatch('on',sub_option)) then
           pframe_opt = .true.
         elseif (chr_cmatch('off',sub_option)) then
           pframe_opt = .false.
         elseif (chr_cmatch('text-style',sub_option)) then
           call graphic_get_text_opt(frame_text_style,status)
         elseif (chr_cmatch('line-style',sub_option)) then
           call graphic_get_line_opt(frame_line_style,status)
         elseif (chr_cmatch('pip-size',sub_option)) then
           call io_getr('Pip-size : ','*',pip_size,status)
         elseif (chr_cmatch('pip-x-offset',sub_option)) then
           call io_getr('Pip-x-offset : ','*',pip_off_x,status)
         elseif (chr_cmatch('pip-y-offset',sub_option)) then
           call io_getr('Pip-y-offset : ','*',pip_off_y,status)
         else
           call io_getr('Frame-space (0.0-1.0) : ','*',
     *                   frame_space,status)
         endif

       elseif (chr_cmatch('grid',option(1:chr_lenb(option)))) then
         sub_option_list(1) = 'line-style ...... options for grid line'
         call io_getopt('Sub-Option (?=list) : ',
     *           'line-style',sub_option_list,1,sub_option,status)
         if (chr_cmatch('line-style',sub_option)) then
           call graphic_get_line_opt(grid_line_style,status)
         endif

       elseif (chr_cmatch('symbol',option(1:chr_lenb(option)))) then
         sub_option_list(1)='text-style ...... options for symbol text'
         call io_getopt('Sub-Option (?=list) : ',
     *            'text-style',sub_option_list,1,sub_option,status)
         if (chr_cmatch('text-style',sub_option)) then
           call graphic_get_text_opt(symbol_text_style,status)
         endif

       elseif (chr_cmatch('vectors',option(1:chr_lenb(option)))) then
         sub_option_list(1) = 'line-style ...... options for grid line'
         call io_getopt('Sub-Option (?=list) : ',
     *              'line-style',sub_option_list,1,sub_option,status)
         if (chr_cmatch('line-style',sub_option)) then
           call graphic_get_line_opt(vector_line_style,status)
         endif

       elseif (chr_cmatch('scale-bar',option(1:chr_lenb(option)))) then
         sub_option_list(1) = 'on .............. enable scale-bar'
         sub_option_list(2) = 'off ............. disable scale-bar'
         sub_option_list(3) = 'width ........... specify width'
         sub_option_list(4) = 'text-on ......... annotate scale-bar'
         sub_option_list(5) = 'text-off ........ plain scale-bar'
         sub_option_list(6) = 'text-style ...... style for text'
         sub_option_list(7) = 'inverted-wedge ...enable inverted wedge'

         call io_getopt('Sub-Option (?=list) : ',
     *                  'on',sub_option_list,7,sub_option,status)
         if (chr_cmatch('on',sub_option)) then
           scale_bar_opt = 1
         elseif (chr_cmatch('off',sub_option)) then
           scale_bar_opt = 0
         elseif (chr_cmatch('width',sub_option)) then
           call io_getr('Scale-bar-Width : ','*',scale_bar_width,status)
         elseif (chr_cmatch('text-on',sub_option)) then
           scale_bar_text = 1
         elseif (chr_cmatch('inverted-wedge',sub_option)) then
           scale_bar_opt = -1
         elseif (chr_cmatch('text-style',sub_option)) then
           call graphic_get_text_opt(scale_bar_text_style,status)
         else
           scale_bar_text = 0
         endif

       elseif
     *   (chr_cmatch('annotations',option(1:chr_lenb(option)))) then
         sub_option_list(1) = 'date-on ......... add date'
         sub_option_list(2) = 'date-off ........ do not add date'
         sub_option_list(3) = 'label-on ........ add information'
         sub_option_list(4) = 'label-off ....... do not add information'
         sub_option_list(5) = 'text-style ...... annotation text style'
         sub_option_list(6) = 'text-drop ....... drop between lines'
         sub_option_list(7) = 'title-on ........ add title'
         sub_option_list(8) = 'title-off ....... do not add title'
         sub_option_list(9) = 'all-on .......... add everything'
         sub_option_list(10)= 'all-off ......... do not add anything'
         call io_getopt('Sub-Option (?=list) : ',
     *             'all-on',sub_option_list,10,sub_option,status)
         if (chr_cmatch('date-on',sub_option)) then
           date_opt = .true.
         elseif (chr_cmatch('date-off',sub_option)) then
           date_opt = .false.
         elseif (chr_cmatch('label-on',sub_option)) then
           cont_opt = .true.
         elseif (chr_cmatch('label-off',sub_option)) then
           cont_opt = .false.
         elseif (chr_cmatch('title-on',sub_option)) then
           title_opt = .true.
         elseif (chr_cmatch('title-off',sub_option)) then
           title_opt = .false.
         elseif (chr_cmatch('all-on',sub_option)) then
           title_opt = .true.
           cont_opt = .true.
           date_opt = .true.
         elseif (chr_cmatch('all-off',sub_option)) then
           title_opt = .false.
           cont_opt = .false.
           date_opt = .false.
         elseif (chr_cmatch('text-style',sub_option)) then
           call graphic_get_text_opt(annotations_text_style,status)
         elseif (chr_cmatch('text-drop',sub_option)) then
           call io_getr('Text-drop : ','*',annotation_drop,status)
         endif

       elseif (chr_cmatch('contours',option(1:chr_lenb(option)))) then
         sub_option_list(1) = 'line-style ...... set line style'
         sub_option_list(2) = 'positive-style .. set positive style'
         sub_option_list(3) = 'negative-style .. set negative style'
         sub_option_list(4) = 'zero-style ...... set zero style'

         call io_getopt('Sub-Option (?=list) : ',
     *                  'line-style',sub_option_list,4,
     *                  sub_option,status)
         if (chr_cmatch('line-style',sub_option)) then
           call io_geti('Style for which line (1-5) : ','1',
     *                  i,status)
           if (i.lt.1 .or. i.gt.5) then
             i = 1
           endif
           call graphic_get_line_opt(contour_styles(1,i),status)
         elseif (chr_cmatch('positive-style',sub_option)) then
           i = positive_style
           call io_geti('Style for positive lines (1-5) : ','*',
     *                  i,status)
           if (i.lt.1 .or. i.gt.5) then
             i = 1
           endif
           positive_style = i
         elseif (chr_cmatch('negative-style',sub_option)) then
           i = negative_style
           call io_geti('Style for negative lines (1-5) : ','*',
     *                  i,status)
           if (i.lt.1 .or. i.gt.5) then
             i = 1
           endif
           negative_style = i
         elseif (chr_cmatch('zero-style',sub_option)) then
           i = zero_style
           call io_geti('Style for zero lines (1-5) : ','*',
     *                  i,status)
           if (i.lt.1 .or. i.gt.5) then
             i = 1
           endif
           zero_style = i
         endif

       elseif (chr_cmatch('reset',option(1:chr_lenb(option)))) then
         call plot_init_opt(status)

       end if

C handle errors
       call cmd_err(status,'SET-STYLE',' ')

       end
