C ANMAP graphics basic structure definitions: plt_basic_defn
C -------------------------------------------
C
C This file defines the basic structures used to define plotting objects 
C in ANMAP, it should be included as the first include file in all plotting
C routines accessing other structure defintions.
C
C Copyright P. Alexander, MRAO, Cavendish Laboratory, Cambridge
C 
C Current Version 1.0   15/02/93
C
C History:
C --------
C Version 0.1 30/10/92
C Version 1.0 15/02/92 -- added definition of total structure size
C Version 1.1 21/08/93 -- added support for arrow style

       integer      len_struct
       parameter   (len_struct = 2000)

C LINE_OPT structure defining line options
C --------
C
C This structure defines the options for text
       integer       len_line_opt
       parameter    (len_line_opt = 5)
       integer       line_opt(len_line_opt)
C   option to specify that this line options structure has been set
       integer       line_opt_set
       equivalence  (line_opt_set, line_opt(1))
C   line style
       integer       line_opt_style
       equivalence  (line_opt_style, line_opt(2))
C  line colour
       integer       line_opt_colour
       equivalence  (line_opt_colour,  line_opt(3))
C  line width
       integer       line_opt_width
       equivalence  (line_opt_width,  line_opt(4))

C TEXT_OPT structure defining text options
C --------
C
C This structure defines the options for text
       integer       len_text_opt
       parameter    (len_text_opt = 5)
       integer       text_opt(len_text_opt)
C   option to specify that this text options structure has been set
       integer       text_opt_set
       equivalence  (text_opt_set, text_opt(1))
C   font
       integer       text_opt_font
       equivalence  (text_opt_font, text_opt(2))
C   text size
       real*4        text_opt_size
       equivalence  (text_opt_size, text_opt(3))
C   text width
       integer       text_opt_width
       equivalence  (text_opt_width, text_opt(4))
C   text colour
       integer       text_opt_colour
       equivalence  (text_opt_colour, text_opt(5))

C ARROW_OPT structure defining arrow options
C --------
C
C This structure defines the options for arrows
       integer       len_arrow_opt
       parameter    (len_arrow_opt = 5)
       integer       arrow_opt(len_arrow_opt)
C   option to specify that this arrow options structure has been set
       integer       arrow_opt_set
       equivalence  (arrow_opt_set, arrow_opt(1))
C   arrow style
       integer       arrow_opt_style
       equivalence  (arrow_opt_style, arrow_opt(2))
C   arrow colour
       integer       arrow_opt_colour
       equivalence  (arrow_opt_colour, arrow_opt(3))
C   arrow width
       integer       arrow_opt_width
       equivalence  (arrow_opt_width, arrow_opt(4))
C   arrow size
       real*4        arrow_opt_size
       equivalence  (arrow_opt_size, arrow_opt(5))

C FILL_OPT structure defining fill options
C --------
C
C This structure defines the options for fills
       integer       len_fill_opt
       parameter    (len_fill_opt = 5)
       integer       fill_opt(len_fill_opt)
C   option to specify that this fill options structure has been set
       integer       fill_opt_set
       equivalence  (fill_opt_set, fill_opt(1))
C   arrow style
       integer       fill_opt_opt
       equivalence  (fill_opt_opt, fill_opt(2))

C GENERAL_OPT sructure to be used to hold a general type of option
C -----------
C
C This structure is large enough to hold a LINE_OPT, TEXT_OPT structure
C and for a line a fill-style also
       integer       len_general_opt
       parameter    (len_general_opt = 5)


C GRAPHIC structure defining basic graphics options
C -------
C
C This defines the basic options for all other structures
       integer       len_graphic_basic
       parameter    (len_graphic_basic = 10)
       integer       len_graphic
       parameter    (len_graphic = len_graphic_basic +
     *                             4*len_general_opt )
       integer       graphic(len_graphic)
C   option to indicate status of plotting this structre
       integer       graphic_status
       equivalence  (graphic_status, graphic(1))
C   view-port in normalized device coordinates
       real*4        graphic_view_port(4)
       equivalence  (graphic_view_port(1), graphic(2))
C   option to specify transparency
       logical       graphic_transparent
       equivalence  (graphic_transparent, graphic(6))
C   depth specified relative depth in the field; 0 in front
       integer       graphic_depth
       equivalence  (graphic_depth, graphic(7))
C   type of graphic structure
       integer       graphic_type
       equivalence  (graphic_type, graphic(8))
C   index of this graphic structure
       integer       graphic_index
       equivalence  (graphic_index, graphic(9))
C   preferred device for this graphic
       integer       graphic_device
       equivalence  (graphic_device, graphic(10))
C   include default text options
       integer       graphic_text_opts(len_text_opt)
       equivalence  (graphic_text_opts(1), graphic(11))
C   include default line and arrow options
       integer       graphic_line_opts(len_line_opt)
       integer       ip_grlo
       parameter    (ip_grlo = len_graphic_basic+len_text_opt+1)
       equivalence  (graphic_line_opts(1), graphic(ip_grlo))
C   include default arrow style
       integer       graphic_arrow_opts(len_arrow_opt)
       integer       ip_grao
       parameter    (ip_grao = ip_grlo+len_line_opt)
       equivalence  (graphic_arrow_opts(1), graphic(ip_grao))
C   include default fill style
       integer       graphic_fill_opts(len_fill_opt)
       integer       graphic_fill_opt
       equivalence  (graphic_fill_opt, graphic_fill_opts(2))
       integer       ip_grfo
       parameter    (ip_grfo = ip_grao + len_arrow_opt)
       equivalence  (graphic_fill_opt, graphic(ip_grfo))

C OBJECT basic graphic object
C ------
C
C this object specifies a basic -- non-text -- object, but includes
C a pointer to a possible list of labels which are TEXT_STRING objects
C
       integer       len_object
       parameter    (len_object = len_general_opt + 10)
       integer       object(len_object)
C   set option to indicate status of this object
       integer       object_status
       equivalence  (object_status, object(1))
C   object type
       integer       object_type
       equivalence  (object_type,  object(2))
C   position and dimensions of the object
       real*4        object_dim(6)
       equivalence  (object_dim(1), object(3))
C   fill-style for this object (if appropriate
       integer       object_fill
       equivalence  (object_fill, object(9))
C   pointer to character label or additional information
       integer       object_lable
       equivalence  (object_lable, object(10))
C   include general options 
       integer       object_opt(len_general_opt)
       equivalence  (object_opt(1), object(11))


C AXIS_OPT structure definining an exis type
C --------
C
C This structure defines the options for an axis 
C    Note:  "X" axis 1=bottom 2=top;  "Y" axis 1=left 2=right
       integer      len_axis_opt
       parameter    (len_axis_opt = 15 + 2*len_line_opt)
       integer      axis_opt(len_axis_opt)
C    option to specify this has been initialised
       integer      axis_opt_set
       equivalence (axis_opt_set, axis_opt(1))
C    draw X,Y=0 axis
       logical      axis_opt_axis
       equivalence (axis_opt_axis, axis_opt(2))
C    draw 1 and 2 sides of frame, none or both
       integer      axis_opt_side
       equivalence (axis_opt_side, axis_opt(3))
C    draw grid
       logical      axis_opt_grid
       equivalence (axis_opt_grid, axis_opt(4))
C    draw labels on 1 or 2 sides, none or both
       integer      axis_opt_label
       equivalence (axis_opt_label, axis_opt(5))
C    draw labels in rotated orientation
       logical      axis_opt_labelr
       equivalence (axis_opt_labelr, axis_opt(6))
C    draw major, large (l) and minor, small (s) tick marks
       logical      axis_opt_tickl, axis_opt_ticks
       equivalence (axis_opt_tickl, axis_opt(9))
       equivalence (axis_opt_ticks, axis_opt(10))
C    draw tick marks in, out or both
       integer      axis_opt_tickt
       equivalence (axis_opt_tickt, axis_opt(11))
C    spacing between major (large) ticks
       real*4       axis_opt_xtick
       equivalence (axis_opt_xtick, axis_opt(12))
C    number of minor ticks
       integer      axis_opt_ntick
       equivalence (axis_opt_ntick, axis_opt(13))
C    use logarithmic axes
       logical      axis_opt_log
       equivalence (axis_opt_log, axis_opt(14))
C    line style for axes and grid
       integer      axis_opt_line_axis(len_line_opt)
       equivalence (axis_opt_line_axis, axis_opt(15))
       integer      axis_opt_line_grid(len_line_opt)
       equivalence (axis_opt_line_grid, axis_opt(15+len_line_opt))

    
C FRAME_OPT structure defining a plot frame
C ---------
C
C This structure defined the appropriate options for a frame
       integer      len_frame_opt
       parameter   (len_frame_opt = 1 + 2*len_axis_opt + len_text_opt)
       integer      frame_opt(len_frame_opt)
C   option to specify the structure has been initialised
       integer      frame_opt_set
       equivalence (frame_opt_set, frame_opt(1))
C   axis type for X and Y
       integer      frame_opt_x_axis(len_axis_opt)
       integer      frame_opt_y_axis(len_axis_opt)
       equivalence (frame_opt_x_axis, frame_opt(2))
       equivalence (frame_opt_y_axis, frame_opt(2+len_axis_opt))
C   text style for labels
       integer      frame_opt_text_opt(len_text_opt)
       equivalence (frame_opt_text_opt, frame_opt(2+2*len_axis_opt))

C
C Global plot control parameters
C ------------------------------
C
C The following data control the overall plot and apply to
C all graphic objects
C
C flag to indicate the plot device is open
       logical    plot_open
C flag to indicate plotting of "next" frame
       logical    plot_next
C flag to indicate the plot should be refreshed
       logical    plot_refresh
C flag to indicate the everything should be plotted
       logical    plot_all
C number of plot segments
       integer    plot_segments
C size and aspect ratio of the plot device
       real*4     plot_size, plot_aspect
C name of the (default) plot device
       character  plot_device*80
C number of the current selected device and default device
       integer    plot_list_selected, plot_list_default
C List of all active devices and associated ID's
       integer       plot_list_max
       parameter    (plot_list_max = 3)
       character*60  plot_list_device(plot_list_max)
       integer       plot_list_id(plot_list_max)
       logical       plot_list_open(plot_list_max)
       real*4        plot_list_aspect(plot_list_max)
       real*4        plot_list_size(plot_list_max)

       common /graphic_plot_control/ plot_open, plot_next, 
     *                               plot_refresh, plot_all,
     *                               plot_segments, plot_size,
     *                               plot_aspect, plot_list_selected,
     *                               plot_list_id, plot_list_open,
     *                               plot_list_default, 
     *                               plot_list_aspect,
     *                               plot_list_size
       common /graphic_plot_text/    plot_device, plot_list_device

C
C Specify all optional styles 
C
C Line-styles
       integer    line_solid, line_dash, line_dot_dash,
     *            line_dot, line_dash_dot_dot, line_min, line_max
       parameter (line_min          = 1)
       parameter (line_max          = 5)
       parameter (line_solid        = 1)
       parameter (line_dash         = 2)
       parameter (line_dot_dash     = 3)
       parameter (line_dot          = 4)
       parameter (line_dash_dot_dot = 5)

       integer    len_line_name
       parameter (len_line_name = 12)
       character*(len_line_name) line_name(line_max)
       data         line_name/ 'solid', 
     *                         'dashed',
     *                         'dot-dash',
     *                         'dotted',
     *                         'dash_dot_dot' /

C Colours
       integer    colour_min, colour_max, colour_background, 
     *            colour_foreground, colour_black, colour_white, 
     *            colour_red, colour_green, colour_blue, colour_cyan,
     *            colour_magenta, colour_yellow, colour_orange,
     *            colour_dark_grey, colour_light_grey
       parameter (colour_min = 0)
       parameter (colour_max = 15)
       parameter (colour_background = 0)
       parameter (colour_foreground = 1)
       
       parameter (colour_black     = 0)
       parameter (colour_white     = 1)
       parameter (colour_red       = 2)
       parameter (colour_green     = 3)
       parameter (colour_blue      = 4)
       parameter (colour_cyan      = 5)
       parameter (colour_magenta   = 6)
       parameter (colour_yellow    = 7)
       parameter (colour_orange    = 8)
       parameter (colour_dark_grey = 14)
       parameter (colour_light_grey = 15)
      
       integer    len_colour_name
       parameter (len_colour_name = 10)
       character*(len_colour_name) colour_name(colour_min:colour_max)
       data colour_name / 'black', 'white',   'red',    'green', 'blue',
     *                    'cyan',  'magenta', 'yellow', 'orange', ' ',
     *                    ' ',     ' ',       ' ',      ' ',
     *                    'dark-grey', 'light-grey' /
       
C fonts
       integer     font_min, font_max, font_normal,
     *             font_roman, font_italic, font_script
       parameter  (font_min     = 1)
       parameter  (font_max     = 4)
       parameter  (font_normal  = 1)
       parameter  (font_roman   = 2)
       parameter  (font_italic  = 3)
       parameter  (font_script  = 4)

       integer    len_font_name
       parameter (len_font_name = 6)
       character*(len_font_name) font_name(font_max)
       data       font_name / 'normal', 'roman', 'italic', 'script' /

C fill style
       integer    fill_min, fill_max, fill_solid, fill_hollow
       parameter (fill_min    = 1)
       parameter (fill_max    = 2)
       parameter (fill_solid  = 1)
       parameter (fill_hollow = 2)

C object types
       integer    object_min, object_max, object_none,
     *            object_line, object_arc, object_circle, 
     *            object_ellipse, object_box, object_erase, 
     *            object_cross, object_text, object_arrow, object_comp
       parameter (object_min     = 0)
       parameter (object_max     = 10)
       parameter (object_none    = 0)
       parameter (object_line    = 1)
       parameter (object_arc     = 2)
       parameter (object_circle  = 3)
       parameter (object_ellipse = 4)
       parameter (object_box     = 5)
       parameter (object_erase   = 6)
       parameter (object_cross   = 7)
       parameter (object_text    = 8)
       parameter (object_arrow   = 9)
       parameter (object_comp    = 10)

       integer    len_object_name
       parameter (len_object_name = 7)
       character*(len_object_name) object_name(object_min:object_max)
       data      object_name / 'none   ','line   ','arc    ','circle ',
     *                         'ellipse','box    ','erase  ','cross  ',
     *                         'text   ','arrow  ','comp   '/

C graphic types
       integer    graphic_min, graphic_max, graphic_none,
     *            graphic_image, graphic_draw, graphic_graph, 
     *            graphic_scratch
       parameter (graphic_min      = 0)
       parameter (graphic_max      = 4) 
       parameter (graphic_none     = 0)
       parameter (graphic_image    = 1)
       parameter (graphic_draw     = 2)
       parameter (graphic_graph    = 3)
       parameter (graphic_scratch  = 4)
       integer    len_graphic_name
       parameter (len_graphic_name = 7)
       character*(len_graphic_name) graphic_name(0:graphic_max)
       data       graphic_name / 'none   ','image  ','draw   ',
     *                           'graph  ','scratch' /


C define command strings
       character*60 graphic_command_plot(graphic_max)
       data graphic_command_plot /
     *      'map-display plot all',
     *      'drawing-display plot all',
     *      'data-display plot all',
     *      'scratch-display plot all' /
       character*60 graphic_command_refresh(graphic_max)
       data graphic_command_refresh /
     *      'map-display plot refresh',
     *      'drawing-display plot refresh',
     *      'data-display plot refresh',
     *      'scratch-display plot refresh' /
       character*60 graphic_command_init(graphic_max)
       data graphic_command_init /
     *      'default-setup map-display',
     *      'default-setup drawing-display',
     *      'default-setup data-display ',
     *      'default-setup scratch-display' /







