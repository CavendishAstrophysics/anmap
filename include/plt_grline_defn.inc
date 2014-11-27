* Structure defining line-style object for graphs: plt_grline_defn.inc
* ------------------------------------------------

C assume plt_basic_defn.inc is already defined
C
C This structure defines the properties of line-styles to be used on
C graphs -- an additional pointer to a character table is used.
C
       integer      len_grline
       parameter   (len_grline = 26 + 2*len_line_opt + len_text_opt)
       integer      grline(len_grline)

C flag indicating status of this line
       integer      grline_status
       equivalence (grline_status, grline(1))
C symbol-marking option:
C   = 0    No symbol mark
C   > 0    Mark with symbols of type grline_symbol
       integer      grline_symbol
       equivalence (grline_symbol, grline(2))
C line display option
C   = 0    No line
C   = 1    Standard line of specified line-style
C   = 2    Use histogram-style line marking
       integer      grline_type
       equivalence (grline_type, grline(3))
C pointer to X/Y-axis data types -- points to character structure,
C    and code for column to use from file/data structure
       integer      grline_x_file, grline_x_col
       integer      grline_y_file, grline_y_col
C flags to indicate a function type for this line
       integer      grline_func_opt, grline_func_defn
C flags to indicate a list type for this line
       integer      grline_list_opt, grline_list_defn
       equivalence (grline_x_file, grline(4))
       equivalence (grline_y_file, grline(5))
       equivalence (grline_func_opt, grline(4))
       equivalence (grline_func_defn, grline(5))
       equivalence (grline_list_opt, grline(4))
       equivalence (grline_list_defn, grline(5))
       equivalence (grline_x_col, grline(6))
       equivalence (grline_y_col, grline(7))
C offset of this line in X and Y
       real*4       grline_offset_x, grline_offset_y
       equivalence (grline_offset_x, grline(8))
       equivalence (grline_offset_y, grline(9))
C scaling for this line in X and Y
       real*4       grline_scale_x, grline_scale_y
       equivalence (grline_scale_x, grline(10))
       equivalence (grline_scale_y, grline(11))
C auto-scaling option for this line
       logical      grline_auto_x, grline_auto_y
       equivalence (grline_auto_x, grline(12))
       equivalence (grline_auto_y, grline(13))

C option for plotting X/Y-type error bars and relevant pointers
C  = 0 no error bar
C  = 1 standard error bar
C  = 2 Upper limit error bar (if error > 0.0)
C  = 3 Lower limit error bar (if error > 0.0)
       integer      grline_ex_opt, grline_ey_opt
       integer      grline_ex_file, grline_ex_col
       integer      grline_ey_file, grline_ey_col
C size of error bar "top"
       real*4       grline_ex_top, grline_ey_top
       equivalence (grline_ex_opt, grline(14))
       equivalence (grline_ey_opt, grline(15))
       equivalence (grline_ex_file, grline(16))
       equivalence (grline_ey_file, grline(17))
       equivalence (grline_ex_col, grline(18))
       equivalence (grline_ey_col, grline(19))
       equivalence (grline_ex_top, grline(20))
       equivalence (grline_ey_top, grline(21))
C logarithmic style options
       logical      grline_x_log, grline_y_log
       equivalence (grline_x_log, grline(22))
       equivalence (grline_y_log, grline(23))

C line-style for the main line
       integer      grline_line_opt(len_line_opt)
       equivalence (grline_line_opt, grline(24))
C line-style for the error bars
       integer      grline_error_opt(len_line_opt)
       equivalence (grline_error_opt, grline(24+len_line_opt))
C text-style for symbol markers
       integer      grline_text_opt(len_text_opt)
       equivalence (grline_text_opt, grline(24+2*len_line_opt))
C options for adding information to the key 
C  = 0   No key entry
C  = 1   Use "file" and "column" information
C  = 2   Use supplied text (grline_key_text)
       integer      grline_key_opt
       integer      grline_key_text
       equivalence (grline_key_opt, 
     *              grline(24+2*len_line_opt+len_text_opt))
       equivalence (grline_key_text, 
     *              grline(25+2*len_line_opt+len_text_opt))
C
C Parameters specific to grline definitions
       integer      grline_func_defined
       parameter   (grline_func_defined = -100)
       integer      grline_list_defined
       parameter   (grline_list_defined = -200)

