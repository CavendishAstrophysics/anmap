* Include file defining array of graphic structures: plt_graphic_array
* --------------------------------------------------
C
C define the array to hold structure
       integer    max_graphic_structures
       parameter (max_graphic_structures = 32)
       integer    graphic_array(len_struct,max_graphic_structures)

C the current graphic and its type
       integer   graphic_current, graphic_current_type

C the default definition to, use for each type
       integer   graphic_default_current(10)

       common /graphic_data_structure/ graphic_array,
     *         graphic_current, graphic_current_type,
     *         graphic_default_current
 
