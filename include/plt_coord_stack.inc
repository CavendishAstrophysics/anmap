C Coordinate system stack : plt_coord_stack
C -----------------------------------------
C

C size of the stack
       integer     len_coord_stack
       parameter  (len_coord_stack = 5)

C define the stack
       real*4      coord_stack(8,len_coord_stack)

       common /coord_stack_buffer/ coord_stack

