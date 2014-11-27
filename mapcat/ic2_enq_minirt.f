*+ ic2_enq_minirt

       subroutine ic2_enq_minirt( minirt, status )
C      -------------------------------------------
C
C Return the minimal header information for the current image header
C
C Returned:
C   minimal header information
       integer    minirt(*)
C Updated:
C   error status
       integer    status
C
C The minimal header information for the current header is returned.  The
C header information is loaded using ic2_head_load.
C
C The minirt Has the following structure
C
C   minirt entry       meaning
C        1               u1                {  Image range
C        2               u2                {    u1 <= u  <= u2
C        3               v1                {    v1 >= v  >= v2
C        4               v2                {
C        5               nx                {  Image size
C        6               ny                {
C        7               data_type
C        8               blank_value
C
C You need to equivalence the final item to a real variable before use.
C 
C-
       call enminirt(minirt,status)
       end
