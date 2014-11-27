C Parameter Values for Pointers and status word values
C
C Version number
       integer    version_prefix
       parameter (version_prefix = 50000)
       integer    version_number
       parameter (version_number = 50001)
C
C Structure of the ic_status record
       integer    P_data
       parameter (P_data   = 1)
       integer    P_access
       parameter (P_access = 2)
       integer    P_open
       parameter (P_open   = 3)
       integer    P_unit
       parameter (P_unit   = 4)
       integer    P_dtype
       parameter (P_dtype   = 5)
       integer    P_page
       parameter (P_page   = 6)
       integer    p_status
       parameter (p_status = 6)
C
C True/False flags
       integer    true
       parameter (true       = 1)
       integer    false
       parameter (false      = 0)
C
C Access modes
       integer    access_none
       parameter (access_none  = -1)
       integer    access_read
       parameter (access_read  = 1)
       integer    access_write
       parameter (access_write = 2)
       integer    access_scratch
       parameter (access_scratch = 3)
       integer    access_create
       parameter (access_create = 4)
       integer    access_clear
       parameter (access_clear = 0)
C
C Data types
       integer    dtype_prefix
       parameter (dtype_prefix = 60000)
       integer    dtype_max
       parameter (dtype_max = 3)
       integer    dtype_df
       parameter (dtype_df   = dtype_prefix + 1)
       integer    dtype_map
       parameter (dtype_map  = dtype_prefix + 2)
       integer    dtype_fits
       parameter (dtype_fits = dtype_prefix + 3)
C
C Sizes of standard image elements
       integer    size_header
       parameter (size_header = 2048)
C
C Actions to perform on data structures/files
       integer    action_read_header
       parameter (action_read_header   = 1001)
       integer    action_write_header
       parameter (action_write_header  = 1002)
       integer    action_copy_header
       parameter (action_copy_header   = 1003)
       integer    action_copy_text
       parameter (action_copy_text     = 1004)
       integer    action_read_data
       parameter (action_read_data     = 1005)
       integer    action_write_data
       parameter (action_write_data    = 1006)
       integer    action_read_control
       parameter (action_read_control  = 1007)
       integer    action_write_control
       parameter (action_write_control = 1008)
       integer    action_delete
       parameter (action_delete        = 1010)
       integer    action_create
       parameter (action_create        = 1011)

C
C basic size parameters

C size of defn record
       integer    len_defn
       parameter (len_defn = 100)

C number of open images
       integer    max_open
       parameter (max_open = 10)
C maximum number of catalogue entries
       integer     max_cat_entries
       parameter  (max_cat_entries = 256)

