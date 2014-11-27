C
C Global definition file for spectyrum package
C
C Maximum number of internal buffers
       integer      max_buff
       parameter   (max_buff = 4)
C
C Header block information
C   number of standard information header
       integer      max_standard_info
       parameter   (max_standard_info  = 10)
C   number of character item arrays
       integer      max_items
       parameter   (max_items = 40)
C   length of item name and item data value
       integer      len_item_name
       parameter   (len_item_name = 20)
       integer      len_item_data
       parameter   (len_item_data = 80)
C
C Data block information
C   maximum number of data points and columns
       integer      max_ndata
       parameter   (max_ndata = 4096)
       integer      max_ncols
       parameter   (max_ncols = 16)
C
C Control block information
C   maximum number of entries in the control block
       integer      max_entries
       parameter   (max_entries = 6)
C
