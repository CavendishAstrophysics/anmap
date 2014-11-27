C
C Spectrum analysis package data block
C
       real*4     spec_data( max_ncols, max_ndata, max_buff )
       integer    spec_hdi( max_standard_info, max_buff )
       character*(len_item_name+len_item_data)
     *            spec_hdc( max_items, max_buff )
C
       common /spec_data_block1/ spec_data
       common /spec_data_block2/ spec_hdi
       common /spec_data_block3/ spec_hdc
