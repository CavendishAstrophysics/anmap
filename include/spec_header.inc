C Spectrum header file definition
C
C define standard information needed for all spectra
       integer       standard_info(max_standard_info)
C .. number of data points
       integer       ndata
       equivalence  (ndata, standard_info(1))
C .. number of columns
       integer       ncols
       equivalence  (ncols, standard_info(2))
C .. number of items of additional info
       integer       nitems
       equivalence  (nitems, standard_info(3))
C
C define character items
       character*(len_item_name) item_name(max_items)
       character*(len_item_data) item_data(max_items)
