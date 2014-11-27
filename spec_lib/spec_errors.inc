C
C Error definition file for the spectrum analysis routines
C
C Illegal number of data points or number not specified
       integer     ill_ndata
       parameter  (ill_ndata = -12001)
C Attempt to access un-allocated internal spectrum
       integer     ill_alloc
       parameter  (ill_alloc = -12002)
C Illegal attempt to access an internal spectrum for write
       integer     ill_alloc_write
       parameter  (ill_alloc_write = -12003)
C Illegal attempt to access an internal spectrum for read
       integer     ill_alloc_read
       parameter  (ill_alloc_read = -12004)
C Unable to allocate an internal spectrum (no more available)
       integer     ill_do_alloc
       parameter  (ill_do_alloc = -12005)
C Illegal column index or number
       integer     ill_column
       parameter  (ill_column = -12006)
C Illegal spectrum identifier
       integer     ill_id
       parameter  (ill_id = -12007)
C Illegal file access attempt
       integer     ill_file_access
       parameter  (ill_file_access = -12008)
