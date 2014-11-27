C Parameter Values for Pointers and status word values
       integer    ip_data
       parameter (ip_data       = 1)
       integer    ip_access
       parameter (ip_access     = 2)
       integer    ip_open
       parameter (ip_open       = 3)
       integer    ip_unit
       parameter (ip_unit       = 4)
       integer    ip_allocated
       parameter (ip_allocated  = 3)
       integer    ip_associated
       parameter (ip_associated = 4)
       integer    ip_tape
       parameter (ip_tape       = 5)
       integer    ip_page
       parameter (ip_page       = 6)
       integer    ip_interlock
       parameter (ip_interlock  = 5)
       integer    ip_type
       parameter (ip_type       = 6)
       integer    ip_pointer
       parameter (ip_pointer    = 7)
       integer    ip_size
       parameter (ip_size       = 8)
C
       integer    type_map
       parameter (type_map     = 2)
       integer    type_redtape
       parameter (type_redtape = 1)
       integer    type_work
       parameter (type_work    = 2)
       integer    type_unknown
       parameter (type_unknown = -1)
C
       integer    true
       parameter (true         = 1)
       integer    false
       parameter (false        = 0)
C
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
       integer    mode_direct
       parameter (mode_direct  = 1)
       integer    mode_sequential
       parameter (mode_sequential = 2)
C
