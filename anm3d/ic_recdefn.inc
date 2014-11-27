
*
** Definition of the map/image catalogue record
*  ============================================
*
C Length of map status word
       integer     length_map_status
       parameter  (length_map_status = 6)
C Catalogue file block size
       integer     cat_blocksize
       parameter  (cat_blocksize = 44*4+length_map_status*4)
C
C Define record
       integer     current_record(cat_blocksize/4)
C number of entry
       integer     current_map
       equivalence(current_map,current_record(1))
C filename
       character   current_filename*96
       equivalence(current_filename,current_record(2))
C source
       character   current_source*24
       equivalence(current_source,current_record(26))
C program
       character   current_program*8
       equivalence(current_program,current_record(32))
C mini redtape
       integer     current_minirt(8)
       equivalence(current_minirt,current_record(36))
C map_status word
       integer     current_map_status(length_map_status)
       equivalence(current_map_status,current_record(44))
