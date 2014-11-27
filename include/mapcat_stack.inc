**
* Map/Image Catalofgue Stack Control
* ==================================
*
C  P. Alexander  MRAO
C
C Define standard parameters and sizes
       integer     max_number_stack_entries
       parameter  (max_number_stack_entries = 32)
       integer     max_active_maps
       parameter  (max_active_maps = 10)
       integer     number_map_entries
       integer     number_redtape_entries
       integer     number_work_entries
       integer     number_stack_entries
       integer     number_map_buffers
       parameter  (number_redtape_entries = 8)
       parameter  (number_work_entries = 0)
       integer     size_map_buffer
       integer     size_map_entry
       integer     size_redtape_entry
       parameter  (size_redtape_entry = 1024)
       integer     size_work_entry
C
C Define and place in common redtape information and map array sizes
       integer     current_redtape
       integer     size_redtape_array
       parameter  (size_redtape_array =
     *                  number_redtape_entries*size_redtape_entry+
     *                  max_active_maps*512)
       real*4      redt_array(size_redtape_array)
       include '/mrao/anmap/include/mapcat_recdefn.inc'
       common /mapcat_redtape/ redt_array, current_record,
     *                         current_redtape
       common /mapcat_numbers/ number_map_entries,
     *                         number_stack_entries,
     *                         number_map_buffers
       common /mapcat_sizes/   size_map_entry,
     *                         size_work_entry,
     *                         size_map_buffer
C
C
C Define stack status word as follows
C
C   Entry:   Meaning:          Values:
C   1        with-data;        0=no, 1=yes
C   2        access-state;     1=read, 2=write, 3=scratch
C   3        allocated;        0=no, 1=yes
C   4        type;             1=map, 2=redtape, 3=work
C   5        pointer;
C   6        map-associated;
C   7        interlock;        stack-entry
C   8        size;
C
C Define Active map list as follows:
C
C   Entry:   Meaning:          Values:
C   1        map-associated;   map catalogue number
C   2        access mode;      1=direct 2=sequential
C   3        unit number;      open unit number
C   4        data pointer;
C   5        redtape pointer;
C   6        allocated buffer;
C
       integer     length_stack_status
       parameter  (length_stack_status = 8)
       integer     stack_status (length_stack_status,
     *                           max_number_stack_entries)
       integer     length_active_list
       parameter  (length_active_list = 6)
       integer     active_list (length_active_list,
     *                          max_active_maps)
       integer     buffer_status(max_active_maps)
       real*4      active_zmnx(2,max_active_maps)
       integer     active_izmnx(2,max_active_maps)
C
       common /mapcat_control/ stack_status, active_list, buffer_status,
     *                         active_zmnx, active_izmnx
C
C Define parameters
       include '/mrao/anmap/include/mapcat_pars.inc'
