** CLEAN Redtape Record Definition
*  -------------------------------
C
C Version 1.0  12/12/89
C
C P. Alexander MRAO
C
C Define Parameters:
C   maximum number of clean iteration
       integer            max_cc_iter
       parameter         (max_cc_iter = 50000)
C Define Record:
       integer            clean_record(128)
       character*8        clean_header
       integer            clean_time_stamp
       equivalence       (clean_record(1), clean_header)
       equivalence       (clean_record(3), clean_time_stamp)
       character*8        clean_text_header
       parameter         (clean_text_header = 'CLEAN-01')

C Variable definitions
C --------------------
C
C    window on map
       integer            clean_window(4)
       equivalence       (clean_window(1), clean_record(4))
C    window on beam
       integer            beam_window(4)
       equivalence       (beam_window(1), clean_record(8))
C    not-box window
       integer            box_window(4)
       equivalence       (box_window(1), clean_record(12))
C    search windows
       integer            search_window(4,9)
       equivalence       (search_window(1,1), clean_record(16))
C    number of search windows
       integer            number_search
       equivalence       (number_search, clean_record(52))
C    fraction to remove
       real*4             fract
       equivalence       (fract, clean_record(53))
C    limiting flux
       real*4             fllim
       equivalence       (fllim, clean_record(54))
C    iteration limit
       integer            itlim
       equivalence       (itlim, clean_record(55))
C    size of beam in U (arcsec)
       real*4             beam_size_u
       equivalence       (beam_size_u, clean_record(56))
C    size of beam in V (arcsec)
       real*4             beam_size_v
       equivalence       (beam_size_v, clean_record(57))
C    position angle of clean beam
       real*4             beam_pa
       equivalence       (beam_pa, clean_record(58))
C    prussian hat size
       real*4             prhat
       equivalence       (prhat, clean_record(59))
C
C Clean history
C -------------
C
C    number of calls to clean
       integer            times_cleaned
       equivalence       (times_cleaned, clean_record(60))
C    number of items in cc list
       integer            number_cc
       equivalence       (number_cc, clean_record(61))
C    number of iterations completed
       integer            number_it_done
       equivalence       (number_it_done, clean_record(62))
C    maximum on residual map
       real*4             max_on_resid
       equivalence       (max_on_resid, clean_record(63))
C    code indicating manner of CLEAN completion:
C                          <0 - abnormal [=STATUS]
C                           1 - iteration limit
C                           2 - flux limit
C                           3 - first zero
C                           4 - user intervention
       integer            how_ended
       equivalence       (how_ended, clean_record(64))
C
C Options
C -------
C
C    not box
       logical            lnotbx
       equivalence       (lnotbx, clean_record(65))
C    prussian hat
       logical            lprhat
       equivalence       (lprhat, clean_record(66))
C    stop at first negative clean component
       logical            lstop0
       equivalence       (lstop0, clean_record(67))
C    truncated beam for restore
       logical            ltrbm
       equivalence       (ltrbm, clean_record(68))
C    restore with original beam
       logical            loldbm
       equivalence       (loldbm, clean_record(69))
C    flag to force cleaning
       logical            lclean
       equivalence       (lclean, clean_record(70))
C    flag to force restore
       logical            lrest
       equivalence       (lrest, clean_record(71))
C
C
C Sort Mode Options
C -----------------
C
C    sort option on/off
       logical            lsort
       equivalence       (lsort, clean_record(72))
C    depth to which sort list is found
       integer            sort_depth
       equivalence       (sort_depth, clean_record(73))
C    number of iterations before sort list is recalculated
       integer            sort_inner
       equivalence       (sort_inner, clean_record(74))
C    gate for sorting option
       real*4             sort_gate
       equivalence       (sort_gate, clean_record(75))
C
C
C File handling and Global Variables
C ----------------------------------
C
C    generic name for files
       character*40       generic_name
       equivalence       (generic_name, clean_record(76))
C    specific beam name
       character*60       beam_name
       equivalence       (beam_name, clean_record(86))
C    specific map name
       character*60       map_name
       equivalence       (map_name, clean_record(101))
C    catalogue entry holding map
       integer            map_cat
       equivalence       (map_cat, clean_record(121))
C    catalogue entry holding beam
       integer            beam_cat
       equivalence       (beam_cat, clean_record(122))
C    catalogue entry holding residual map
       integer            residual_cat
       equivalence       (residual_cat, clean_record(123))
C    catalogue entry holding CLEANed map
       integer            clean_cat
       equivalence       (clean_cat, clean_record(124))
C    catalogue entry holding clean components map
       integer            cc_cat
       equivalence       (cc_cat, clean_record(125))
C
C Place this record in COMMON
C ---------------------------
C
       common /clean_common_record/ clean_record
C
C Other parameters and buffers held in common
C -------------------------------------------
C
C redtape array buffers for the map and beam redtapes of input maps
       integer     redt_map(1024), redt_beam(1024)
C definition flag set if map has been defined interactively
       logical     map_defined
C
       common /clean_save/ redt_map,redt_beam
       common /clean_map_defined/ map_defined
C
