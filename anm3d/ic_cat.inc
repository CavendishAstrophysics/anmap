**
* Map/Image Catalogue Include File
* ================================
C
C
C Define standard parameters

C default directory
       character    cat_defdir*128
C catalogue file name
       character    cat_file*128
C default map
       integer      default_map
C catalogue file unit number
       integer      cat_unit
C maximum allowed map area for a read_area allocation
       real*4       area_max_read
C
C Place variables in common
       common /icc_vals/ cat_unit, default_map, area_max_read
       common /icc_file/ cat_file, cat_defdir
C
C
       include 'ic_recdefn.inc'