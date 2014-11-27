**
*  Map-Stack Errors
*  ----------------
C
C
C Error file for the stack system
C
       integer    ill_nxtmap
       parameter (ill_nxtmap  =-2010) ! Unable to find catalogue entry
       integer    ill_stkalloc
       parameter (ill_stkalloc=-2015) ! Error allocating stack space
       integer    ill_stkacc
       parameter (ill_stkacc  =-2017) ! Stack already allocated
       integer    ill_stkdat
       parameter (ill_stkdat  =-2019) ! No data in stack entry
       integer    ill_mapout
       parameter (ill_mapout  =-2020) ! No output map
       integer    ill_mapamb
       parameter (ill_mapamb  =-2022) ! Ambiguous map/source name
       integer    ill_mapno
       parameter (ill_mapno   =-2024) ! Map/source not found
       integer    ill_mapopen
       parameter (ill_mapopen =-2025) ! Map file already open
       integer    ill_catacc
       parameter (ill_catacc  =-2027) ! Illegal catalogue access type
       integer    ill_catent
       parameter (ill_catent  =-2030) ! Illegal map catalogue entry
       integer    ill_catuse
       parameter (ill_catuse  =-2035) ! Map already allocated
       integer    ill_catwrt
       parameter (ill_catwrt  =-2040) ! Map not available for write
       integer    ill_catrd
       parameter (ill_catrd   =-2045) ! Map not available for read
       integer    ill_catope
       parameter (ill_catope  =-2050) ! Catalogue file not open
       integer    ill_stkent
       parameter (ill_stkent  =-2055) ! Illegal Stack entry
