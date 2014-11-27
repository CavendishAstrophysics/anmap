**
*  Map-Stack Errors
*  ----------------
C
C
C Error file for the stack system
C
       integer    ill_active
       parameter (ill_active  =-7002) ! Map not in active state
       integer    ill_buffers
       parameter (ill_buffers =-7004) ! No more sequential map buffers
       integer    ill_unmode
       parameter (ill_unmode  =-7006) ! Unknown access mode
       integer    ill_mode
       parameter (ill_mode    =-7008) ! Illegal access mode
       integer    ill_nxtmap
       parameter (ill_nxtmap  =-7010) ! Unable to find catalogue entry
       integer    ill_stkalloc
       parameter (ill_stkalloc=-7015) ! Error allocating stack space
       integer    ill_realloc
       parameter (ill_realloc =-7016) ! Unable to allocate to paged map
       integer    ill_stkacc
       parameter (ill_stkacc  =-7017) ! Stack already allocated
       integer    ill_stktyp
       parameter (ill_stktyp  =-7018) ! Unknown data type
       integer    ill_stkdat
       parameter (ill_stkdat  =-7019) ! No data in stack entry
       integer    ill_mapout
       parameter (ill_mapout  =-7020) ! No output map
       integer    ill_mapamb
       parameter (ill_mapamb  =-7022) ! Ambiguous map/source name
       integer    ill_mapno
       parameter (ill_mapno   =-7024) ! Map/source not found
       integer    ill_mapopen
       parameter (ill_mapopen =-7025) ! Map file already open
       integer    ill_catacc
       parameter (ill_catacc  =-7027) ! Illegal catalogue access type
       integer    ill_catent
       parameter (ill_catent  =-7030) ! Illegal map catalogue entry
       integer    ill_catuse
       parameter (ill_catuse  =-7035) ! Map already allocated
       integer    ill_catwrt
       parameter (ill_catwrt  =-7040) ! Map not available for write
       integer    ill_catrd
       parameter (ill_catrd   =-7045) ! Map not available for read
       integer    ill_catope
       parameter (ill_catope  =-7050) ! Catalogue file not open
       integer    ill_stkent
       parameter (ill_stkent  =-7055) ! Illegal Stack entry
       integer    ill_hditem 
       parameter (ill_hditem  =-7060) ! Unknown header item
