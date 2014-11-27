C Example-Prog-0:  Initialise Anmap Package and call user routine
C
C
C Document origin and update of this routine
C   P. Alexander, MRAO, Cambridge, 09/04/92
C
*-

C define work array
       integer       nm, nb
       integer       msize, bsize
       parameter    (msize = 256*256)
       parameter    (bsize=1024)
       parameter    (nm=4)
       parameter    (nb=1)
       real*4        map_array( nm*msize + nb*bsize )
C error status
       integer       status

C perform standard initialization
       call anm_start( 0,nm,msize,nb,bsize,status )

C call user routine
       call user_routine( map_array, status )

C check status value and report an error to the user
       call cmd_err(status,'Example-0','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end

