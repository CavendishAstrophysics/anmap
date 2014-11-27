C Segment-flux
C   calculate average flux in annuli bounded by a circular segment
C
C Document origin and update of this routine
C   P. Alexander, MRAO, Cambridge, 1/11/93
C
*-

C define work array
       integer       nm, nb
       integer        msize, bsize
       parameter    (msize = 1024*1024)
       parameter    (bsize=4096)
       parameter    (nm=16)
       parameter    (nb=3)
       real*4        map_array( nm*msize + nb*bsize )
C error status
       integer       status

C perform standard initialization
       call anm_start( 0,nm,msize,nb,bsize,status )

C read input map: imap is the catalogue identifier
       call do_inmap(map_array,status)

C check status value and report an error to the user
       call cmd_err(status,'io_data','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end
