*$ Batch MODE CLEAN Program
*  ------------------------
C
C This is a stand alone program to interface to various ANMAP routines
*-
C define parameters to be read from the job queue
       integer         ifile
       character*80    file

       include '../include/clean_record.inc'
       include '../include/clean_sys_pars.inc'

C
C allocate work space
       integer         map_size
       parameter      (map_size = 1024)
       real*4          map(map_size,map_size)
       integer         beam_size
       parameter      (beam_size = 1024)
       real*4          beam(beam_size,beam_size)
       real*4          cc_values(max_cc_iter)
       integer*2       cc_posns(2,max_cc_iter)
       integer*2       cc_list(max_cc_iter)

C define work array
       integer       nm, nb
       integer       msize, bsize
       parameter    (msize = 1)
       parameter    (bsize=1)
       parameter    (nm=1)
       parameter    (nb=1)

C error status
       integer       status
 
C perform standard initialization
       call anm_start( 0,1,1,1,1,status )
 

C get map to clean and do the work
       call io_getwrd('Map to clean : ','NONE',file,ifile,status)
       call clean_image( file(1:ifile), map, beam,
     *                   cc_values, cc_posns, cc_list, status )

C finally perform standard shut-down
       call anm_end( status )
       call cmd_err(status,'CLEAN',' ')
       end
