C Extract-Column
C   extract a column from an image
C
C Document origin and update of this routine
C   P. Alexander, MRAO, Cambridge, 1/11/93
C
*-

C define work array
       integer       nm, nb
       integer       msize, bsize
       parameter    (msize = 256*256)
       parameter    (bsize=1024)
       parameter    (nm=16)
       parameter    (nb=3)
       real*4        map_array( nm*msize + nb*bsize )
C error status
       integer       status

C perform standard initialization
       call anm_start( 0,nm,msize,nb,bsize,status )

C read input map: imap is the catalogue identifier
       call do_extract_column(map_array,status)

C check status value and report an error to the user
       call cmd_err(status,'EXTRACT-COLUMN','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end
C
C
*+ do_extract_column

       subroutine do_extract_column(map_array,status)
C      ----------------------------------------------
C
C Extract a specifiewd column from the image
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C A specified column is extracted from the image and the results written
C to the standard results file.
*-

       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'

C Local variables:
C   mini redtape
       integer    minirt(8)
C   input map and data pointer
       integer    imap, ip_map
C   results output variables
       integer    iout, i, iu, iv, iuv(2)
       equivalence (iuv(1),iu)
       equivalence (iuv(2),iv)
C   results arrays
       real*4       x(1024), y(1024)

C check status
       if (status.ne.0) return

C read in the map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       if (status.ne.0) then
         call cmd_err(status,'EXTRACT-COLUMN',' ')
         goto 999
       end if
       call enminirt(minirt,status)

C find user defined parameters
       call io_geti('Column to extract (U) : ','0',iu,status)

C do the work
       i = 0
       do iv=minirt(3),minirt(4),-1
         i = i + 1
         x(i) = iv
         call iuvval2(minirt,map_array(ip_map),iuv,y(i),status)
       enddo
C
C write this out to the results file
       call io_opefil(iout,general_results_file,'WRITE',0,status)
       if (status.ne.0) then
         call cmd_err(status,'EXTRACT-COLUMN',
     *                 'Error accessing results file')
         goto 999
       end if
       write (iout,*)'%ndata ',minirt(6)
       write (iout,*)'%ncols 2'
       write (iout,*)'%title Extract-Column ',iu
       do i = 1, minirt(5)
         write (iout,*) x(i), y(i)
       end do
       close (iout)

999    call map_end_alloc(imap,map_array,status)
       end



