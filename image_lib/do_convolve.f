C
C
*+ do_convolve

       subroutine do_convolve(map_array,status)
C      ----------------------------------------
C
C perform a generalised convolution of an image with a supplied function
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The input map is convolved with a specified digitised function.  This
C function must be supplied by the user -- the actual work is performed
C by an IMAGE routine.
C
*-

C Functions
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C Local variables
C   file name
       character  filename*(iolen_file)
C   mini redtape of input and output map
       integer    minirt(8)
C   pointers to input map, output map and work space
       integer    imap, ip_map, imapo, ip_mapo, iwork, ip_work
C   size and centre of convolving function, loop counter
       integer    nx, ny, ix, iy, n
C   unit number for convolving function file
       integer    iunit
C   null data value on input map
       real*4     null

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default_Map','read',imap,status)
       if (status.ne.0) goto 999
C read range for map
       call enminirt(minirt,status)
C force input map into core -- return pointer
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call ennull(null,status)
C pointer to output map
       call map_alloc_out(0,0,'DIRECT',imapo,ip_mapo,status)
C find work space
       call map_alloc_scr(64,64,'DIRECT',iwork,ip_work,status)
C read convolving array into work space
       call io_getfil('File for convolving function : ',
     *                ' ',filename,status)
       call io_opefil(iunit,filename(1:chr_lenb(filename)),
     *                'READ',2,status)
       if (status.ne.0) goto 999
       read (iunit,*) nx, ny, ix, iy
       read (iunit,*) (map_array(ip_work+n-1), n=1,nx*ny)
       close(iunit)

C do the convolution
       call image_convolve(minirt(5),minirt(6),map_array(ip_map),
     *                     nx,ny,ix,iy,map_array(ip_work),null,
     *                     map_array(ip_mapo),status)

C update the text of the redtape
       call adredt('CREATED','CONVOLVE',status)

C Tidy up, make output map new current map etc.
999    continue
       call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap,map_array,status)
       call map_end_alloc(iwork,map_array,status)

C check status value
       call cmd_err(status,'CONVOLVE','Failed ')
       end
