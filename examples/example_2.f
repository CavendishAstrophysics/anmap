C Example-Prog-1:  Scale a map by a user-supplied value
C
C
C The map is scaled such that:
C
C   "output-map"  =  "scale-factor" * ("input-map"  -  "offset-level")
C
C where scale-factor and offset-level are supplied by the user.
C
C This example program performs the same function as scale-map in
C the map-analysis sub-system of ANMAP.
C
C Document origin and update of this routine
C   P. Alexander, MRAO, Cambridge, 09/04/92
C
*-

C define work array
       integer       nm, nb
       integer        msize, bsize
       parameter    (msize = 256*256)
       parameter    (bsize=1024)
       parameter    (nm=16)
       parameter    (nb=3)
       real*4        map_array( nm*msize + nb*bsize )
C error status
       integer       status

C pointers and map identifiers
       integer       imap1, ip_map1, imap2, ip_map2,
     *               imap3, ip_map3, imapo, ip_mapo
C mini redtape for map size etc.
       integer       minirt(8)

C perform standard initialization
       call anm_start( 0,nm,msize,nb,bsize,status )

C read input maps:
       call map_getmap('Map-1 : ','Default_Map','READ',imap1,status)
       call map_alloc_in(imap1,'DIRECT',map_array,ip_map1,status)
       call map_getmap('Map-2 : ','Default_Map','READ',imap2,status)
       call map_alloc_in(imap2,'DIRECT',map_array,ip_map2,status)
       call map_getmap('Map-3 : ','Default_Map','READ',imap3,status)
       call map_alloc_in(imap3,'DIRECT',map_array,ip_map3,status)
       call map_alloc_out(0,0,'DIRECT',imapo,ip_mapo,status)
       if (status.ne.0) goto 999

C find the size of input maps
       call enminirt(minirt,status)

C do all the hard work of modelling the data
       call fit_model( minirt(5),minirt(6),
     *                 map_array(ip_map1), map_array(ip_map2),
     *                 map_array(ip_map3), map_array(ip_mapo),
     *                 status )

       call adredt('CREATED','MODEL',status)

999    call map_end_alloc(imapo,map_array,status)
       call map_end_alloc(imap1,map_array,status)
       call map_end_alloc(imap2,map_array,status)
       call map_end_alloc(imap3,map_array,status)

C check status value and report an error to the user
       call cmd_err(status,'MODEL','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end
C
C
*+ fit_model

       subroutine fit_model( nx, ny, im1, im2, im3, imout, status)
C      -----------------------------------------------------------
C
C Example program for passing images via calling sequenc
C
C Given:
C  image dimensions
       integer  nx, ny
C  input images
       real*4   im1(nx,ny), im2(nx,ny), im3(nx,ny)
C Returned:
C  output image
       real*4   imout(nx,ny)
C Updated:
C  error code
       integer  status
C
C This is a model routine which uses a number of images passed to it
C by the calling program and performs all analysis internal to this
C routine.
C
C P. Alexander MRAO, Cambridge.
*-

C local vriables


C check status on entry to the routine
       if (status.ne.0) return

C etc. ...

C if the data modelling has failed then indicate this by an error code
       status = -20001

       end
