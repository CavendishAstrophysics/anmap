C
C
*+ map_alloc_out_s

       subroutine map_alloc_out_s(iuv,access,imap,ip_map,status)       
C      ---------------------------------------------------------------
C
C Allocate output maps required in the remainder of a routine
C
C Given:
C   Output map uv range (if all zero, values are taken from redtape)
       integer        iuv(4)
C   Access mode required -- DIRECT or SEQUENTIAL
       character*(*)  access
C Returned:
C   Map catalogue entry
       integer        imap
C   Map pointer
       integer        ip_map
C   Status word
       integer        status
C
C Output maps for a routine are allocated.
C
C Two access modes are supported:
C    SEQUENTIAL -- line by line; the entire image is not needed at once
C    DIRECT     -- full image in core
C
C If DIRECT access is specified then the pointer to the start of the
C allocated space is returned in ip_map, if the access mode is
C SEQUENTIAL calls to MAP_ROW_READ and MAP_ROW_WRITE will be required
C nested in the sequential access loop.
C
C SIZEU, SIZEV are the sizes of the image in the U and V direction. If        
C i/p IUV are zero then the values are obtained from current redtape.
C
C On completion a call to MAP_END_ALLOC is required to tidy the output
C and indeed write out the image if the access requested is DIRECT.
C
C PA, 11/7/90  (map_alloc_out)
C PA, 15/9/92; bug in overwriting input parameters corrected, fails
C              with certain compilers
C SEGH, 31/7/98: map_alloc_out_s to deal with general uvrange,
C                since map_alloc_out did not work correctly for
C                a map with offset centre whose size changed wrt I/P
*-
       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer     is_map, minirt(10), iunit, ip_redt, mode
       integer     sizeu, sizev
       real*4      zmnx(2)
       integer     izmnx(4)
C   If input iuv are all zero, assume uv range from current redtape
       logical     sameuv

C test STATUS on entry
       if (status.ne.0) return

C find suitable output map and allocate
       call mapcat_next(imap,status)
       call mapcat_acc(imap,'WRITE',status)
       if (status.ne.0) then
         call mapcat_err(status,'map_alloc_out',
     *                   'Allocation of output maps failed')
         goto 999
       end if

C set size if not specified ( uvrange all 0's means take from redtape)      
       sizeu = iuv(2)-iuv(1)+1
       sizev = iuv(3)-iuv(4)+1
       sameuv = .false.
       call enminiRT(minirt,status)
       if (iuv(1).eq.0.and.iuv(2).eq.0.and.iuv(3).eq.0.and.iuv(4).eq.0)
     *  then
         sameuv = .true.
         sizeu = minirt(5)
         sizev = minirt(6)
       end if

C create output map
       call redt_setfile('PAGEING-FILE',status)
       if (.not. sameuv) then
         minirt(1) = iuv(1)
         minirt(2) = iuv(2)
         minirt(3) = iuv(3)
         minirt(4) = iuv(4)
         minirt(5) = sizeu
         minirt(6) = sizev
       end if
C       call stredt(minirt,3,status)
       zmnx(1) =  1.0
       zmnx(2) = -1.0
       izmnx(1)= 0
       izmnx(2)= 0
       izmnx(3)= 0
       izmnx(4)= 0
       call stscal(zmnx,izmnx,status)
       minirt(7) = 3
       call redt_setrt(minirt,status)
       call redt_dump(imap,status)

C allocate space as required:
       if (chr_cmatch(access,'DIRECT').or.
     *     chr_cmatch(access,'SEQUENTIAL')) then
C .. if access mode is DIRECT then find space
         call stack_access(imap,'WRITE','MAP',sizeu*sizev,is_map,status)
         call stack_enqpnt(is_map,ip_map,status)
         call stack_enqmode(imap,mode,ip_map,ip_redt,iunit,status)
         call stack_dpredt(ip_redt,status)
       else
         status = ill_unmode
         goto 999
       end if
*       print *,'.. alloc_out :: ',imap, is_map, ip_map, ip_redt
999    call mapcat_err(status,'map_alloc_out','Space allocation failed')

       end


