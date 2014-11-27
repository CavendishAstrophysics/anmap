C
C
*+ map_alloc_out

       subroutine map_alloc_out(usize,vsize,access,imap,ip_map,status)
C      ---------------------------------------------------------------
C
C Allocate output maps required in the remainder of a routine
C
C Given:
C   Size of output map in U and V
       integer        usize, vsize
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
C SIZEU, SIZEV are the sizes of the image in the U and V direction.  If
C either is zero then the values are obtained from the current redtape.
C
C On completion a call to MAP_END_ALLOC is required to tidy the output
C and indeed write out the image if the access requested is DIRECT.
C
C PA, 11/7/90
C PA, 15/9/92; bug in overwriting input parameters corrected, fails
C              with certain compilers
*-
       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer     is_map, minirt(10), iunit, ip_redt, mode
       integer     sizeu, sizev
       real*4      zmnx(2)
       integer     izmnx(4)

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

C set size if not specified
       sizeu = usize
       sizev = vsize
       call enminiRT(minirt,status)
       if (sizeu.le.0 .or. sizev.le.0) then
         sizeu = minirt(5)
         sizev = minirt(6)
       end if

C create output map
       call redt_setfile('PAGEING-FILE',status)
       if (sizev.ne.minirt(6)) then
         minirt(6) = sizev
         minirt(3) = sizev/2
         minirt(4) = -sizev+minirt(3)+1
       end if
       if (sizeu.ne.minirt(5)) then
         minirt(5) = sizeu
         minirt(1) = -sizeu/2
         minirt(2) = sizeu+minirt(1)-1
       end if
       call stredt(minirt,3,status)
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


