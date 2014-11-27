C
C
*+ map_alloc_scr

       subroutine map_alloc_scr(sizeu,sizev,access,imap,ip_map,status)
C      ---------------------------------------------------------------
C
C Allocate scratch maps required in the remainder of a routine
C
C Given:
C   Size of scratch map in U and V
       integer        sizeu, sizev
C   Access mode required -- DIRECT
       character*(*)  access
C Returned:
C   Map catalogue entry
       integer        imap
C   Map pointer
       integer        ip_map
C   Status word
       integer        status
C
C Scratch maps for a routine are allocated.
C
C Two access modes are supported in general:
C    SEQUENTIAL -- line by line; the entire image is not needed at once
C    DIRECT     -- full image in core
C HOWEVER, only DIRECT access is sensible for scratch files and
C SEQUENTIAL mode is therefore faulted.
C
C If DIRECT access is specified then the pointer to the start of the
C allocated space is returned in ip_map, if the access mode is
C SEQUENTIAL calls to MAP_ROW_READ and MAP_ROW_WRITE will be required
C nested in the sequential access loop.
C
C SIZEU, SIZEV are the sizes of the image in the U and V direction.  If
C either is zero then the values are obtained from the current redtape.
C
C On completion a call to MAP_END_ALLOC is required to tidy access state.
C
C PA, 11/7/90
*-
       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer     is_map, minirt(10)

C test STATUS on entry
       if (status.ne.0) return

C find suitable output map and allocate
       call mapcat_next(imap,status)
       call mapcat_acc(imap,'SCRATCH',status)
       if (status.ne.0) then
         call mapcat_err(status,'map_alloc_out',
     *                   'Allocation of output maps failed')
         goto 999
       end if

C set size if not specified
       if (sizeu.le.0 .or. sizev.le.0) then
         call enminiRT(minirt,status)
         sizeu = minirt(5)
         sizev = minirt(6)
       end if

C allocate space as required
       if (chr_cmatch(access,'DIRECT')) then
C .. if access mode is DIRECT then find space
         call stack_access(imap,'SCRATCH','MAP',
     *                     sizeu*sizev,is_map,status)
         call stack_enqpnt(is_map,ip_map,status)
       else
         status = ill_mode
       end if

999    call mapcat_err(status,'map_alloc_scr','Space allocation failed')

       end
