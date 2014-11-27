C
C
*+ map_alloc_new

       subroutine map_alloc_new(sizeu,sizev,imap,filename,status)
C      ----------------------------------------------------------
C
C Allocate and create output map, but do not provide access to it
C
C Given:
C   Size of output map in U and V
       integer        sizeu, sizev
C Returned:
C   Map catalogue entry
       integer        imap
C   File name associated with this map
       character*(*)  filename
C   Status word
       integer        status
C
C An output map for a routine is created and an entry added to the
C map catalogue.  No space is allocated in the stack and no pointer
C to the map is returned, the map file name is returned by this
C routine.   The primary use of this routine is to create a map
C so that it may be accessed by an OFFLINE process.
C
C SIZEU, SIZEV are the sizes of the image in the U and V direction.  If
C either is zero then the values are obtained from the current redtape.
C
C On completion a call to MAP_END_ALLOC is required to tidy the output
C and update the redtape as far as possible.
C
C PA, 28/3/91
*-
       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer     minirt(8)
       real*4      zmnx(2)
       integer     izmnx(4)

C test STATUS on entry
       if (status.ne.0) return

C find suitable output map and allocate
       call mapcat_next(imap,status)
       call mapcat_acc(imap,'CREATE',status)
       if (status.ne.0) then
         call mapcat_err(status,'map_alloc_new',
     *                   'Allocation of new map failed')
         goto 999
       end if

C set size if not specified
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
       call mapcat_enqrt(imap,filename,minirt,status)

999    call mapcat_err(status,'map_alloc_new','Map creation failed')

       end


