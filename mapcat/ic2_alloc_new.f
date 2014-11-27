C
C
*+ ic2_alloc_new

       subroutine ic2_alloc_new(sizeu,sizev,imap,filename,status)
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
C On completion a call to IC2_END_ALLOC is required to tidy the output
C and update the redtape as far as possible.
C
*-
       include 'ic_pars.inc'
       call map_alloc_new(sizeu,sizev,imap,filename,status)
999    call mapcat_err(status,'ic2_alloc_new','Map creation failed')

       end


