C
C
*+ mapcat_mapenq

       subroutine mapcat_mapenq(irec,status)
C      -------------------------------------
C
C Open the physical file for a catalogue entry
C
C Input:
C    map-catalogue record
       integer             irec(*)
C    Status
       integer             status
C
C Open the physical file associated with map IMAP. The unit number on
C which the map is opened is returned in IUNIT.
C
*-
C include definitions to the status word entries
       include '/mrao/include/chrlib_functions.inc'
       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/iolib_constants.inc'

C local variables
       integer        n, ip, iu, idate(3)
       integer        srec(cat_blocksize/4)
       character      file*128
       real*8         ramap, decmap, refdat, obsdat

C check status on entry
       if (status.ne.0) return

C save current record and copy data passed to routine
       do n=1,cat_blocksize/4
         srec(n) = current_record(n)
       enddo
       do n=1,cat_blocksize/4
         current_record(n) = irec(n)
       enddo

       ip = 0
       file = current_filename(1:chr_lenb(current_filename))
       call opemap(iu,file,'READ',ip,status)
       call rdredt(iu,ip,status)
       call enminirt(current_minirt,status)
       call enmapc(ramap,decmap,refdat,obsdat,current_source,status)
       call enhist(idate,current_program,status)
       close (iu)

C copy results to calling routine
       current_filename=
     *     current_filename(1:chr_lenb(current_filename))//char(0)
       current_source=
     *     current_source(1:chr_lenb(current_source))//char(0)
       current_program=
     *     current_program(1:chr_lenb(current_program))//char(0)
       do n=1,cat_blocksize/4
         irec(n) = current_record(n)
       enddo

C tidy up and report any errors
       do n=1,cat_blocksize/4
         current_record(n) = srec(n)
       enddo
999    call mapcat_err(status,'mapcat_mapenq',
     *                 'Failed to open map')

       end
