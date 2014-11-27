C
C
*+ ic_catopen

       subroutine ic_catopen(status)
C      -----------------------------
C
C Open the catalogue file if required
C
C Returned:
C     Status
       integer      status
C
*-
       include '/mrao/include/iolib_constants.inc'
C
C Local variables
       character tmp_file*(iolen_file)
       integer   len_cf, i, pages_on_file
       logical   exists

       include '/mrao/include/chrlib_functions.inc'
       include 'ic_pars.inc'
       include 'ic_cat.inc'

       if (status.ne.0) return
       len_cf = chr_lenb(cat_file)
       call io_namfil( cat_file, tmp_file, 0, status )
       exists = status.eq.0
       status = 0
       if (.not.exists) then
         pages_on_file = max_cat_entries*cat_blocksize/(512.0*4.0)
         call io_crefil(cat_file(1:len_cf),pages_on_file,
     *                  .false.,1,status)
       end if
       if (cat_unit.eq.0) then
         call io_operan(cat_unit,cat_file(1:len_cf),
     *               'WC',cat_blocksize,0,status)
       end if
       if (.not.exists) then
         do i = 1, max_cat_entries
           current_filename = ' '
           current_source   = ' '
           current_program  = ' '
           current_map      = i
           current_map_status(p_data)   = false
           current_map_status(p_open)   = false
           current_map_status(p_unit)   = 0
           current_map_status(p_access) = access_clear
           call io_wrfile(cat_unit,i,current_record,
     *                 cat_blocksize/4,status)
         end do
         if (status.ne.0) goto 999
       end if

999    call ic_err(status,'ic_catopen','Fatal Catalogue Error')

       end
C
C
*+ ic_catclose

       subroutine ic_catclose(status)
C      -----------------------------
C
C Close the catalogue file
C
C Returned:
C     Status
       integer      status
C
*-
       include '/mrao/include/iolib_constants.inc'
C
       include '/mrao/include/chrlib_functions.inc'
       include 'ic_cat.inc'

       if (status.ne.0) return
       if (cat_unit.gt.0) then
         close (cat_unit)
         cat_unit = 0
       end if

       call ic_err(status,'ic_catclose','Fatal Catalogue Error')

       end
C
C
*+ ic_catread

       subroutine ic_catread(ce,irec,status)
C      -------------------------------------
C
C Read a record from the catalogue file
C
C Input:
C    Catalogue entry
       integer       ce
C Updated:
C    Catalogue record structure
       integer       irec(*)
C Returned:
C    Status
       integer       status
*-

       include '/mrao/include/chrlib_functions.inc'
       include 'ic_pars.inc'
       include 'ic_cat.inc'
       include 'ic_errors.inc'

       integer   n, words

       if (status.ne.0) return
       if (ce.le.0 .or. ce.gt.max_cat_entries) then
         status = ill_catent
       else
         if (cat_unit.eq.0) then
             status = ill_catope
         else
             words = cat_blocksize/4
             call io_rdfile(cat_unit,ce,current_record,
     *                      words,status)
         end if
       end if

       current_filename=
     *     current_filename(1:chr_lenb(current_filename))//char(0)
       current_source=
     *     current_source(1:chr_lenb(current_source))//char(0)
       current_program=
     *     current_program(1:chr_lenb(current_program))//char(0)

       do n=1,cat_blocksize/4
          irec(n) = current_record(n) 
       end do

       call ic_err(status,'ic_catread',' ')
       end
C
C
*+ ic_catwrite

       subroutine ic_catwrite(ce,irec,status)
C      --------------------------------------
C
C Read a record from the catalogue file
C
C Input:
C    Catalogue entry
       integer       ce
C Updated:
C    Catalogue record structure
       integer       irec(*)
C Returned:
C    Status
       integer       status

       include '/mrao/include/chrlib_functions.inc'
       include 'ic_pars.inc'
       include 'ic_cat.inc'
       include 'ic_errors.inc'

       integer   n, words

       if (status.ne.0) return
       if (ce.le.0 .or. ce.gt.max_cat_entries) then
         status = ill_catent
       else
         if (cat_unit.eq.0) then
           status = ill_catope
         else
           do n=1,cat_blocksize/4
              current_record(n) = irec(n)
           end do
           current_filename=
     *       current_filename(1:chr_lenb(current_filename))
           current_source=
     *       current_source(1:chr_lenb(current_source))
           current_program=
     *       current_program(1:chr_lenb(current_program))
           words = cat_blocksize/4
           call io_wrfile(cat_unit,ce,current_record,
     *                    words,status)
         end if
       end if
       call ic_err(status,'ic_catwrite',' ')
       end
C
C
*+ ic_catdef

       subroutine ic_catdef(mode,ce,status)
C      ------------------------------------
C
C Manage the default map
C
C Given:
C   Mode ( 0 = set 1 = enquire)
       integer         mode
C Returned:
C   Default map
       integer         ce
C   Status word
       integer         status
C
C The current value of the default map is returned
C
*-
       include 'ic_cat.inc'

       if (status.ne.0) return
       if (mode.eq.0) then
         ce =  default_map
       else
         default_map = ce
       endif
       call ic_err(status,'ic_catdef',' ')
       end
C
C
*+ ic_defdir

       subroutine ic_defdir(mode,dd,status)
C      ------------------------------------
C
C Set default map directory
C
C Given:
C   Mode ( 0 = set 1 = enquire)
       integer          mode
C Input:
C    default map directory
       character*128    dd
C Returned:
C    status word
       integer          status
C
C The default map directory is set or enquired.  A check is made that the
C directory exists.
C
C Warning this command will affect the destination of PAGED temporary
C maps.  It is essential that this command is used with great care.
C
*-
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'
       include 'ic_cat.inc'

       if (status.ne.0) return
       if (mode.eq.0) then
         call io_namfil(dd(1:chr_lenb(dd)),cat_defdir,0,status)
         if (status.ne.0) cat_defdir = ' '
       elseif (mode.eq.1) then
         dd = cat_defdir(1:chr_lenb(cat_defdir))//char(0)
       endif

       call ic_err(status,'ic_defdir',' ')

       end
C
C
*+ ic_catfile

       subroutine ic_catfile(mode,dd,status)
C      -------------------------------------
C
C Set default catalogue file
C
C Given:
C   Mode ( 0 = set 1 = enquire)
       integer          mode
C Input:
C    default catalogue file
       character*128    dd
C Returned:
C    status word
       integer          status
C
C The catalogue file is set or enquired.
C
C Warning this command will affect the destination of PAGED temporary
C maps.  It is essential that this command is used with great care.
C
*-
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'
       include 'ic_cat.inc'

       if (status.ne.0) return
       if (mode.eq.0) then
         cat_file = dd(1:chr_lenb(dd))
       elseif (mode.eq.1) then
         dd = cat_file(1:chr_lenb(cat_file))//char(0)
       endif

       call ic_err(status,'ic_catfile',' ')

       end
C
C
C
C
       subroutine ic_catlres( i, j, list )
C      -----------------------------------
C
C Manage the results buffer
C
C Given
C    mode
       integer    i
C    index
       integer    j
C    list of results
       integer    list(10)
C
C Manage the results list:
C
C   mode = 0              initialise
C   mode = 1              set latest result = j
C   mode = 2              return list of results
C
       integer    n
       integer    ilist(10)
       common /mapcat_lres/ ilist

       if (i.eq.0) then
          do n=1,10
            ilist(n) = 0
          enddo
       elseif (i.eq.1) then
          do n=10,2,-1
            ilist(n) = ilist(n-1)
          enddo
          ilist(1) = j
       elseif (i.eq.2) then
          do n=1,10
            list(n) = ilist(n)
          enddo
       endif

       end
C
C
*+ ic_mapenq

       subroutine ic_mapenq(irec,status)
C      ---------------------------------
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
       include '/mrao/include/iolib_constants.inc'
       include 'ic_cat.inc'
       include 'ic_errors.inc'

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
999    call ic_err(status,'ic_mapenq',
     *                 'Failed to open map')

       end
C
C
*+ ic_err

       subroutine ic_err(status,routine,message)
C      ----------------------------------------
C
C Report an error message
C
C Given:
C   status
       integer    status
C   name of the current routine
       character  routine*(*)
C   additional message text
       character  message*(*)
C
C The error message is written to the error device or the output
C device depending on the value of STATUS. If STATUS=0 the action
C is to return without an error messahe,if STATUS .ne. 0
C then the IOLIB routine io_wrerr is used to output a complete
C error message, and the routine name is appended for a trace-back.
*-
       integer    istat
       logical    error_file_set
       common    /ic_local_err/ error_file_set

       include   '/mrao/include/iolib_errors.inc'

       if (status.eq.iolib_ok) return

C set up local error messages if not done so already
       if (.not.error_file_set) then
         istat = 0
         call io_setmsg( '/mrao/anmap/ic_lib/new/ic_errors.inc', istat)
         error_file_set = .true.
       end if

C output any message
       call cmd_err( status, routine, message )

       end
