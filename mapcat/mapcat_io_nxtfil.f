C
C
*+mapcat_io_nxtfil

       subroutine mapcat_io_nxtfil (name, file, source, count, status)
C      ------------------------------------------------------------
C
C  Finds the next map file matching a given filename.
C
C  Given:
C      NAME      char*(*)    input filename pattern
C      COUNT     integer     current count of matching files
C
C  Returned:
C      FILE      char*(*)    next matching filename
C      SOURCE    char*(*)    source name
C      COUNT     integer     current count of matching files
C      STATUS    integer     status value
C
C  Returns the next map file from a list of disc files matching NAME.
C  The found filename is returned in FILE, and the current count of
C  matching files in COUNT.  The STATUS value should be zero on entry,
C  and is normally unchanged.  If the list is exhausted, the returned
C  status will be non-zero (NO_FILE), and COUNT will contain the number
C  of files in the list.  The list will be re-initialised if the routine
C  is entered with a new value for the input file name, or with COUNT equal
C  to zero.  The file type must match one of the standard file types defining
C  a valid map-type file.
C
C  (DJT, 18 May 87)
C  (PA, 3/3/92)
*-
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'

       integer       maxt
       parameter    (maxt = 20)
       character*(*) name, file, source
       character     lsource*(iolen_file),
     *               dir*(iolen_dir)
       character*4   type_list(maxt), type
       integer       iobjx, count, status, it
       logical       test
       data  type_list / 'MAP ','CLN ',
     *                   'IMAP','ICLN',
     *                   'QMAP','QCLN',
     *                   'UMAP','UCLN',
     *                   'BEAM','BSET',
     *                   'MI  ','PERC',
     *                   'CHI ','RESI',
     *                   'CCMP','APER',
     *                   'COS ','SIN ',
     *                   'AMP ','PHI '
     *                 /

       if (status.ne.0) return

1      call io_nxtfil( name, file, iobjx, count, status )
       call io_brkfil( file, dir, lsource, type )
       source =
     *   lsource(1:chr_lenb(lsource))//'_'//type(1:chr_lenb(type))
       if (status.eq.0) then
         test = .false.
         call chr_chucas(type)
         do it=1,maxt
           test = test .or. type.eq.type_list(it)
         end do
         if (.not.test) goto 1
       endif

       end
