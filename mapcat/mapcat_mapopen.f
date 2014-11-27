C
C
*+ mapcat_mapopen

       subroutine mapcat_mapopen(imap,access,iunit,status)
C      ---------------------------------------------------
C
C Open the physical file for a catalogue entry
C
C Input:
C    Map Catalogue entry
       integer             imap
C    Access
       character*(*)       access
C Returned:
C    Unit number
       integer             iunit
C    Status
       integer             status
C
C Open the physical file associated with map IMAP. The unit number on
C which the map is opened is returned in IUNIT.
C
*-
C include definitions to the status word entries
       include 'mapcat_pars.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/iolib_constants.inc'

C status word
       integer        map_status(10)
C redtape
       integer        minirt(10)
C local status
       integer        istat, iprint
C filename
       character      filename*(iolen_file)


C check status on entry
       if (status.ne.0) return

C check IMAP for validity
       call mapcat_chk(imap,'NONE',status)
       if (status.ne.0) goto 999

C read status word
       call mapcat_enqst(imap,map_status,status)

C check current status
       if (map_status(ip_open).eq.true) then
         iunit = map_status(ip_unit)
         return
       end if

C open map
       call mapcat_enqrt(imap,filename,minirt,status)
       iprint = 0
       call opemap(map_status(ip_unit),filename,
     *             access,iprint,status)
C update status word
       if (status.eq.0) then
         iunit = map_status(ip_unit)
         map_status(ip_open) = true
       else
         map_status(ip_open) = false
         map_status(ip_unit) = 0
       end if
       istat = 0
       call mapcat_setst(imap,map_status,istat)
       call mapcat_err(istat,'mapcat_mapopen',
     *                 'Error accessing Catalogue')

C error report
999    call mapcat_err(status,'mapcat_mapopen',
     *                 'Failed to open map file')

       end
