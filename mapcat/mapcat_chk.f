C
C
C
*+ mapcat_chk

       subroutine mapcat_chk(imap,access,status)
C      -----------------------------------------
C
C Check a catalogue entry for the specified status
C
C Input:
C    Map catalogue entry
       integer                  imap
C    Access required
       character*(*)            access
C Returned:
C    Status
       integer                  status
C
C The specified catalogue entry is checked for the requested access. An
C error is generated if the catalogue entry is invalid or not available
C for the access requested.  This routine generates an error message in
C addition to setting status on exit.
*-
       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (status.ne.0) return

       if (imap.le.0 .or. imap.gt.max_cat_entries) then
         status = ill_catent
         return
       end if
       call mapcat_read(imap,status)
       if ( (chr_cmatch(access,'READ') .or.
     *       chr_cmatch(access,'WRITE')) .and.
     *       status.eq.0                   ) then
         if (chr_cmatch(access,'WRITE') .and. status.eq.0) then
           if (current_map_status(ip_access).eq.access_read) then
             status = ill_catuse
           end if
           if (current_map_status(ip_data).eq.true) then
             status = ill_catwrt
           end if
         else if (chr_cmatch(access,'READ') .and. status.eq.0) then
           if (current_map_status(ip_access).eq.access_write) then
             status = ill_catuse
           end if
           if (current_map_status(ip_data).eq.false) then
             status = ill_catrd
           end if
         end if
       end if

       call mapcat_err(status,'MAP-CATALOGUE','Unable to access MAP')

       end
