C
C
*+ mapcat_acc

       subroutine mapcat_acc(imap,access,status)
C      -----------------------------------------
C
C Set the access state to a map catalogue entry
C
C Input:
C     Map catalogue entry
        integer            imap
C     Access
        character*(*)      access
C Returned:
C     Status
        integer            status
C
C Set the access state to the specified catalogue entry.  This ensures
C The catalogue entry is not opened more than once for read/write
C simultaneously.
*-

       integer   map_status(10)

       include 'mapcat_pars.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (status.ne.0) return
       call mapcat_enqst(imap,map_status,status)
       if (status.eq.0) then
         if (map_status(ip_access).eq.access_clear) then
            if (chr_cmatch(access,'READ')) then
              map_status(ip_access) = access_read
            else if (chr_cmatch(access,'WRITE')) then
              map_status(ip_access) = access_write
            else if (chr_cmatch(access,'SCRATCH')) then
              map_status(ip_access) = access_scratch
            else if (chr_cmatch(access,'CREATE')) then
              map_status(ip_access) = access_create
            else if (chr_cmatch(access,'CLEAR')) then
              map_status(ip_access) = access_clear
            else
              status = ill_catacc
            end if
         else if (chr_cmatch(access,'CLEAR')) then
            map_status(ip_access) = access_clear
         else
            status = ill_catuse
         end if
         call mapcat_setst(imap,map_status,status)
       end if
       call mapcat_err(status,'mapcat_acc','Access Failed')
       end
