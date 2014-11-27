C
C
*+ map_getmap

       subroutine map_getmap(prompt,default,access,imap,status)
C      --------------------------------------------------------
C
C Prompt the user for a valid map identifier
C
C Given:
C   Prompt
       character*(*)      prompt
C   Default response
       character*(*)      default
C   Access required to map -- READ, WRITE
       character*(*)      access
C Returned:
C   Map entry
       integer            imap
C   Status word
       integer            status
C
C The user is prompted for a map. A default value may be specified and
C DEFAULT_MAP may be given as the default string. If the value
C given is not a valid map then the routine returns a non-zero value of
C STATUS.
C
*-
       include 'mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'
C

       if (status.ne.0) return
       call mapcat_getmap(prompt,default,access,imap,status)

C load redtape if access reuired is not NONE
       if (.not.chr_cmatch(access,'NONE')) then
         call redt_load(imap,status)
       end if

999    call mapcat_err(status,'map_getmap','Error reading map entry')

       end
