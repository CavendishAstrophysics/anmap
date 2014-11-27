C
C
*+ ic2_getmap

       subroutine ic2_getmap(prompt,default,imap,status)
C      -------------------------------------------------
C
C Prompt the user for a valid map identifier
C
C Given:
C   Prompt
       character*(*)      prompt
C   Default response
       character*(*)      default
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
       include 'ic_pars.inc'
       call map_getmap(prompt,default,'READ',imap,status)
       call mapcat_err(status,'getmap',
     *                 'Error in obtaining map description')

       end
