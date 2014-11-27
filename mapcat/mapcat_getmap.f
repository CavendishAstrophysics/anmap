C
C
*+ mapcat_getmap

       subroutine mapcat_getmap(prompt,default,access,imap,status)
C      -----------------------------------------------------------
C
C Prompt the user for a valid map identifier
C
C Given:
C   Prompt
       character*(*)      prompt
C   Default response
       character*(*)      default
C   Access required to map
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
       integer       lg, len_pr, len_pr1, len_st, len_def, len_text

       character     text*80, string*80, def*30, defg*30, map_text*80

       logical       special

C check status flag on entry
       special=.false.
       if (status.ne.0) return

C check default string for DEFAULT_MAP
       defg = default
       call chr_chucas(defg)
       lg = chr_lenb(defg)
       if (defg(1:lg).eq.'DEFAULT_MAP'.or.defg(1:lg).eq.'DEFAULT-MAP')
     *     THEN
         imap=default_map
         if (imap.lt.10) then
           write(string,'(''Default-map='',i1)')imap
         else if (imap.lt.100) then
           write(string,'(''Default-map='',i2)')imap
         else
           write(string,'(''Default-map='',i3)')imap
         end if
         special=.true.

       else if (defg(1:lg).eq.'NEXT_MAP'.or.defg(1:lg).eq.'NEXT-MAP')
     *     then
         call mapcat_next(imap,status)
         string = ' '
         if (imap.lt.10) then
           write(string,'(i1)') imap
         else if (imap.lt.100) then
           write(string,'(i2)') imap
         else
           write(string,'(i3)') imap
         end if
         special=.true.

       end if

C if special action is required then construct new prompt
       text=' '
       len_pr=len(prompt)

       if (special) then
         len_pr=len(prompt)
         len_pr1=chr_lenb(prompt)
         len_st=chr_lenb(string)
         text = ' '
         if (chr_chalfn(prompt(len_pr1:len_pr1))) then
           write(text,'(a,'' ['',a,''] '')')prompt,string(1:len_st)

         else
           write(text,'(a,'' ['',a,''] '',a)')prompt(1:len_pr1-1),
     *           string(1:len_st),prompt(len_pr1:len_pr)

         end if

         len_pr=len_pr+len_st+4
         def = ' '
         if (imap.lt.10) then
           write(def,'(i1)')imap
         else if (imap.lt.100) then
           write(def,'(i2)')imap
         else
           write(def,'(i3)')imap
         end if

       else
         text=prompt
         def=default

       end if

C Prompt
       len_def=chr_lenb(def)
       call io_getwrd(text(1:len_pr),def(1:len_def),
     *             map_text,len_text,status)
C find map entry
       call mapcat_fndmap(map_text(1:len_text),imap,status)
C check and return on error
       call mapcat_chk(imap,access,status)

       call cmd_err(status,'GET-MAP',
     *              'Error in obtaining map description')

       end
