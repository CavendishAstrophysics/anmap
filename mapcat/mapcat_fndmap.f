C
C
*+ mapcat_fndmap

       subroutine mapcat_fndmap(map,imap,status)
C      -----------------------------------------
C
C Interprete the text MAP as a catalogue entry or disc file
C
C Input:
C    Map catalalogue identifier
       character*(*)       map
C Returned:
C    Map catalogue entry
       integer             imap
C    Status
       integer             status
C
C The text string MAP is scanned and interpreted either as a catalogue
C entry, as a source name or as a disc file.  If the string is a map
C name then it is added to the catalogue unless already present.  IMAP
C is returned in all cases as the catalogue entry of the specified map.
*-

       include 'mapcat_cat.inc'
       include 'mapcat_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/iolib_functions.inc'

       integer    len_map, istat, count
       integer    i
       character  source*30, search*80, string*80
       integer    len_source, len_program, len_search
       logical    selected

C check status on entry
       if (status.ne.0) return
       len_map = chr_lenb(map)
       if (operating_system.ne.'UNIX') then
         call chr_chucas(map)
       end if

C check for stack entry
       istat = 0
       call chr_chctoi(map(1:len_map),imap,istat)
       if (istat.eq.0) then
         if (imap.gt.0 .and. imap.le.max_cat_entries) then
           return
         end if
       end if

C look for characters only associtaed with a file specification
       if (chr_chkchr(file_char_type,map(1:len_map))) goto 20
       if (chr_chkchr(file_char_version,map(1:len_map))) goto 20
       if (chr_chkchr(dir_char_right,map(1:len_map))) goto 20

C look for match to source names
       call mapcat_open(status)
       count = 0
       i = 0
       selected = .false.
       do while (i.lt.max_cat_entries .and. .not.selected)
         i = i + 1
         call mapcat_read(i,status)
         len_source  = chr_lenb(current_source)
         len_program = chr_lenb(current_program)
         search = current_source(1:len_source)//'-'//current_program
         call chr_chucas(search)
         len_search = chr_lenb(search)
         if (current_map_status(ip_data).eq.false) goto 10
         if (chr_cmatch(map(1:len_map),search(1:len_search))) then
           count = count + 1
           imap = i
           if (count.eq.1) then
             source = current_source
           end if
           if (count.eq.2) then
             string = 'Select '//source(1:chr_lenb(source))//' ? '
             selected = io_yesno(string(1:chr_lenb(string)),
     *                  'yes',status)
             if (.not.selected) then
               string =
     *         'Select '//current_source(1:chr_lenb(current_source))//
     *         ' ? '
               selected = io_yesno(string(1:chr_lenb(string)),
     *         'yes',status)
             end if
             if (selected) then
               source = current_source
               count = 1
             end if
           else if (count.gt.2) then
             string =
     *       'Select '//current_source(1:chr_lenb(current_source))//
     *       ' ? '
             selected = io_yesno(string(1:chr_lenb(string)),
     *       'yes',status)
             if (selected) then
               source = current_source
               count = 1
             end if
           end if
         end if
 10      continue
       end do
       if (count.eq.1) then
         return
       end if
       if (count.gt.1) then
         status = ill_mapamb
         return
       end if

C look for match to file names
20     continue
       call mapcat_addtocat(map(1:len_map),.true.,count,status)
       if (count.gt.0) then
         call map_enqdef(imap,status)
       else if (count.eq.0) then
         status = ill_mapno
       end if

       end
