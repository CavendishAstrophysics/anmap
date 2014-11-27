*$ 3) Map/Image catalogue routines
*  -------------------------------
C
C N.B. The routine MAPCAT_io_nxtfil is machine dependent
C
*+ mapcat_addtocat

       subroutine mapcat_addtocat(default_description,
     *                            prompt_state,count,status)
C      -----------------------------------------------------
C
C To prompt for and read (a) map(s) into the catalogue
C
C Given:
C    character string desribing default search route for files
       character*(*)   default_description
C    prompt state (turn prompting on/off)
       logical         prompt_state
C Returned:
C    number of maps added to the catalogue
       integer         count
C    status word
       integer         status
C
C The routine will prompt for a valid file name and open the file checking
C to see whether it is a valid map file. The map is read (along with
C the redtape) using the standard RA map reading routines in
C (library)MAPLIB. The buffer entries are updated and the current map set
C to the one just read in.
C
*-
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/iolib_errors.inc'

C local variables used to access file and redtape
       integer                  imap, ifile
       integer                  minirt(10), redtape_buffer(1024)
       integer                  x_pages, x_style
C character variables
       character*(iolen_file)   file_description, filename
       character*(iolen_dir)    default_dir
       character*80             string
       character                source*40, program*20
       integer                  len_fn, len_fd, len_d
       integer                  len_st, idate(3)
C logical variables
       logical                  check_addition
C counters
       integer                  i
C debug function
       logical                  cmd_dblev

C check status on entry
       if (status.ne.0) return

C set number of maps added to zero
       count = 0

C Prompt for filename description
       call mapcat_enqdefdir(default_dir,status)
       len_d = chr_lenb(default_dir)
       len_fd = chr_lenb(default_description)
       if (len_fd.eq.0) then
         file_description=
     *        default_dir(1:len_d)//dir_char_right//'*'
         call io_getwrd('Map file description : ','*',
     *               file_description,len_fd,status)
       else
         file_description = default_description
         len_fd = chr_lenb(file_description)
       end if

C loop for all files matching description
       count = 0
       if (cmd_dblev(6)) then
         print *,'.. file_description = ',file_description(1:len_fd)
       end if
       if (chr_lend(file_description,dir_char_right) .eq.
     *     chr_lenb(file_description) ) then
         string = default_dir(1:len_d)//dir_char_right//
     *            file_description(1:len_fd)
         file_description = string
         len_fd = chr_lenb(file_description)
         if (cmd_dblev(6)) then
           print *,'.. now = ',file_description(1:len_fd)
           print *,'.. chr_lend= ',chr_lend(file_description)
           print *,'.. chr_lenb= ',chr_lenb(file_description)
         end if
       end if
       do while (status.eq.0)
         call mapcat_io_nxtfil(file_description(1:len_fd),
     *                   filename,source,count,status)
*         print *,'.. got status = ',status,no_file,count
         if (status.eq.no_file) then
           if (count.eq.0) then
             call io_wrout('.. no files found matching description')
           end if
           status = 0
           goto 10
         else if (status.ne.0) then
           goto 10
         end if
         len_fn = chr_lenb(filename)
         string = ' '
         write(string,5)filename(1:len_fn)
 5       format('Include ',A,' in Map-Catalogue ? ')
         len_st = chr_lenb(string) + 1
         if (prompt_state) then
           check_addition = io_yesno(string(1:len_st),'yes',status)
         else
           check_addition = .true.
         end if
         imap = 0

         if (check_addition) then
C .. check to see if the file is already in the catalogue
           if (operating_system.ne.'UNIX') then
             call chr_chucas(filename(1:len_fn))
           end if
           call mapcat_exist(filename(1:len_fn),imap,status)
         end if

         if (check_addition .and. imap.eq.0) then
C .. Open the file as a map
           call mapcat_next(imap,status)
           call mapcat_acc(imap,'WRITE',status)
           call mapcat_setrt(imap,filename(1:len_fn),minirt,status)
           do i=1,chr_lenb(source)
             if (source(i:i).eq.file_char_sep) then
               source(i:i) = '-'
             end if
           end do
           call mapcat_setsr(imap,source,program,status)
           call mapcat_mapopen(imap,'READ',ifile,status)
           if (status.ne.0) then
              goto 999
           end if

C .. Read redtape
           call chredt(status)
           if (status.eq.0) then
             call dpredt(redtape_buffer,status)
             call enxrdt(x_pages,x_style,status)
             if (x_pages.ge.1) then
               call dpxrdt(redtape_buffer(513),1,status)
             end if
           else
             status = 0
           end if
           call rdredt(ifile,0,status)
           call enminirt(minirt,status)
           call enhist(idate,program,status)
           call mapcat_setrt(imap,filename,minirt,status)
           call mapcat_setsr(imap,source,program,status)
           call mapcat_mapclose(imap,status)
           call ldredt(redtape_buffer,status)
           call chredt(status)
           if (status.eq.0) then
             call enxrdt(x_pages,x_style,status)
             if (x_pages.ge.1) then
               call ldxrdt(redtape_buffer(513),1,status)
             end if
           else
             status = 0
           end if
           call mapcat_end(imap,status)
           if (status.eq.0) then
             string = ' '
             len_st = chr_lenb(source)
             write (string,7)source(1:len_st),imap
  7          format('Map : ',A,' in Catalogue Entry : ',I4)
             len_st = chr_lenb(string)
             call io_wrout(string(1:len_st))
             call map_setdef(imap,status)
             count = count + 1
           end if
         else if (check_addition .and. imap.ne.0) then
           string = ' '
           write (string,8)filename(1:len_fn),imap
 8         format('File : ',A,' already allocated Entry : ',I4)
           len_st = chr_lenb(string)
           call io_wrout(string(1:len_st))
           call map_setdef(imap,status)
         end if
       end do
10     continue
999    call cmd_err(status,'Add-to-Catalogue','Read operation failed')

       end

