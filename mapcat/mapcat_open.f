C
C
*+ mapcat_open

       subroutine mapcat_open(status)
C      -----------------------------
C
C Open the map catalogue file if required
C
C Returned:
C     Status
       integer      status
C
*-
       include '/mrao/include/iolib_constants.inc'
C
C Local variables
       character loc_user*(iolen_user),
     *           tmp_file*(iolen_file),
     *           loc_file*(iolen_file)
       integer   len_lf, loc_mode, loc_termno
       logical   exists, dir_exists
       integer   imap, pages_on_file, idir, n, nn
       integer   len_cf

       include 'mapcat_cat.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (status.ne.0) return
       len_cf = chr_lenb(cat_file)
       if (len_cf.eq.0) then
         cat_file = default_cat_file
         current_cat_file = 0
         len_cf = chr_lenb(cat_file)
       end if
       call io_enqexe(loc_user,loc_mode,loc_termno)
       loc_user = '~/mrao'
       call io_makfil(loc_user,cat_file(1:len_cf),'mctg',
     *             tmp_file,len_lf)
       call io_namfil( tmp_file, loc_file, 0, status )
       len_lf = chr_lenb(loc_file)
       exists = status.eq.0
       status = 0
*       inquire (file=loc_file(1:len_lf), exist=exists)
       if (.not.exists) then
         pages_on_file = max_cat_entries*cat_blocksize/(512.0*4.0)
         call io_crefil(loc_file(1:len_lf),pages_on_file,
     *                  .false.,1,status)
         do n=1,num_buffers
           current_map = 0
           do nn=1,cat_blocksize/4
              buffer_records(nn,n) = current_record(nn)
           end do
         end do
         call stack_init(-1,0,-1,0,status)
       end if
       if (cat_unit.eq.0) then
         call io_operan(cat_unit,loc_file(1:len_lf),
     *               'WC',cat_blocksize,0,status)
         call io_makfil(loc_user,cat_dir_file,'mctg',
     *               tmp_file,len_lf)
         call io_namfil( tmp_file, loc_file, 0, status )
         len_lf = chr_lenb( loc_file )
         dir_exists = status.eq.0
         status = 0
*         inquire (file=loc_file(1:len_lf), exist=dir_exists)
         do n=1,num_buffers
           current_map = 0
           do nn=1,cat_blocksize/4
              buffer_records(nn,n) = current_record(nn)
           end do
         end do
         call stack_init(-1,0,-1,0,status)
         if (dir_exists) then
           call io_opefil(idir,loc_file(1:len_lf),'READ',0,status)
           do n=1,10
             read (idir,*) defined_cat_file(n)
             read (idir,'(A)') list_cat_file(n)
           end do
           close (idir)
         end if
       end if
       if (.not.exists) then
         do imap = 1, max_cat_entries
           current_filename = ' '
           current_source   = ' '
           current_program  = ' '
           current_map      = imap
           current_map_status(ip_data)   = false
           current_map_status(ip_open)   = false
           current_map_status(ip_unit)   = 0
           current_map_status(ip_access) = access_clear
           call io_wrfile(cat_unit,imap,current_record,
     *                 cat_blocksize/4,status)
         end do
         if (status.ne.0) goto 999
         call io_wrout('.. Map-Catalogue created and initialised')
         call io_wrout('.. Use Add-To-Catalogue to define maps/images')
       end if

999    call mapcat_err(status,'mapcat_open','Fatal Stack-System Error')

       end
