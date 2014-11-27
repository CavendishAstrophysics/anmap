C
C
*+ mapcat_exlist

       subroutine mapcat_exlist(ilist,nlist,status)
C      --------------------------------------------
C
C Find list of catalogue entries acording to a seach criterion
C
C Returned:
C    list of catalogue entries
       integer     ilist(*)
C    length of list
       integer     nlist
C    status
       integer     status
C
C-

       include 'mapcat_cat.inc'

C Local variables
C   options
       integer           nopt
       parameter        (nopt = 4)
       character*80      options(nopt), option
       data options(1) /
     * 'index-list ............. search by catalogue entry number '
     *                /
       data options(2) /
     * 'source-name ............ search by full catalogue source name'
     *                /
       data options(3) /
     * 'file-name .............. search by disc file name of entry'
     *                /
       data options(4) /
     * 'program-name ........... search by program name of entry'
     *                /

C counters
       integer           i
C strings and length of strings
       integer           len_source, len_program, len_st1, len_st2,
     *                   len_file
       character*80      st1, st2

C functions
       include '/mrao/include/chrlib_functions.inc'


C check status on entry
       if (status.ne.0) return

C find search options
       call io_getopt('Search-type : ','Index-List',options,nopt,
     *             option,status)

       if (chr_cmatch(option,options(1))) then
         do i=1,256
           ilist(i) = i
         end do
         call io_getlst('Catalogue-index-list : ',' ',
     *               st1,ilist,256,nlist,status)

       else if (chr_cmatch(option,options(2))) then
         st1 = ' '
         st2 = ' '
         call io_getwrd('Source-name : ',' ',st1,len_st1,status)
         if (status.ne.0) goto 999
         nlist = 0
         do i = 1,max_cat_entries
           call mapcat_read(i,status)
           if (status.ne.0) goto 999
           if (current_map_status(ip_data).eq.true) then
             len_source  = chr_lenb(current_source)
             len_program = chr_lenb(current_program)
             write (st2,'(A,''-'',A)')
     *                          current_source(1:len_source),
     *                          current_program(1:len_program)
             len_st2 = chr_lenb(st2)
             if (chr_cmatch(st1(1:len_st1),st2(1:len_st2))) then
               nlist = nlist+1
               ilist(nlist) = i
             end if
           end if
         end do

       else if (chr_cmatch(option,options(3))) then
         st1 = ' '
         call io_getwrd('File-name : ',' ',st1,len_st1,status)
         if (status.ne.0) goto 999
         nlist = 0
         do i = 1,max_cat_entries
           call mapcat_read(i,status)
           if (status.ne.0) goto 999
           if (current_map_status(ip_data).eq.true) then
             len_file = chr_lenb(current_filename)
             if (chr_cmatch(st1(1:len_st1),
     *                  current_filename(1:len_file))) then
               nlist = nlist+1
               ilist(nlist) = i
             end if
           end if
         end do

       else if (chr_cmatch(option,options(4))) then
         st1 = ' '
         st2 = ' '
         call io_getwrd('Program-name : ',' ',st1,len_st1,status)
         if (status.ne.0) goto 999
         nlist = 0
         do i = 1,max_cat_entries
           call mapcat_read(i,status)
           if (status.ne.0) goto 999
           if (current_map_status(ip_data).eq.true) then
             st2 = current_program
             len_st2 = chr_lenb(st2)
             if (chr_cmatch(st1(1:len_st1),st2(1:len_st2))) then
               nlist = nlist+1
               ilist(nlist) = i
             end if
           end if
         end do

       end if

999    call mapcat_err(status,'mapcat_exlist',' ')
       end
