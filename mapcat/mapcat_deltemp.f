C
C
*+ mapcat_deltemp

       subroutine mapcat_deltemp(prompt_state,default,status)
C      ------------------------------------------------------
C
C Delete temporary maps
C
C Input:
C    flag controling prompting
       logical         prompt_state
C    default string (yes or no)
       character*(*)   default
C    Status
       integer         status
C
*-

       include 'mapcat_cat.inc'

C strings
       character*80  string
C counter
       integer       n
C string lengths etc
       integer       len_string, len_source, len_program
       integer       chr_lenb
C logical functions
       logical       io_yesno

C check status on entry
       if (status.ne.0) return

C run through the list and test each entry in turn
       call mapcat_open(status)
       do n = 1,max_cat_entries
         call mapcat_read(n,status)
         if (status.ne.0) goto 999
         if (current_map_status(ip_data).eq.true) then
           if (current_map_status(ip_page).eq.true) then
             len_source  = chr_lenb(current_source)
             len_program = chr_lenb(current_program)
             write (string,100) current_map,
     *                          current_source(1:len_source),
     *                          current_program(1:len_program)
             len_string = chr_lenb(string)
             if (prompt_state) then
               if (io_yesno(string(1:len_string),default,status)) then
                 call mapcat_delete(n,.false.,status)
                 call stack_remove(n,status)
               end if
             else
               call mapcat_delete(n,.false.,status)
               call stack_remove(n,status)
             end if
           end if
         end if
       end do

100    format('Delete map ',i3,' [',a,'-',a,'] : ')
999    call mapcat_err(status,'mapcat_deltemp',' ')

       end
