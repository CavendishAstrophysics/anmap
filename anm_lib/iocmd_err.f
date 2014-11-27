C
C
*+ iocmd_err

       subroutine iocmd_err(status,routine,message)
C      --------------------------------------------
C
C Report an error message
C
C Given:
C    status code to interprete
       integer         status
C    routine name
       character*(*)   routine
C    message text
       character*(*)   message
C
C The error message is written to the error device or the output
C device depending on the value of STATUS. If STATUS=0 the action
C is to return without an error messahe,if STATUS .ne. 0
C then the IOLIB routine IO_WRERR is used to output a complete
C error message, and the routine name is appended for a trace-back.
*-
C
       integer    max_attention
       parameter (max_attention = 1)

       integer                                count_attention
       common /iocmd_local_attention_count/   count_attention
       character*120     char_buff
       integer           len_r,len_m,len_cb,istat
C
       logical                      error_file_set
       common /iocmd_local_err_set/ error_file_set

       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

C determine string lengths
       len_r = chr_lenb(routine)
       len_m = chr_lenb(message)

       if (status.eq.iolib_ok) return

C set up local error messages if not done so already
       if (.not.error_file_set) then
         istat = 0
         call iocmd_enqvar( 'IOCMD_ERRORS', char_buff, istat )
         call io_setmsg( char_buff, istat)
         error_file_set = .true.
       end if

C take differing actions depending on STATUS
       call io_errset(status)
       if (status.eq.usr_break) then
         if (count_attention .lt. max_attention) then
           call io_wrout('***(INTERUPT)')
         end if
         count_attention = count_attention + 1

       else if (status.ne.0 .and. status.ne.usr_break) then
C .. construct trace back message
         char_buff='('//routine(1:len_r)//') '//message(1:len_m)
         len_cb = chr_lenb(char_buff)
         call chr_chucas(char_buff(1:len_r+1))
         call io_wrerr(status,char_buff(1:len_cb))

       else
C .. construct a simple message
         char_buff='***('//routine(1:len_r)//') '//message(1:len_m)
         call io_wrout(char_buff(1:len_cb))

       end if
       end


