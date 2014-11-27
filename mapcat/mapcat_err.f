C
C
*+ mapcat_err

       subroutine mapcat_err(status,routine,message)
C      ---------------------------------------------
C
C Report an error message
C
C Given:
C   status
       integer    status
C   name of the current routine
       character  routine*(*)
C   additional message text
       character  message*(*)
C
C The error message is written to the error device or the output
C device depending on the value of STATUS. If STATUS=0 the action
C is to return without an error messahe,if STATUS .ne. 0
C then the IOLIB routine io_wrerr is used to output a complete
C error message, and the routine name is appended for a trace-back.
*-
       integer    istat
       logical    error_file_set
       common    /mapcat_local_err_set/ error_file_set

       include   '/mrao/include/iolib_errors.inc'

       if (status.eq.iolib_ok) return

C set up local error messages if not done so already
       if (.not.error_file_set) then
         istat = 0
         call io_setmsg( '(anmap)mapcat-errors:incl', istat)
         error_file_set = .true.
       end if

C output any message
       call cmd_err( status, routine, message )

       end
