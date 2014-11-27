C
C Utility Subroutine Library
C --------------------------
C
C The routines in this library perform a number of standard functions
C and consist in large part of standard algorithms for particular
C functions.  To assist documentation layout, LATEX is used in subroutine
C comments especially for mathematics and special symbols.
C
C-
*$ Section 0: General utilities
*  ----------------------------

*+ utl_err

       subroutine utl_err( istat, routine, message )
C      ---------------------------------------------
C
C Return an error message and trace if required
C
C Given:
C   error status
       integer         istat
C   calling routine
       character*(*)   routine
C   message to be written in addition to standard error
       character*(*)   message
C
C An error report is generated if the error code is not equal to
C zero.  The routine name and a message are also reported in addition
C to the translation of the standard error text.
C
C P. Alexander MRAO, Cambridge.  02/08/91
*-
C This version used MRAO IOLIB routines to translate and report the
C error.
C Local variables:
C   character string holding output message
       character*120     char_buff
C   lengths of character strings
       integer           len_r,len_m,len_cb
C   local status value
       integer           s

       include   '/mrao/include/chrlib_functions.inc'
       include   '../include/utilities_consts.inc'
       include   '../include/utilities_errors.inc'

C local buffer signalling error file being defined
       integer                     set_utl_err_file
       common /utilities_err_save/ set_utl_err_file

C determine string lengths
       len_r = chr_lenb(routine)
       len_m = chr_lenb(message)

       if (istat.eq.ill_utl_ok) return

C set up local error messages if not done so already
       if (set_utl_err_file.ne.-1) then
         s = 0
         call io_setmsg( utl_error_file, s)
         set_utl_err_file = -1
       end if

C construct trace back message
       char_buff='('//routine(1:len_r)//') '//message(1:len_m)
       len_cb = chr_lenb(char_buff)
       call chr_chucas(char_buff(1:len_r+1))
       call io_wrerr(istat,char_buff(1:len_cb))

       end
