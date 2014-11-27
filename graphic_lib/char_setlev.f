C
C
*+ char_setlev

       subroutine char_setlev(value,string,len_str)
C      --------------------------------------------
C
C choose a character representation for the contour level
*-
       character*(*)   string
       integer         len_str
       integer         ival_log, ival_num
       real*4          value

C prepare number
       if (abs(value).lt.1.0e-25) then
         ival_log = 3
         ival_num = 0
       else
         ival_log = nint(log10(abs(value)))
         ival_num = nint(value*(10.0**(3-ival_log)))
       end if

C convert to character string
       call pgnumb(ival_num,ival_log-3,2,string,len_str)

       end
