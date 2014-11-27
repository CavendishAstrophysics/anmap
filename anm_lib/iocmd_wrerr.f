C
C
*+ iocmd_wrerr

       subroutine iocmd_wrerr(name,string)
C      -----------------------------------
C
C output an error message and reset the I/O units
C
C Given:
C   command/routine name
       character*(*)     name
C   string to output
       character*(*)     string
C
C The routine will reset the I/O units to the terminal and
C output the string STRING so that further I/O is returned to the
C user. Such action will be required when commands are read from
C a file and not the terminal.
C
*-
       include '/mrao/include/chrlib_functions.inc'

C Local variables:
C CHAR_BUFF       --   character string to hold the output buffer
       character*120    char_buff
C LEN_N           --   length of NAME
C LEN_S           --   length of STRING
C LEN_CB          --   length of CHAR_BUFF
       integer          len_n,len_s,len_cb

C output string
       len_n = chr_lenb(name)
       len_s = chr_lenb(string)
       char_buff = '***('//name(1:len_n)//') '//string(1:len_s)
       call chr_chucas(char_buff(1:len_n+4))
       len_cb = chr_lenb(char_buff)
       call io_wrout(char_buff(1:len_cb))

       end
