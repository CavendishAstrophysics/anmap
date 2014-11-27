C
C
*+ anm_exec

       subroutine anm_exec(command,sub_system,mode,option,s)
C      -----------------------------------------------------
C
C Execute a program from within Anmap
C
C Given:
C    exit command (not checked)
       character*(*)    command
C    calling sub-system
       character*(*)    sub_system
C    processes execution mode (online, background, batch)
       character*(*)    mode
C    option for command (append, exact)
C        append  ---  append command line options to command
C        exact   ---  use exect form of supplied command 
       character*(*)    option
C Updated:
C    error return code
       integer          s
C 
C The supplied command is executed from within anamp.  This routine
C is system dependent and will make an attempt to execute an external
C routine even on systems not supporting multi-processing.
C-

       character*1024   cli, string
       integer          ls, lc

       include          '/mrao/include/chrlib_functions.inc'
       logical          cmd_dblev

C check for a non-zero status value on entry
       if (s.ne.0) return

C save variable definitions
       call cmd_end(s)

C now perform command
       call io_enqcli(cli,ls)
       lc = chr_lenb(command)
       if (chr_cmatch(option(1:chr_lenb(option)),'append')) then
         string = command(1:lc)//' '//cli(1:ls)
       else
         string = command
       endif
       ls = chr_lenb(string)
       if (chr_cmatch(mode(1:chr_lenb(mode)),'online').or.
     *     chr_cmatch(mode(1:chr_lenb(mode)),'foreground')) then
         cli = string
       elseif (chr_cmatch(mode(1:chr_lenb(mode)),'background')) then
         cli = string(1:ls)//' &'
       elseif (chr_cmatch(mode(1:chr_lenb(mode)),'batch')) then
         cli = 'echo '//string(1:ls)//' | batch -c '
       endif
       lc = chr_lenb(cli)
       if (cmd_dblev(2)) then
         print *,'EXECUTING: ',cli(1:lc)
       endif
       call io_system(cli(1:lc),s)
       call io_setcli( ' ' )
       end
