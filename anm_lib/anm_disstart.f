*+ anm_disstart

       subroutine anm_disstart( interp, cs, lcs, s )
C      ---------------------------------------------
C
C initialise dispatched command interpreter
C
C Given:
C  pointer to Tcl interpreter
       integer         interp(*)
C Updated:
C  command line
       character*(*)   cs
C  length of command line to last non-blanck character
       integer         lcs
C  error status
       integer         s
C
C-
       if (s.ne.0) return
       if (lcs.gt.0) then
         call io_setcli( ' ' )
         call io_setcli(cs(1:lcs))
         lcs = 0
         cs = ' '
       endif
       end


