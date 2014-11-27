*+ anm_disend

       subroutine anm_disend( interp, results, cs, lcs, s )
C      ----------------------------------------------------
C
C closedown dispatched command interpreter
C
C Given:
C  pointer to Tcl Interpreter
       integer         interp(*)
C Updated:
C  results string
       character*(*)   results
C  command line
       character*(*)   cs
C  length of command line to last non-blanck character
       integer         lcs
C  error status
       integer         s
C
C-
       include '/mrao/include/chrlib_functions.inc'

       if (s.ne.0) return
       if (chr_lenb(results).gt.0) then
         cs = cs(1:chr_lenb(cs))//results(1:chr_lenb(results))//char(0)
       endif
       lcs = chr_lenb(cs)
       cs = cs(1:lcs)//char(0)
       call iocmd_pars2tcl( interp, s)
       end


