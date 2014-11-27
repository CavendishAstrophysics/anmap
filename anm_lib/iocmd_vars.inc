C
C parameter name table for iocmd library
C
       integer    nvars
       parameter (nvars = 100)
       character*40  iocmd_var(nvars)
       character*120 iocmd_text(nvars)
       common /iocmd_varsc/ iocmd_var, iocmd_text


