*+ spec_end

       subroutine spec_end( s )
C      ------------------------
C
C End a spectral-analysis program
C
C Updated:
C   error status
       integer    s
C
C Close down after a spectrum-analysis command
C-
       call cmd_end( s )
       end
