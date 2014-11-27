C
*+synch_fitmode

       subroutine synch_fitmode( set_mode )
C      ------------------------------------
C
C Define the mode (type) of least-squares fitting
C
C Given:
C    mode
       integer    set_mode
C
C Setup the mode switch for synchrotron fitting
C
C-
       integer                    mode, icount
       common /local_lsfun1_mode/ mode, icount


       mode = set_mode
       icount = 0

       end
