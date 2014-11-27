C
C
       real*4 function ct_lin_grey( rgb, ci, cimin, cimax )
C      ----------------------------------------------------
C
C Function defining a standard linear grey-scale.
C
C    ct_lin_grey = (ci-cimin)/(cimax-cimin)
C
*-

       integer        rgb, ci, cimin, cimax
       ct_lin_grey  = (float(ci-cimin)/float(cimax-cimin))
       end
