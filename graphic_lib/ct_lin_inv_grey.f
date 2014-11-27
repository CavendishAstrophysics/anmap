C
       real*4 function ct_lin_inv_grey( rgb, ci, cimin, cimax )
C      --------------------------------------------------------
C
C Function defining a standard linear inverted grey-scale.
C
C    ct_lin_inv_grey = (cimax-ci)/(cimax-cimin)
C
*-

       integer        rgb, ci, cimin, cimax
       ct_lin_inv_grey  = (float(cimax-ci)/float(cimax-cimin))
       end
