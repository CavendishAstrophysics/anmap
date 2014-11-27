C
       real*4 function ct_enh_grey( rgb, ci, cimin, cimax )
C      ----------------------------------------------------
C
C Function defining an enhanced grey-scale.
C
C defining:
C             x = (ci-cimin)/(cimax-cimin)
C then:
C   ct_enh_grey = ((x-lower_bound)/(upper_bound-lower_bound))**power_law
C
C for:
C  lower_bound  <=  x  <=  upper_bound
C
C otherwise:
C   ct_enh_grey = 0.0
C
C Where the parameters upper_bound, lower_bound and power_law are passed
C in common.
*-

       include '../include/tv_modify.inc'

       integer        rgb, ci, cimin, cimax
       real*4         x
       x  = (float(ci-cimin)/float(cimax-cimin))
       if (x.ge.lower_bound .and. x.le.upper_bound) then
         ct_enh_grey = ((x-lower_bound)/(upper_bound-lower_bound))**
     *                 power_law
       else if (x.lt.lower_bound) then
         ct_enh_grey = 0.0
       else
         ct_enh_grey = 1.0
       end if
       end
