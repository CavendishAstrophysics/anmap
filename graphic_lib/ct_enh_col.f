C
C
       real*4 function ct_enh_col( rgb, ci, cimin, cimax )
C      ---------------------------------------------------
C
C Function defining an enhanced colour-scale.
C
C defining:
C             x = (ci-cimin)/(cimax-cimin)
C then:
C   ct_enh_col = ((x-start_colour)/(upper_bound-lower_bound))**power_law
C
C for:
C  lower_bound,start_colour  <=  x  <=  upper_bound
C
C otherwise:
C   ct_enh_col = 0.0 or 1.0
C
C Where the parameters upper_bound, lower_bound and power_law are passed
C in common and depend on the colour index
*-

       include '../include/tv_modify.inc'

       integer        rgb, ci, cimin, cimax
       real*4         x

       x  = (float(ci-cimin)/float(cimax-cimin))
       if (x.ge.lower_bound .and. x.le.upper_bound) then
         if ((x-lower_bound).ge.col_starts(rgb)) then
           ct_enh_col = (       (x-col_starts(rgb))
     *                     /(upper_bound-col_starts(rgb))
     *                  )**col_power_law(rgb)
         else
           ct_enh_col = 0.0
         end if
       else if (x.lt.lower_bound) then
         ct_enh_col = 0.0
       else
         ct_enh_col = 1.0
       end if
       end
