C
C
       real*4 function ct_standard( rgb, ci, cimin, cimax )
C      ----------------------------------------------------
C
C Function defining an standard colour-scale.
C
C defining:
C             x = (ci-cimin)/(cimax-cimin)
C then:
C                red   = sin(power(red)*x)**2,
C                green = sin(power(green)*x)**2,
C                blue  = sin(power(blue)*x)**2
*-

       include '../include/tv_modify.inc'

       integer        rgb, ci, cimin, cimax
       real*4         x

       x  = (float(ci-cimin)/float(cimax-cimin))
       ct_standard = (sin( col_index(rgb)*x ))**2
       end
