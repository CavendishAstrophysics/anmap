C
C
*+ synch_2p_val

       real function synch_2p_val( x )
C      -------------------------------
C
C Return the value of alpha at a given X in the current 2P lookup table
C
C Given:
C    X value to lookup
       real*4        x
C Returned:
C    value at X returned via function name
C
C PA, 13/02/92
*-

       include '../include/synch_defn.inc'

       integer   n, nn
       real*4    frac

       if (x.le.xx(1)) then
         nn = 1
       else
         nn = i2pt
         do n=1,i2pt
           if (x.le.xx(n)) then
             nn = n-1
             goto 1
           end if
         end do
       end if

1      continue
       if (nn.eq.1 .or. nn.eq.i2pt) then
         synch_2p_val = aa(nn)
       else
         frac = (x-xx(nn))/(xx(nn+1)-xx(nn))
         synch_2p_val = aa(nn) + frac*(aa(nn+1)-aa(nn))
       end if
       end
