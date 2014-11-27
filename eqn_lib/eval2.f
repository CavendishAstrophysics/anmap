       real*8  x(10), y(10)
       character*256 string
       integer  n, nn
       nn = 10
       do n=1,nn
         x(n) = n*0.1
       enddo
       print *,'Setup x,y : ',x,y
       string = 'x * sin(x)'//char(0)
       print *,'Calling eval_eqn::'
*       call eval_eqn( string, nn, x, y )

       do n=1,nn
         print *,x(n),y(n)
       enddo
       
       end

