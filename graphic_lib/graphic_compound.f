*+ graphic_compound

       subroutine graphic_compound( region, s )
C      ----------------------------------------
C 
C Plot a compound object
C
C Given:
C  bounding box for the compound object
       real*4     region(4)
C Updated:
C  erorr status
       integer    s
C
C Compound objects are defined via a series of direct drawing statements.
C The objects may be defined in a file or entered interactively.
C
C Version 1.0 March 1993.
C
C-
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer      istat
       logical      command_loop
       character    option*20, string*80, opt1*10, opt2*10
       integer      len_opt, len_string
       real*4       x1, x2, y1, y2, xr, yr, u1, u2, v1, v2,
     *              w1, w2, z1, z2, tscale
       integer      n, nx, ny
       real*4       x(1024), y(1024)
       integer      ix(1024), iy(1024)
       equivalence (x,ix)
       equivalence (y,iy)

       if (s.ne.0) return


C buffer coordinate system
       call graphic_push_coord( s )

C setup coordinates and arrange for scaling to the viewport
       call pgqvp( 0, x1, x2, y1, y2 )
       xr = x2 - x1
       yr = y2 - y1
       u1 = region(1)*xr + x1
       u2 = region(2)*xr + x1
       v1 = region(3)*yr + y1
       v2 = region(4)*yr + y1
       call pgsetvp(u1,u2,v1,v2)
       tscale = min(u2-u1,v2-v1)
       call pgsch( tscale )

C loop and read plotting instructions
       command_loop = .true.
       istat = 0
       do while (command_loop)
         call io_getwrd('Plot> ','exit',option,len_opt,istat)
         command_loop = istat.eq.0
         command_loop = .not.chr_cmatch(option,'exit')
         if (command_loop) then
           if (chr_cmatch(option,'!') .or.
     *         chr_cmatch(option,'#')    ) then
             call io_setcli(' ')
           elseif (chr_cmatch(option,'buffer-start')) then
             call pgbbuf
           elseif (chr_cmatch(option,'buffer-end')) then
             call pgebuf
           elseif (chr_cmatch(option,'update')) then
             call pgupdt
           elseif (chr_cmatch(option,'window')) then
             call io_getr('x1 : ','0.0',x1,istat)
             call io_getr('x2 : ','1.0',x2,istat)
             call io_getr('y1 : ','0.0',y1,istat)
             call io_getr('y2 : ','1.0',y2,istat)
             call pgsetwin(x1,x2,y1,y2)
           elseif (chr_cmatch(option,'view-port')) then
             call io_getr('x1 : ','0.0',x1,istat)
             call io_getr('x2 : ','1.0',x2,istat)
             call io_getr('y1 : ','0.0',y1,istat)
             call io_getr('y2 : ','1.0',y2,istat)
             xr = u2 - u1
             yr = v2 - v1
             w1 = x1*xr + u1
             w2 = x2*xr + u1
             z1 = y1*yr + v1
             z2 = y2*yr + v1
             call pgsetvp(u1,u2,v1,v2)
           elseif (chr_cmatch(option,'move')) then
             call io_getr('x : ','*',x1,istat)
             call io_getr('y : ','*',y1,istat)
             call pgmove( x1, y1 )
           elseif (chr_cmatch(option,'draw')) then
             call io_getr('x : ','*',x1,istat)
             call io_getr('y : ','*',y1,istat)
             call pgdraw( x1, y1 )
           elseif (chr_cmatch(option,'line')) then
             call io_getr('x1 : ','*',x1,istat)
             call io_getr('y1 : ','*',y1,istat)
             call io_getr('x2 : ','*',x2,istat)
             call io_getr('y2 : ','*',y2,istat)
             call pgmove( x1, y1 )
             call pgdraw( x2, y2 )
           elseif (chr_cmatch(option,'text')) then
             call io_getwrd('Text (''in quotes'') : ',' ',
     *                       string,len_string,istat)
             call io_getr('x : ','*',x1,istat)
             call io_getr('y : ','*',y1,istat)
             call io_getr('angle : ','*',x2,istat)
             call io_getr('justify : ','*',y2,istat)
             call pgptext(x1,y1,x2,y2,string(1:len_string))
           elseif (chr_cmatch(option,'font')) then
             call io_geti('font : ','1',n,istat)
             call pgscf(n)
           elseif (chr_cmatch(option,'size')) then
             call io_getr('size : ','1.0',x1,istat)
             call pgsch(x1*tscale)
           elseif (chr_cmatch(option,'colour')) then
             call io_geti('colour : ','1',n,istat)
             call pgsci(n)
           elseif (chr_cmatch(option,'rgb')) then
             call io_geti('colour : ','1',n,istat)
             call pgsci(n)
             call io_getr('red : ','0.0',x1,istat)
             if (x1.lt.0.0 .or. x1.gt.1.0) then
               x1 = 0.0
             endif
             call io_getr('green : ','0.0',x2,istat)
             if (x2.lt.0.0 .or. x2.gt.1.0) then
               x2 = 0.0
             endif
             call io_getr('blue : ','0.0',y1,istat)
             call io_getr('saturation (0-1) : ','0.0',y1,istat)
             if (y1.lt.0.0 .or. y1.gt.1.0) then
               y1 = 0.0
             endif
             call pgscr(n,x1,x2,y1)
           elseif (chr_cmatch(option,'hls')) then
             call io_geti('colour : ','1',n,istat)
             call pgsci(n)
             call io_getr('hue (0-360) : ','0.0',x1,istat)
             if (x1.lt.0.0 .or. x1.gt.360.0) then
               x1 = 0.0
             endif
             call io_getr('lightness (0-1) : ','0.0',x2,istat)
             if (x2.lt.0.0 .or. x2.gt.1.0) then
               x2 = 0.0
             endif
             call io_getr('saturation (0-1) : ','0.0',y1,istat)
             if (y1.lt.0.0 .or. y1.gt.1.0) then
               y1 = 0.0
             endif
             call pgscr(n,x1,x2,y1)
           elseif (chr_cmatch(option,'style')) then
             call io_geti('style : ','1',n,istat)
             call pgsls( n )
           elseif (chr_cmatch(option,'fill')) then
             call io_geti('fill : ','1',n,istat)
             call pgsfs( n )
           elseif (chr_cmatch(option,'width')) then
             call io_geti('width : ','1',n,istat)
             call pgslw( n )
           elseif (chr_cmatch(option,'box')) then
             call io_getwrd('X-option : ',' ',opt1,len_string,istat)
             call io_getr('X-tick : ','0.0',x1,istat)
             call io_geti('Nx-subdivision : ','0',nx,istat)
             call io_getwrd('Y-option : ',' ',opt2,len_string,istat)
             call io_getr('Y-tick : ','0.0',y1,istat)
             call io_geti('Ny-subdivision : ','0',ny,istat)
             call pgbox(opt1,x1,nx,opt2,y1,ny)
           elseif (chr_cmatch(option,'err-x')) then
             call io_getr('Data-y : ','0.0',y1,istat)
             call io_getr('Err-X1 : ','0.0',x1,istat)
             call io_getr('Err-X2 : ','0.0',x2,istat)
             call io_getr('Top : ','0.0',y2,istat)
             call pgerrx(1,x1,x2,y1,y2)
           elseif (chr_cmatch(option,'err-y')) then
             call io_getr('Data-x : ','0.0',y1,istat)
             call io_getr('Err-Y1 : ','0.0',x1,istat)
             call io_getr('Err-Y2 : ','0.0',x2,istat)
             call io_getr('Top : ','0.0',y2,istat)
             call pgerry(1,x1,x2,y1,y2)
           elseif (chr_cmatch(option,'label')) then
             string = ' ' 
             opt1 = ' '
             call io_getwrd('Text (in quotes) : ',' ',
     *                       string,len_string,istat)
             call io_getwrd('Side : ',' ',
     *                       opt1,len_string,istat)
             call io_getr('displacement : ','*',x1,istat)
             call io_getr('coordinate : ','*',y1,istat)
             call io_getr('justify : ','*',y2,istat)
             call pgmtext(opt1,x1,y1,y2,string)
           elseif (chr_cmatch(option,'point')) then
             call io_getr('x1 : ','*',x1,istat)
             call io_getr('y1 : ','*',y1,istat)
             call io_geti('symbol : ','*',n,istat)
             call pgpoint(1,x1,y1,n)
           elseif (chr_cmatch(option,'rectangle')) then
             call io_getr('x1 : ','*',x1,istat)
             call io_getr('y1 : ','*',y1,istat)
             call io_getr('x2 : ','*',x2,istat)
             call io_getr('y2 : ','*',y2,istat)
             call pgrect(x1,x2,y1,y2)
           elseif (chr_cmatch(option,'tbox')) then
             call io_getwrd('X-option : ',' ',opt1,len_string,istat)
             call io_getr('X-tick : ','0.0',x1,istat)
             call io_geti('Nx-subdivision : ','0',nx,istat)
             call io_getwrd('Y-option : ',' ',opt2,len_string,istat)
             call io_getr('Y-tick : ','0.0',y1,istat)
             call io_geti('Ny-subdivision : ','0',ny,istat)
             call pgbox(opt1,x1,nx,opt2,y1,ny)
           elseif (chr_cmatch(option,'polygon')) then
             call io_geti('Number of points : ','1',nx,istat)
             nx = min(nx,1024)
             do n=1,nx
               call io_getr('X-point : ','0.0',x(n),istat)
               call io_getr('Y-point : ','0.0',y(n),istat)
             enddo
             call pgpoly(nx,x,y)
           elseif (chr_cmatch(option,'multi-point')) then
             call io_geti('Number of points : ','1',nx,istat)
             call io_geti('Symbol : ','1',ny,istat)
             nx = min(nx,1024)
             do n=1,nx
               call io_getr('X-point : ','0.0',x(n),istat)
               call io_getr('Y-point : ','0.0',y(n),istat)
             enddo
             call pgpoint(nx,x,y,ny)
           elseif (chr_cmatch(option,'multi-line')) then
             call io_geti('Number of points : ','1',nx,istat)
             nx = min(nx,1024)
             do n=1,nx
               call io_getr('X-point : ','0.0',x(n),istat)
               call io_getr('Y-point : ','0.0',y(n),istat)
             enddo
             call pgline(nx,x,y)
           elseif (chr_cmatch(option,'histogram')) then
             call io_geti('Number of points : ','1',nx,istat)
             call io_geti('Central-values (yes=1 0=no): ','1',
     *                    ny,istat)
             nx = min(nx,1024)
             do n=1,nx
               call io_getr('X-point : ','0.0',x(n),istat)
               call io_getr('Y-point : ','0.0',y(n),istat)
             enddo
             if (ny.eq.1) then
               call pgbin(nx,x,y,.true.)
             else
               call pgbin(nx,x,y,.false.)
             endif
           elseif (chr_cmatch(option,'contour')) then
             call io_geti('Nx : ','1',nx,istat)
             call io_geti('Ny : ','1',ny,istat)
             nx = min(nx,32)
             ny = min(ny,32)
             call io_getnr('Transformation : ','0',y,6,istat)
             call io_geti('N-contours : ','1',n,istat)
             call io_getnr('Contours : ','0',y(7),n,istat)
             call io_getnr('Data : ','0',x,nx*ny,istat)
             call pgcons(x,nx,ny,1,nx,1,ny,y(7),n,y)
           elseif (chr_cmatch(option,'grey')) then
             call io_geti('Nx : ','1',nx,istat)
             call io_geti('Ny : ','1',ny,istat)
             nx = min(nx,32)
             ny = min(ny,32)
             call io_getnr('Transformation : ','0',y,6,istat)
             call io_getr('Foreground : ','0',x1,istat)
             call io_getr('Background : ','1',x2,istat)
             n = nx*ny
             call io_getnr('Data : ','0',x,n,istat)
             call pggray(x,nx,ny,1,nx,1,ny,x1,x2,y)
           elseif (chr_cmatch(option,'pixels')) then
             call io_geti('Nx : ','1',nx,istat)
             call io_geti('Ny : ','1',ny,istat)
             nx = min(nx,32)
             ny = min(ny,32)
             call io_getr('x1 : ','0.0',x1,istat)
             call io_getr('x2 : ','0.0',x2,istat)
             call io_getr('y1 : ','0.0',y1,istat)
             call io_getr('y2 : ','0.0',y2,istat)
             call io_getni('Data : ','0',ix,nx*ny,istat)
             call pgpixl(ix,nx,ny,1,nx,1,ny,x1,x2,y1,y2)
           endif
         endif
       enddo

C jump here in case of error or end here for normal operation
999    continue


C restore coordinates
       istat = 0
       call graphic_pop_coord( istat )
C report errors
       call cmd_err(s,'graphic_compound',' ')

       end
