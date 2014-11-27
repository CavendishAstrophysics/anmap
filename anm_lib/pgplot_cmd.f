*+ pgplot_cmd

       subroutine pgplot_cmd( interp, cline, lcl, res, lrs, s )
C      --------------------------------------------------------
C 
C Pgplot tcl interface command
C
C Updated:
C  tcl interpreter
       integer               interp(*)
C  command line and results string
       character*2048        cline,  res
       integer               lcl,    lrs
C  erorr status
       integer    s
C
C
C Version 1.0 March 1994.
C
C-
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'

C local variables
       character    option*20, string*80, opt1*10, opt2*10
       character*60 opts(10), opt
       integer      lo, ls
       real*4       x1, x2, y1, y2, u1, u2, v1, v2
       integer      n, m, nx, ny, nz
       real*4       x(1024), y(1024), z(1024)
       integer      ix(1024), iy(1024), iz(1024)
       equivalence (x,ix)
       equivalence (y,iy)
       equivalence (z,iz)

C functions
       integer      pginit

       if (s.ne.0) return


C setup command line for command interpretation
       call io_setcli(cline(1:lcl))
       res = ' '
       lrs = 0

C read option and interprete it
       call io_getwrd('Pgplot: ',' ',option,lo,s)
       if (s.ne.0) return
         if (chr_cmatch(option,'bbuf')) then
           call pgbbuf
         elseif (chr_cmatch(option,'ebuf')) then
           call pgebuf
         elseif (chr_cmatch(option,'next-page')) then
           call pgpage
         elseif (chr_cmatch(option,'update')) then
           call pgupdt
         elseif (chr_cmatch(option,'paper')) then
           call io_getr('Width : ','0.0',x1,s)
           call io_getr('Aspect-ratio : ','0.0',x2,s)
           call pgpap(x1,x2)
         elseif (chr_cmatch(option,'stream-open')) then
           opt = ' '
           call io_getwrd('Device : ','/xwindow',opt,ls,s)
           call io_geti('Segments-X : ','1',nx,s)
           call io_geti('Segments-Y : ','1',ny,s)
           call io_geti('Stream-id : ','1',nz,s)
           m = 0
           n = pginit(m,opt,nx,ny,nz)

         elseif (chr_cmatch(option,'stream-select')) then
           call io_geti('Stream-id : ','1',nz,s)
           call pgselect( nz )

         elseif (chr_cmatch(option,'stream-close')) then
           call io_geti('Stream-id : ','1',nz,s)
           call pgclose( nz )

         elseif (chr_cmatch(option,'window')) then
           call io_getr('x1 : ','0.0',x1,s)
           call io_getr('x2 : ','1.0',x2,s)
           call io_getr('y1 : ','0.0',y1,s)
           call io_getr('y2 : ','1.0',y2,s)
           call pgsetwin(x1,x2,y1,y2)
         elseif (chr_cmatch(option,'viewport').or.
     *         chr_cmatch(option,'vport')) then
           call io_getr('x1 : ','0.0',x1,s)
           call io_getr('x2 : ','1.0',x2,s)
           call io_getr('y1 : ','0.0',y1,s)
           call io_getr('y2 : ','1.0',y2,s)
           call pgsetvp(x1,x2,y1,y2)

         elseif (chr_cmatch(option,'clear')) then
           call pgsave
           call pgqwin(x1,x2,y1,y2)
           call pgqvp(0,u1,u2,v1,v2)
           call pgsetvp(0.0,1.0,0.0,1.0)
           call pgsetwin(0.0,1.0,0.0,1.0)
           call pgsfs( 1 )
           call pgsci( 0 )
           call pgrect( 0.0,1.0,0.0,1.0 )
           call pgunsa
           call pgsetvp(u1,u2,v1,v2)
           call pgsetwin(x1,x2,y1,y2)
         elseif (chr_cmatch(option,'move')) then
           call io_getr('x : ','*',x1,s)
           call io_getr('y : ','*',y1,s)
           call pgmove( x1, y1 )
         elseif (chr_cmatch(option,'draw')) then
           call io_getr('x : ','*',x1,s)
           call io_getr('y : ','*',y1,s)
           call pgdraw( x1, y1 )
         elseif (chr_cmatch(option,'line')) then
           call io_getr('x1 : ','*',x1,s)
           call io_getr('y1 : ','*',y1,s)
           call io_getr('x2 : ','*',x2,s)
           call io_getr('y2 : ','*',y2,s)
           call pgmove( x1, y1 )
           call pgdraw( x2, y2 )
         elseif (chr_cmatch(option,'arrow')) then
           call io_getr('x1 : ','*',x1,s)
           call io_getr('y1 : ','*',y1,s)
           call io_getr('x2 : ','*',x2,s)
           call io_getr('y2 : ','*',y2,s)
           call pgarro( x1, y1, x2, y2 )
         elseif (chr_cmatch(option,'point')) then
           call io_getr('x1 : ','*',x1,s)
           call io_getr('y1 : ','*',y1,s)
           call io_geti('symbol : ','*',n,s)
           call pgpoint(1,x1,y1,n)
         elseif (chr_cmatch(option,'rectangle')) then
           call io_getr('x1 : ','*',x1,s)
           call io_getr('y1 : ','*',y1,s)
           call io_getr('x2 : ','*',x2,s)
           call io_getr('y2 : ','*',y2,s)
           call pgrect(x1,x2,y1,y2)
         elseif (chr_cmatch(option,'circle')) then
           call io_getr('x1 : ','*',x1,s)
           call io_getr('y1 : ','*',y1,s)
           call io_getr('Radius : ','*',x2,s)
           call pgcirc(x1,y1,x2)
         elseif (chr_cmatch(option,'polygon')) then
           call io_geti('Number of points : ','1',nx,s)
           nx = min(nx,1024)
           do n=1,nx
             call io_getr('X-point : ','0.0',x(n),s)
             call io_getr('Y-point : ','0.0',y(n),s)
           enddo
           call pgpoly(nx,x,y)
         elseif (chr_cmatch(option,'mpoint')) then
           call io_geti('Number of points : ','1',nx,s)
           call io_geti('Symbol : ','1',ny,s)
           nx = min(nx,1024)
           do n=1,nx
               call io_getr('X-point : ','0.0',x(n),s)
               call io_getr('Y-point : ','0.0',y(n),s)
           enddo
           call pgpoint(nx,x,y,ny)
         elseif (chr_cmatch(option,'mspoints')) then
           call io_geti('Number of points : ','1',nx,s)
           nx = min(nx,1024)
           do n=1,nx
               call io_getr('X-point : ','0.0',x(n),s)
               call io_getr('Y-point : ','0.0',y(n),s)
               call io_geti('Symbol : ','0.0',iz(n),s)
           enddo
           nz = nx
           call pgpnts(nx,x,y,iz,nz)
         elseif (chr_cmatch(option,'mline')) then
           call io_geti('Number of points : ','1',nx,s)
           nx = min(nx,1024)
           do n=1,nx
               call io_getr('X-point : ','0.0',x(n),s)
               call io_getr('Y-point : ','0.0',y(n),s)
           enddo
           call pgline(nx,x,y)

       elseif (chr_cmatch(option,'text')) then
           string = ' '
           call io_getr('x : ','*',x1,s)
           call io_getr('y : ','*',y1,s)
           call io_getstr('Text  : ',' ', string,s)
           call pgptext(x1,y1,0.0,0.0,string)
       elseif (chr_cmatch(option,'ptext')) then
           string = ' '
           call io_getr('x : ','*',x1,s)
           call io_getr('y : ','*',y1,s)
           call io_getr('angle : ','0.0',x2,s)
           call io_getr('justify : ','0.0',y2,s)
           if (y2.lt.0.0) y2 = 0.0
           if (y2.gt.1.0) y2 = 1.0
           call io_getstr('Text  : ',' ', string,s)
           call pgptext(x1,y1,x2,y2,string)
         elseif (chr_cmatch(option,'mtext') .or. 
     *           chr_cmatch(option,'label')) then
           string = ' '
           opts(1) = 'b .... bottom'
           opts(2) = 't .... top'
           opts(3) = 'l .... left'
           opts(4) = 'r .... right'
           opts(5) = 'lv ... left rotated'
           opts(6) = 'rv ... right rotated'
           call io_getopt('Side (?=list) : ','b',opts,6,opt,s)
           call io_getr('displacement : ','*',x1,s)
           call io_getr('coordinate : ','*',y1,s)
           call io_getr('justify : ','*',y2,s)
           if (y2.lt.0.0) y2 = 0.0
           if (y2.gt.1.0) y2 = 1.0
           call io_getstr('Text  ',' ',string,s)
           call pgmtext(opt,x1,y1,y2,string)

        elseif (chr_cmatch(option,'font')) then
           call io_geti('font : ','1',n,s)
           call pgscf(n)
        elseif (chr_cmatch(option,'tsize')) then
           call io_getr('size : ','1.0',x1,s)
           call pgsch(x1)
        elseif (chr_cmatch(option,'colour')) then
           call io_geti('colour : ','1',n,s)
           call pgsci(n)
        elseif (chr_cmatch(option,'rgb')) then
           call io_geti('colour : ','1',n,s)
           call pgsci(n)
           call io_getr('red : ','0.0',x1,s)
           if (x1.lt.0.0 .or. x1.gt.1.0) x1 = 0.0
           call io_getr('green : ','0.0',x2,s)
           if (x2.lt.0.0 .or. x2.gt.1.0) x2 = 0.0
           call io_getr('blue : ','0.0',y1,s)
           if (y1.lt.0.0 .or. y1.gt.1.0) y1 = 0.0
           call pgscr(n,x1,x2,y1)
         elseif (chr_cmatch(option,'hls')) then
           call io_geti('colour : ','1',n,s)
           call pgsci(n)
           call io_getr('hue (0-360) : ','0.0',x1,s)
           if (x1.lt.0.0 .or. x1.gt.360.0) x1 = 0.0
           call io_getr('lightness (0-1) : ','0.0',x2,s)
           if (x2.lt.0.0 .or. x2.gt.1.0) x2 = 0.0
           call io_getr('saturation (0-1) : ','0.0',y1,s)
           if (y1.lt.0.0 .or. y1.gt.1.0) y1 = 0.0
           call pgscr(n,x1,x2,y1)
         elseif (chr_cmatch(option,'style')) then
           call io_geti('style : ','1',n,s)
           call pgsls( n )
         elseif (chr_cmatch(option,'fill')) then
           call io_geti('fill : ','1',n,s)
           call pgsfs( n )
         elseif (chr_cmatch(option,'width')) then
           call io_geti('width : ','1',n,s)
           call pgslw( n )
         elseif (chr_cmatch(option,'head')) then
           call io_geti('fill-style : ','1',n,s)
           call io_getr('angle : ','45.0',x1,s)
           call io_getr('vent : ','0.3',x2,s)
           if (x2.lt.0.0) x2 = 0.0
           if (x2.gt.1.0) x2 = 1.0
           call pgsah( n, x1, x2 )

         elseif (chr_cmatch(option,'box')) then
           call io_getwrd('X-option : ',' ',opt1,ls,s)
           call io_getr('X-tick : ','0.0',x1,s)
           call io_geti('Nx-subdivision : ','0',nx,s)
           call io_getwrd('Y-option : ',' ',opt2,ls,s)
           call io_getr('Y-tick : ','0.0',y1,s)
           call io_geti('Ny-subdivision : ','0',ny,s)
           call pgbox(opt1,x1,nx,opt2,y1,ny)
         elseif (chr_cmatch(option,'tbox')) then
           call io_getwrd('X-option : ',' ',opt1,ls,s)
           call io_getr('X-tick : ','0.0',x1,s)
           call io_geti('Nx-subdivision : ','0',nx,s)
           call io_getwrd('Y-option : ',' ',opt2,ls,s)
           call io_getr('Y-tick : ','0.0',y1,s)
           call io_geti('Ny-subdivision : ','0',ny,s)
         elseif (chr_cmatch(option,'err-x')) then
           call io_getr('Data-y : ','0.0',y1,s)
           call io_getr('Err-X1 : ','0.0',x1,s)
           call io_getr('Err-X2 : ','0.0',x2,s)
           call io_getr('Top : ','0.0',y2,s)
           call pgerrx(1,x1,x2,y1,y2)
         elseif (chr_cmatch(option,'err-y')) then
           call io_getr('Data-x : ','0.0',y1,s)
           call io_getr('Err-Y1 : ','0.0',x1,s)
           call io_getr('Err-Y2 : ','0.0',x2,s)
           call io_getr('Top : ','0.0',y2,s)
           call pgerry(1,x1,x2,y1,y2)

         elseif (chr_cmatch(option,'data-histogram')) then
           call io_geti('Number of points : ','1',nx,s)
           call io_geti('Central-values (yes=1 0=no): ','1',ny,s)
           nx = min(nx,1024)
           do n=1,nx
               call io_getr('X-point : ','0.0',x(n),s)
               call io_getr('Y-point : ','0.0',y(n),s)
           enddo
           if (ny.eq.1) then
               call pgbin(nx,x,y,.true.)
           else
               call pgbin(nx,x,y,.false.)
           endif
         elseif (chr_cmatch(option,'data-contour')) then
           call io_geti('Nx : ','1',nx,s)
           call io_geti('Ny : ','1',ny,s)
           nx = min(nx,32)
           ny = min(ny,32)
           call io_getnr('Transformation : ','0',y,6,s)
           call io_geti('N-contours : ','1',n,s)
           call io_getnr('Contours : ','0',y(7),n,s)
           call io_getnr('Data : ','0',x,nx*ny,s)
           call pgcons(x,nx,ny,1,nx,1,ny,y(7),n,y)
         elseif (chr_cmatch(option,'data-greyscale')) then
           call io_geti('Nx : ','1',nx,s)
           call io_geti('Ny : ','1',ny,s)
           nx = min(nx,32)
           ny = min(ny,32)
           call io_getnr('Transformation : ','0',y,6,s)
           call io_getr('Foreground : ','0',x1,s)
           call io_getr('Background : ','1',x2,s)
           n = nx*ny
           call io_getnr('Data : ','0',x,n,s)
           call pggray(x,nx,ny,1,nx,1,ny,x1,x2,y)
         elseif (chr_cmatch(option,'data-pixels')) then
           call io_geti('Nx : ','1',nx,s)
           call io_geti('Ny : ','1',ny,s)
           nx = min(nx,32)
           ny = min(ny,32)
           call io_getr('x1 : ','0.0',x1,s)
           call io_getr('x2 : ','0.0',x2,s)
           call io_getr('y1 : ','0.0',y1,s)
           call io_getr('y2 : ','0.0',y2,s)
           call io_getni('Data : ','0',ix,nx*ny,s)
           call pgpixl(ix,nx,ny,1,nx,1,ny,x1,x2,y1,y2)
         endif

C report errors
       call iocmd_err(s,'pgplot command',' ')

       end


