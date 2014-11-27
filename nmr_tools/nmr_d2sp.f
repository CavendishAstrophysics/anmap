



C   size, centre pixel of output region and loop counters
       integer    nddd
       parameter (nddd = 20000)
       real*4     dout( nddd )
       integer    din( nddd )
       integer    status

C prompt for file and open
       status = 0
       call io_initio
       call dowork( dout, din, status )
       end
C
C
       subroutine dowork( dout, din, status )
C   file name and user name
       character  infile*(64), outfile*(64)
       integer    iun, iuo, n, m, nx
       real*4     xpix, xpixl, xpixr, x
       real*4     dout(*)
       integer    din(*)
       integer    chr_lenb, status

       call io_getfil('Input file name : ',' ',infile,status)
       call io_getfil('Output file name : ',' ',outfile,status)
       call io_geti( 'Dimension : ','1024',nx,status)
       call io_getr('X-scale : ','1.0',xpix,status)
       if (xpix.lt.0.0) then
          call io_getr('X-left : ','1.0',xpixl,status)
          call io_getr('X-right : ','1.0',xpixr,status)
       endif
       n = nx
       call io_operan(iun,infile(1:chr_lenb(infile)),
     *                  'READ',n*4,0,status)
       call io_rdfile(iun,1,din,n,status)
       close (iun)

       if (xpix.gt.0.0) then
         xpixl = (-float(nx)/2.0)*xpix
       else
         xpix = (xpixr - xpixl)/float(nx - 1)
       endif
       do m=1,nx
         dout(m) = din(m)
       enddo
       call io_opefil(iuo,outfile(1:chr_lenb(outfile)),
     *                  'WRITE',0,status)
       write(iuo,*)'%ndata ',nx
       write(iuo,*)'%ncols ',2
       do m=1,nx
          x=(m-1)*xpix + xpixl
          write(iuo,*) x, dout(m)
       enddo
       close (iuo)

       end

