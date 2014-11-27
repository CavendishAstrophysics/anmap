      integer ndata
      parameter (ndata=1024)
      real*4 data(ndata*ndata)
      character file*64
      integer ifile
      integer status

      include '/mrao/include/maplib_redtape.inc'
c
      status=0
      call io_initio
      call io_getfil('Input MAP filename:','.map',file,status)
      call opemap(ifile,file,'read',0,status)
      call rdredt(ifile,0,status)
      call rdmap(ifile,data,status)
      close(ifile)
c
      call io_getfil('Output FITS filename:','.FITS',file,status)
      call fits_opmap(ifile,file,'write',1,status)
      call fits_wredt(ifile,1,status)
      call fits_wrmap(ifile,data,status)
      close(ifile)

      end
