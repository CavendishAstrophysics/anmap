C   f1dxy.f
C
C   MPH V1 930505
C
C   based on f1dre.f
C
C   inputs: filenames, file size, mm/pixel
C
C   output: data in std. output format (header,x,y)
C           x=0 corresponds to image centre i.e. slice position in 
C           simple xy imaging
C           x dimension in real*4
C
C

C standard include files
       include '/mrao/include/chrlib_functions.inc'
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'

C file and directory name
       character*256 fname,fout
       character*1 next_byte
       integer error,rec,ichan,len,status,i
       integer result(16384*4)
       real*4 x,xpix,xpixl,xpixr
       character cline*(1024)
       status = 0      
C
C      We have allowed for 16K (=16384) array size
C
      ichan = 10
C
C
       call io_enqline( cline, status )
       call io_setcli( cline )
       call io_getwrd( 'Binary-file : ',' ',fname,i,status)
       call io_getwrd( 'Output-file : ',' ',fout,i,status)
       call io_geti('Data-set-length : ','1024',len,status)
       call io_getr('X-scale : ','1.0',xpix,status)
       if (xpix.lt.0.0) then
          call io_getr('X-left : ','1.0',xpixl,status)
          call io_getr('X-right : ','1.0',xpixr,status)
       endif
       len = 2*len
       if (status.ne.0) stop
       if (len .gt.0  .and. len .le. 16384) goto 70
       print *,'*** NMR_A2SP : Illegal data set length : ',len/2
       stop 

C++++++      BINARY OLD, DIRECT ACCESS, LRECL = 1 BYTE
70    OPEN (UNIT = ICHAN,FILE = FNAME,FORM = 'BINARY',
     + STATUS = 'OLD',ACCESS = 'DIRECT',RECL = 1,
     + IOSTAT = ERROR )
      OPEN (UNIT = 2,FILE = FOUT(1:chr_lenb(fout)),
     +      STATUS = 'NEW',IOSTAT = ERROR )
C
C     The intensity information starts at this byte in the file
C
       rec=769
       do 10 i = 1 ,len
       result(i)=0
      read(ichan,REC=rec)next_byte
      result(i)=ichar(next_byte)
      rec=rec+1
      read(ichan,REC=rec)next_byte
      result(i)=result(i)+ichar(next_byte)*256
      rec=rec+1
      read(ichan,REC=rec)next_byte
      result(i)=result(i)+ichar(next_byte)*256*256
10    rec=rec+1
C ... output real values only
      close (ichan)
      print *,'Done read rec = ',rec
      do 20 i=1 , len
C ... check for negative numbers
       if(result(i).gt.16777216/2)then
          result(i)=result(i)-16777216
       end if  
20    continue  

C .. output results

C .. output std header for sp_display compatibility
       print *,'.. outputting standard header: '
       write(2,9999)len/2
9999   format('%ndata  ',I4/
     *        '%ncols 3'/
     *        '%title 1D image')

C    x range runs from -len/2*xpix to len/2*xpix
C    note len include RE and IM components so len/2*2 = len
      print *,'.. outputting data'
      if (xpix.gt.0.0) then
         xpixl = (-float(len)/4.0)*xpix
      else
         xpix = (xpixr - xpixl)/float(len/2 - 1)
      endif
      do i=1,len,2
         x=((i-1)/2)*xpix + xpixl
         write(2,100) x,result(i),result(i+1)
100      format(f10.4,4x,i8,4x,i8)
      end do
      close (2)
      end



