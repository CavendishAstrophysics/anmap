C   fopbnet.f
C   image translation, modified for BRUKNET transfer
C   930809
C
      character*20 fname,fout
      character*1 next_byte
      integer error,rec,byte
      integer result(65536*4)
C
C      We have allowed for 256*256 array size
C
      ichan = 10
C
C     The name of our file was mhen3e03.ima
C
C     fname='mhen3e03.ima'
C
       write(*,*)'Enter Binary Filename (extension should be .ima)'
       read(*,200)fname
       write(*,*)'Output filename (file extension should be .imf)'
       read(*,200)fout
200    format (a20)
201     continue
       write(*,*) 'What is the Array size (64 or 128 or 256)'
       read(*,210) len
210    format (i8)
       if (len .gt.0  .and. len .le. 256) goto 70
       goto 201

C++++++      BINARY OLD, DIRECT ACCESS, LRECL = 1 BYTE
70    OPEN (UNIT = ICHAN,FILE = FNAME,FORM = 'BINARY',
     + STATUS = 'OLD',ACCESS = 'DIRECT',RECL = 1,
     + IOSTAT = ERROR )
      OPEN (UNIT = 2,FILE = FOUT,STATUS = 'NEW',IOSTAT = ERROR )
C
C     The intensity information starts at this byte in the file
C
       rec=13057
       do 10 i = 1 ,len*len
       result(i)=0
      read(ichan,REC=rec)next_byte
      result(i)=ichar(next_byte)
c      rec=rec+1
c      read(ichan,REC=rec)next_byte
c      result(i)=result(i)+ichar(next_byte)*256
c      rec=rec+1
c      read(ichan,REC=rec)next_byte
c      result(i)=result(i)+ichar(next_byte)*256*256
10    rec=rec+1
      do 20 i=1 , len*len/8
      j = (i-1)*8
20     write(2,*) ( result(k),k=j+1,j+8)
100   format(i8)
      stop
      end


