C   fopenms.f
C
C   M.P. Hollewand 930319
C
C  -program to translate binary Bruker image file to integer text file
C   based on fopen.f
C  -input file is a multislice image of 4 slices
C  -output file is single image in 4 quadrants
C
C   input - output mapping
C
C    input    output
C      1
C      2       1  2
C      3       3  4
C      4
C------------------------------------------------------------------------
C
      character*20 fname,fout
      character*1 next_byte
      integer error,rec,byte,nsli
      integer result(4,65536*4)
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
       write(*,*) 'Enter Number of Slices (multiple of 2)'
       read (*,210) nsli
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
      do 12 n = 1, nsli
       do 10 i = 1 ,len*len
         result(n,i)=0
         read(ichan,REC=rec)next_byte
         result(n,i)=256*result(n,i)+ichar(next_byte)
         rec=rec+1
         read(ichan,REC=rec)next_byte
         result(n,i)=256*result(n,i)+ichar(next_byte)
         rec=rec+1
         read(ichan,REC=rec)next_byte
         result(n,i)=256*result(n,i)+ichar(next_byte)
10       rec=rec+1
12    continue
C
C ... output image data - to view as 4 images in one display
C
      do 20 np = 1,3,2
         do 25 j=1,len
            do 30 i=1,len/8
               k=(j-1)*len+(i-1)*8
               write(2,*)  ( result(np,l), l=k+1,k+8)
30          continue
            do 32 i=1,len/8
               k=(j-1)*len+(i-1)*8
               write(2,*)  ( result(np+1,l), l=k+1,k+8)
32          continue
25       continue
20    continue          


100   format(i8)
      write(*,*) 'The output file has dimensions of:',nsli/2*len
      stop
      end

