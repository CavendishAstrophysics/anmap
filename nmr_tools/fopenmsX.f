C     fopenmsX.f
C
C    M.P. Hollewand  930324
C
C   - program to translate Brucker image file in binary form to 
C     array of integer text.
C   - based on fopenms.f
C   - this version opens a multislice dataset and creates one output 
C     file for every slice
C   - output filenames:  the filenames are given as input to the 
C     do_fopenmsX procedure
C
C     FILENAME PROTOCOL 
C        if the input file is xxxxx.ima (usual output is xxxxx.dat)
C        then the file for the first slice is:  xxxxx.s1.dat
C                        and the nth slice is:  xxxxx.sn.dat
C

C
      integer    max_slice
      parameter (max_slice=4)
      character*20 fname
      character*20 fout(max_slice)
      character*1 next_byte
      integer error,rec,byte,nsli
      integer result(max_slice,65536*4)
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
       write(*,*) 'Enter Number of Slices (multiple of 2)'
       read (*,210) nsli
       write(*,*)'Output filename (file extension should be .imf)'

       if(nsli.gt.16) then
          write(*,*) 'Too many slices - program stopping'
          goto 300
       end if

       do i=1,nsli
          read(*,200) fout(i)
       end do
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

C ... open nsli units
      do 60 i=1,nsli
         OPEN (UNIT=10+i,FILE=FOUT(i),STATUS='NEW',IOSTAT=ERROR)
60    continue
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
C ... output image data - 1 image per file
C
      do 20 np=1,nsli
         ounit=10+np
         do 25 j=1,len
            do 30 i=1,len/8
               k=(j-1)*len+(i-1)*8
               write(ounit,*)  ( result(np,l), l=k+1,k+8)
30          continue
25       continue
20    continue 


100   format(i8)
      write(*,*) 'The output file has dimensions of:',nsli/2*len

300   continue
      stop
      end

