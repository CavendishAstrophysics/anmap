C NMR_READ: read nmr image file into Anmap map-catalogue
C
C Document origin and update of this routine
C   P. Alexander, MRAO, Cambridge, 09/04/92
C
*-

C define work array
       integer       nm, nb
       integer       msize, bsize
       parameter    (msize = 256*256)
       parameter    (bsize=1024)
       parameter    (nm=4)
       parameter    (nb=1)
       real*4        map_array( nm*msize + nb*bsize )
       character     cline*(1024)
C error status
       integer       status

C perform standard initialization
       status = 0
       call io_enqline( cline, status )
       call io_setcli( cline )

       call stack_init(nm,msize,nb,bsize,status)
C call user routine
       call nmr_read_ser( map_array, status )

C check status value and report an error to the user
       call cmd_err(status,'NMR-READ','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end
C
C
*+ nmr_read_image

       subroutine nmr_read_ser(map_array,status)
C      -----------------------------------------
C
C Read an NMR serial file into Anmap
C
C Updated:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C An NMR image file consisting of one or more frames is read into
C the Map catalogue.  Each frame is placed in a separate catalogue entry.
C
*-

       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C Local variables
C   file name and user name
       character   filename*(iolen_file), user_name*(iolen_user)
       character   dir*(iolen_file), type*(iolen_type), val*(5),
     *             source*(iolen_file), string*(iolen_file)
       character*1 next_byte
       integer     bsize
       parameter  (bsize = 128*128*32)
       real*4      buffer(bsize)
C   execution mode variables
       integer     mode, termno
       real*8      dxsize
C   mini redtape of map
       integer     minirt(8)
C   pointers to map data
       integer     ip_map, imap
C   size, centre pixel of output region and loop counters
       integer     nx, ny, nf, n, i, j, ls, m, n1, n2, n3
       integer     iunit
C   function to read
       integer     next_int


C   redtape options
       real*8        pi4
       parameter    (pi4 = 3.14159265D+0/4.0)

C check status on entry
       if (status.ne.0) return
       iunit = 0

C prompt for file and open
       call io_getfil('NMR-image file-name : ',' ',filename,status)
       if (status.ne.0) goto 999

C find input style
       dxsize = 1.0D+0
       call io_geti('X/U-dimension : ','128',nx,status)
       ny = nx
       call io_geti('Y/V-dimension (No. FIDs) : ','*',ny,status)
*       call io_geti('Number-of-frames : ','1',nf,status)
       nf = 1

C open input data file
       call io_nxtlun(iunit,status)
       open (unit=iunit,file=filename(1:chr_lenb(filename)),
     + form = 'binary',
     + status = 'old',access = 'direct',recl = 1,
     + iostat = status )
       status = 0

C read header information
*       i = 775
*       nf = next_int( iunit, i )
*       i = 784
*       nx = next_int( iunit, i )
*       ny = next_int( iunit, i )
       if (2*nf*nx*ny.gt.bsize) then
         print *,'*** (NMR-READ) Buffer size execeeded : failed'
         goto 999
       endif
C initialise the redtape for this map:
       call nwredt(nx,ny,3,0,status)
       call stmapj(3,dxsize,0.0D+0,1950.0D+0,0.0D+0,status)
       call stmapc(pi4,pi4,1950.0D+0,1950.0D+0,' ',status)
       call sttype(0.0,-1,'UNKNOWN','UNKNOWN',status)
       call adredt('CREATED','IN-MAP',status)
       call io_enqexe(user_name,mode,termno)
       call adredt('OWNER',user_name,status)
       call adredt('USER',user_name,status)
       call enminirt(minirt,status)

C set up output map
       dir = ' '
       string = ' '
       type = ' '
       call io_brkfil(filename,dir,string,type)
       source = string(1:chr_lenb(string))//'-'//type
       call io_getwrd('Source-name : ',source,source,ls,status)
       if (status.ne.0) goto 999

C read each image frame
       i = 769
       m = 0
       do n=1,nf
         print *,'Reading frame ',n
         do j=1,2*nx*ny
           m = m + 1
           read(iunit,REC=i) next_byte
           buffer(m) = ichar(next_byte)
           n1 = ichar(next_byte)
           i = i + 1
           read(iunit,REC=i) next_byte
           buffer(m) = buffer(m) + ichar(next_byte)*256
           n2 = ichar(next_byte)
           i = i + 1
           read(iunit,REC=i) next_byte
           buffer(m) = buffer(m) + ichar(next_byte)*256*256
           n3 = ichar(next_byte)
           i = i + 1
*           print *,buffer(m),n1,n2,n3
           if(buffer(m).gt.16777216/2)then
              buffer(m)=buffer(m)-16777216
            end if 
         enddo
       enddo
       if (iunit.ne.0) close(iunit)

C place data into map catalogue
       call cmd_init( status )
       call mapcat_open(status)
       m = 0
       do n=1,nf
         string = ' '
         if (nf.gt.1) then
           call chr_chitoc(n,val,ls)
           string = source(1:chr_lenb(source))//'-REAL'//val(1:ls)
         else
           string = source(1:chr_lenb(source))//'-REAL'
         endif
         call redt_setsr(string(1:chr_lenb(string)),'IN-MAP',status)
         call map_alloc_out(nx,ny,'DIRECT',imap,ip_map,status)
         j = 0
         do i=1,2*nx*ny,2
           j = j + 1
           map_array(ip_map+j-1) = buffer(m+i)
         enddo
         call map_end_alloc(imap,map_array,status)
         string = ' '
         if (nf.gt.1) then
           call chr_chitoc(n,val,ls)
           string = source(1:chr_lenb(source))//'-IMAG'//val(1:ls)
         else
           string = source(1:chr_lenb(source))//'-IMAG'
         endif
         call redt_setsr(string(1:chr_lenb(string)),'IN-MAP',status)
         call map_alloc_out(nx,ny,'DIRECT',imap,ip_map,status)
         j = 0
         do i=2,2*nx*ny,2
           j = j + 1
           map_array(ip_map+j-1) = buffer(m+i)
         enddo
         call map_end_alloc(imap,map_array,status)
         m = m + 2*nx*ny
       enddo
       call mapcat_close( status )

C check status value
999    call cmd_err(status,'NMR-READ','Failed ')
       end
C
C
       integer function next_int ( iunit, i )
C      --------------------------------------
C
       character*1 next_byte
       integer iunit, i, iv
       iv = 0 
       read(iunit,REC=i) next_byte
       iv = ichar(next_byte)
       i = i + 1
       read(iunit,REC=i) next_byte
       iv = iv + ichar(next_byte)*256
       i = i + 1
       read(iunit,REC=i) next_byte
       iv = iv + ichar(next_byte)*256*256
       i = i + 1
       next_int = iv
       end
       character*1 function next_char ( iunit, i )
C      -------------------------------------------
C
       character*1 next_byte
       integer iunit, i
       read(iunit,REC=i) next_byte
       next_char = next_byte
       i = i + 1
       end

