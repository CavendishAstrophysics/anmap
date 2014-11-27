C galaxy-read : simple filter to provide a galaxy model to map
C               filter applying various possible interpolation
C               methods
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

       call anm_start( 0,nm,msize,nb,bsize,status )
*       call stack_init(nm,msize,nb,bsize,status)

C call user routine
       call galaxy_read( map_array, status )

C check status value and report an error to the user
       call cmd_err(status,'GALAXY-READ','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end
C
C
*+ galaxy_read

       subroutine galaxy_read(map_array,status)
C      ----------------------------------------
C
C Read an NMR image file into Anmap
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
       include 'galaxy_header.inc'
       include 'galaxy_common.inc'

C Local variables
C   file name and user name
       character     filename*(iolen_file), user_name*(iolen_user),
     *               string*(iolen_file)
       character*60  opts(10), opt
C   execution mode variables
       integer       mode, termno
C   mini redtape of map
       integer       minirt(8)
       real*8        dxsize
       real*4        size
C   pointers to map data
       integer       ip_map, imap
C   size, centre pixel of output region and loop counters
       integer       nx, ny, i, m, uv(2)
       integer       iunit
C  version number of model file
       integer       version
C   functions
       integer       iuvmap2
C   redtape options
       real*8        pi4
       parameter    (pi4 = 3.14159265D+0/4.0)

C check status on entry
       if (status.ne.0) return
       iunit = 0

C prompt for file and open
       call io_getfil('Galaxy-data file-name : ',' ',filename,status)
       if (status.ne.0) goto 999

C find input style
       dxsize = 1.0D+0
       call io_geti('Image-Dimension : ','512',nx,status)
       ny = nx
       call io_getr('Pixel-size : ','1.0',size,status)
       dxsize = size
       opts(1) = 'young-stars ..... image of inverse stellar age'
       opts(2) = 'old-stars ....... image of stellar age'
       opts(3) = 'young-clouds .... image of inverse cloud ages'
       opts(4) = 'old-clouds ...... image of cloud ages'
       opts(5) = 'cloud-mass ...... image of cloud mass'
       call io_getopt('Output-option (?=list) : ','young-stars',
     *                 opts, 5, opt, status )

C read the data file
       call io_nxtlun(iunit,status)
       open (unit=iunit,file=filename,form='unformatted')
       call data_read(iunit,version,status)
       close(iunit)
       if (status.ne.0) goto 999

C get a title
       call io_getstr('Title : ','GALAXY',string,status)
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
       call redt_setsr(string(1:chr_lenb(string)),'IN-MAP',status)
       call map_alloc_out(nx,ny,'DIRECT',imap,ip_map,status)
       do m=1,nx*ny
           map_array(ip_map+m-1) = 0.0
       enddo
       if (chr_cmatch('young-stars',opt)) then
         do i=1,nstar
           uv(1) = nint(star(1,i)*cos(star(2,i))/size)
           uv(2) = nint(star(1,i)*sin(star(2,i))/size)
           m = iuvmap2(minirt,uv)
           if (m.ne.0) then
             map_array(ip_map+m-1)=map_array(ip_map+m-1)+1.0/stage(i)
           endif
         enddo
       elseif (chr_cmatch('old-stars',opt)) then
         do i=1,nstar
           uv(1) = nint(star(1,i)*cos(star(2,i))/size)
           uv(2) = nint(star(1,i)*sin(star(2,i))/size)
           m = iuvmap2(minirt,uv)
           if (m.ne.0) then
             map_array(ip_map+m-1)=map_array(ip_map+m-1)+stage(i)
           endif
         enddo
       elseif (chr_cmatch('young-clouds',opt)) then
         do i=1,ncloud
           uv(1) = nint(cloud(1,i)*cos(cloud(2,i))/size)
           uv(2) = nint(cloud(1,i)*sin(cloud(2,i))/size)
           m = iuvmap2(minirt,uv)
           if (m.ne.0) then
             map_array(ip_map+m-1)=map_array(ip_map+m-1)+1.0/clage(i)
           endif
         enddo

       elseif (chr_cmatch('old-clouds',opt)) then
         do i=1,ncloud
           uv(1) = nint(cloud(1,i)*cos(cloud(2,i))/size)
           uv(2) = nint(cloud(1,i)*sin(cloud(2,i))/size)
           m = iuvmap2(minirt,uv)
           if (m.ne.0) then
             map_array(ip_map+m-1)=map_array(ip_map+m-1)+clage(i)
           endif
         enddo

       elseif (chr_cmatch('cloud-mass',opt)) then
         do i=1,ncloud
           uv(1) = nint(cloud(1,i)*cos(cloud(2,i))/size)
           uv(2) = nint(cloud(1,i)*sin(cloud(2,i))/size)
           m = iuvmap2(minirt,uv)
           if (m.ne.0) then
             map_array(ip_map+m-1)=map_array(ip_map+m-1)+cloud(7,i)
           endif
         enddo

       endif
       call map_end_alloc(imap,map_array,status)

C check status value
999    call cmd_err(status,'GALAXY-READ','Failed ')
       end

C
C+ data_read

      subroutine data_read (dunit,version,iflag)
c
c     Reads data from opened main data file (on unit DUNIT) according to the 
c       format specified by VERSION.  This subroutine can deal with data written
c       in either '7' or '701' format (see notebook 28/01/94).
c
c     INPUT VARIABLE:
c       dunit - file unit from which data is to be read
c
c     OUTPUT VARIABLES:
c       version - format version number
c       iflag   - return status code
c
c ******************************************************************************
c
      include 'galaxy_common.inc'
      include 'galaxy_header.inc'

      integer dunit,                       ! FILE UNIT
     +        version,                     ! FORMAT SPECIFIER
     +        iflag,                       ! STATUS RETURN CODE
     +        i,j                          ! COUNTERS
c
c ------------------------------------------------------------------------------
c
c     DETERMINE FILE FORMAT NUMBER

      read (dunit,err=500) version
      rewind (dunit)

c     LATEST FILE FORMAT

      if (version.eq.701) then
        read (dunit,err=500) version,source,ncloud,nstar,itmax,tstep,
     +                       a0,elas,vmax,mst,msp,eff,narm,omegap,r0,
     +                       p,ang,amp
        do 10 i=1,ncloud
          read (dunit,err=500) (cloud(j,i),j=1,7),clage(i)
   10   continue
        do 20 i=1,nstar
          read (dunit,err=500) (star(j,i),j=1,6),stage(i)
   20   continue
        iflag = 0
        return

c     OTHER SUFFICIENTLY SIMILAR FORMATS

      else if (version.eq.7) then
        read (dunit,err=500) version,source,ncloud,nstar,itmax,tstep,
     +                       a0,mst,msp,eff,narm,omegap,r0,p,ang,amp
        do 30 i=1,ncloud
          read (dunit,err=500) (cloud(j,i),j=1,7),clage(i)
   30   continue
        do 40 i=1,nstar
          read (dunit,err=500) (star(j,i),j=1,6),stage(i)
   40   continue
        elas = 0.7
        vmax = 0.51
        iflag = 0
        return

c     DEFINITELY NON-COMPATIBLE FORMATS

      else
        print*,'Incorrect version (DATA_READ)'
        iflag = 1
        return
      endif

c     ERROR RETURN

  500 print*,'Error on reading data (DATA_READ)'
      iflag = 2
      return

      end


