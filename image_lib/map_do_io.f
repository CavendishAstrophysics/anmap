*+ do_outmap

       subroutine do_outmap(map_array,status)
C      --------------------------------------
C
C Write out a portion of a map in a specified format
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C A specified region of the input map is written to an output file
C with a specified (non-standard) format.  Formats currently supported
C are:
C
C  ASCII       ---      ascii file with NX,NY,CENTREX,CENTREY header
C  I2-HEADER   ---      I2 array with NX,NY header
C  I4-HEADER   ---      I4 array with NX,NY header
C  R4-HEADER   ---      R4 array with NX,NY header
C  I2-ARRAY    ---      I2 array with NO header
C  I4-ARRAY    ---      I4 array with NO header
C  R4-ARRAY    ---      R4 array with NO header
C  FITS        ---      FITS image
*-

       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C Local variables
C   file name
       character  filename*(iolen_file)
       character  string*(iolen_file), dir*(iolen_file),
     *            type*(iolen_type)
C   mini redtape of map
       integer    minirt(8)
C   pointers to map data
       integer    ip_map, imap
C   size, centre pixel of output region and loop counters
       integer    nx, ny, icx, icy, iu, iv, i, iv1, iu1, jj
       integer*2  nx2, ny2
C   scale factor for integer data
       real*4     scale
C   format string, length and output unit number
       character  format_string*60
       integer    len_format, iunit
C   header items
       integer    iproj
       real*8     dxsize, skew, epoch, prang
       real*4     xsize, xoff, yoff

C   work array
       real*4        r4_array(1024)
       integer       i4_array(1024)
       integer*2     i2_array(2048)
       equivalence  (r4_array(1),i4_array(1))
       equivalence  (r4_array(1),i2_array(1))

C   option list
       integer        num_options
       parameter     (num_options = 9)
       character*80   option_list(num_options), option
       data option_list(1)
     * /'ASCII .................. ascii (text) file with header'/
       data option_list(2)
     * /'I2-header .............. I2 integer array with header record'/
       data option_list(3)
     * /'I4-header .............. I4 integer array with header record'/
       data option_list(4)
     * /'R4-header .............. R4 real array with header record'/
       data option_list(5)
     * /'I2-array ............... I2 integer array NO header record'/
       data option_list(6)
     * /'I4-array ............... I4 integer array NO header record'/
       data option_list(7)
     * /'R4-array ............... R4 real array NO header record'/
       data option_list(8)
     * /'VIEW ................... sdt and spr files for use in VIEW'/
       data option_list(9)
     * /'FITS ................... output as a FITS image'/


C check status on entry
       if (status.ne.0) return

C find input map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       if (status.ne.0) goto 999

C find output style
       call io_getopt('Output data style : ','FITS',option_list,
     *              num_options,option,status)
       if (status.ne.0) goto 999

C find range to write to the output for all cases except fits images
       call redt_load( imap, status )
       call enminirt(minirt,status)
       iu1 = minirt(1)
       iv1 = minirt(3)
       if (.not.chr_cmatch(option(1:chr_lenb(option)),'FITS')) then
         call plot_getuv('UV-range on output : ','*',minirt,status)
         if (status.ne.0) then
           call cmd_wrerr('OUTPUT-MAP','Invalid range on input map')
           goto 999
         end if
       endif

C read input map
       call map_alloc_area(imap,minirt,map_array,ip_map,status)
       if (status.ne.0) goto 999

       nx = minirt(2)-minirt(1)+1
       ny = minirt(3)-minirt(4)+1

C prompt for file
       call io_getfil('Output file name : ',' ',filename,status)
       if (status.ne.0) goto 999

C output data
       if (chr_cmatch(option(1:chr_lenb(option)),'ASCII')) then
         call io_opefil(iunit,filename(1:chr_lenb(filename)),'WRITE',2,
     *                  status)
         icx = nx/2 + 1
         icy = ny/2 + 1
         call io_geti('Central X,U pixel for header : ','*',icx,status)
         call io_geti('Central Y,V pixel for header : ','*',icy,status)
         call io_getwrd('Output-format (FORTRAN) : ','(1P20E9.2)',
     *               format_string,len_format,status)
         if (format_string(1:1).ne.'(' .or.
     *       format_string(len_format:len_format).ne.')' ) then
           call io_wrout('***(OUTPUT-MAP) Illegal format for output')
           goto 999
         end if
         call io_getr('Scale factor : ','1.0',scale,status)
         if (status.ne.0) goto 999
         write(iunit,10) nx, ny, icx, icy, scale
  10     format(1X,I5,I5,I5,I5,1PE14.4)
         do iv=minirt(3),minirt(4),-1
           i=0
           do iu=minirt(1),minirt(2),1
             i = i+1
             r4_array(i) = scale*
     *         map_array(ip_map+minirt(5)*(iv1-iv)+iu-iu1)
           end do
           write(iunit,fmt=format_string(1:len_format))
     *          (r4_array(i), i=1,nx)
         end do
       else if (chr_cmatch(option(1:chr_lenb(option)),'I2-HEADER') .or.
     *          chr_cmatch(option(1:chr_lenb(option)),'I2-ARRAY')) then
         call io_operan(iunit,filename(1:chr_lenb(filename)),'WRITE',
     *                  nx*2,0,status)
         call io_getr('Scale factor : ','1.0',scale,status)
         if (status.ne.0) goto 999
         if (chr_cmatch(option(1:chr_lenb(option)),'I2-HEADER')) then
           nx2 = nx
           ny2 = ny
           i2_array(1) = nx2
           i2_array(2) = ny2
           r4_array(2) = scale
           call io_wrfile(iunit,1,i4_array,nx/2,status)
           jj = 1
         else
           jj = 0
         end if
         do iv=minirt(3),minirt(4),-1
           i=0
           jj = jj + 1
           do iu=minirt(1),minirt(2),1
             i = i+1
             i2_array(i) = scale*
     *         map_array(ip_map+minirt(5)*(iv1-iv)+iu-iu1)
           end do
           call io_wrfile(iunit,jj,i4_array,nx/2,status)
         end do
       else if (chr_cmatch(option(1:chr_lenb(option)),'I4-HEADER') .or.
     *          chr_cmatch(option(1:chr_lenb(option)),'I4-ARRAY')) then
         call io_getr('Scale factor : ','1.0',scale,status)
         if (status.ne.0) goto 999
         call io_operan(iunit,filename(1:chr_lenb(filename)),'WRITE',
     *                  nx*4,0,status)
         if (chr_cmatch(option(1:chr_lenb(option)),'I4-HEADER')) then
           i4_array(1) = nx
           i4_array(2) = ny
           r4_array(3) = scale
           call io_wrfile(iunit,1,i4_array,nx,status)
           jj = 1
         else
           jj = 0
         end if
         do iv=minirt(3),minirt(4),-1
           i=0
           jj = jj + 1
           do iu=minirt(1),minirt(2),1
             i = i+1
             i4_array(i) = scale*
     *         map_array(ip_map+minirt(5)*(iv1-iv)+iu-iu1)
           end do
           call io_wrfile(iunit,jj,i4_array,nx,status)
         end do
       else if (chr_cmatch(option(1:chr_lenb(option)),'R4-HEADER') .or.
     *          chr_cmatch(option(1:chr_lenb(option)),'R4-ARRAY')) then
         call io_operan(iunit,filename(1:chr_lenb(filename)),'WRITE',
     *                  nx*4,0,status)
         if (chr_cmatch(option(1:chr_lenb(option)),'R4-HEADER')) then
           r4_array(1) = nx
           r4_array(2) = ny
           r4_array(3) = 1.0
           call io_wrfile(iunit,1,r4_array,nx,status)
           jj = 1
         else
           jj = 0
         end if
         do iv=minirt(3),minirt(4),-1
           i=0
           jj = jj + 1
           do iu=minirt(1),minirt(2),1
             i = i+1
             r4_array(i) = map_array(ip_map+minirt(5)*(iv1-iv)+iu-iu1)
           end do
           call io_wrfile(iunit,jj,r4_array,nx,status)
         end do
       else if (chr_cmatch(option(1:chr_lenb(option)),'VIEW')) then

         dir = ' '
         string = ' '
         type = ' '
         call io_brkfil( filename(1:chr_lenb(filename)),
     *                   dir, string, type )
C .. write header file
         call io_makfil( dir, string, 'spr', filename, jj )
         call io_opefil( iunit, filename(1:chr_lenb(filename)),
     *                   'WRITE',2,status)
         call enmapj( iproj, dxsize, skew, epoch, prang, status )
         xoff = -minirt(1)
         yoff = -minirt(4)
         xsize = dxsize
         write(iunit,*) '2'
         write(iunit,*) nx
         write(iunit,*) xoff
         write(iunit,*) xsize
         write(iunit,*) ny
         write(iunit,*) yoff
         write(iunit,*) xsize
         write(iunit,*) '3'
         call io_makfil( dir, string, 'sdt', filename, jj)
         write(iunit,*) filename(1:jj)
         close (iunit)
C .. write main data file
         call io_operan(iunit,filename(1:chr_lenb(filename)),'WRITE',
     *                  nx*4,0,status)
         jj = 0
         do iv=minirt(3),minirt(4),-1
           i=0
           jj = jj + 1
           do iu=minirt(1),minirt(2),1
             i = i+1
             r4_array(i) = map_array(ip_map+minirt(5)*(iv1-iv)+iu-iu1)
           end do
           call io_wrfile(iunit,jj,r4_array,nx,status)
         end do
       else if (chr_cmatch(option(1:chr_lenb(option)),'FITS')) then

C .. write DISC-FITS file
         dir = ' '
         string = ' '
         type = ' '
         call io_brkfil( filename(1:chr_lenb(filename)),
     *                   dir, string, type )
         if (chr_lenb(type).eq.0) then
           call io_makfil( dir, string, 'fits', filename, jj )
         endif
         status = 0
         call map_end_alloc(imap,map_array,status)
         call anm_end(status)
         call fits_opmap( iunit, filename(1:chr_lenb(filename)),
     *                    'write', 1, status )
         call fits_wredt( iunit,1,status )
         call fits_wrmap( iunit,map_array(ip_map),status )
         close(iunit)
         stop
       end if

C Tidy up
999    continue
       if (iunit.ne.0 .and. iunit.ne.terminal_out) then
           close (iunit)
       endif
       call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'OUTPUT-MAP','Failed ')
       end
C
C
*+ do_inmap

       subroutine do_inmap(map_array,status)
C      --------------------------------------
C
C Read a data file of specified format as a map
C
C Updated:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C A data file with a specified format is read and a standard "MAP"
C constructed. Formats currently supported are:
C
C  ASCII-HEADER  ---      ascii file with header
C  ASCII-ARRAY   ---      ascii file with NO header
C  I2-ARRAY      ---      I2 array with NO header
C  I4-ARRAY      ---      I4 array with NO header
C  R4-ARRAY      ---      R4 array with NO header
C  R8-ARRAY      ---      R8 array with NO header
C  VIEW          ---      VIEW format files
C  FITS          ---      Disc-Fits file
*-

       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/maplib_redtape.inc'

C Local variables
C   file name and user name
       character  filename*(iolen_file), user_name*(iolen_user)
       character  string*(iolen_file), dir*(iolen_file),
     *            type*(iolen_type)
C   execution mode variables
       integer    mode, termno, jj, iword, iframe
       real*4     xsize, ysize, xoff, yoff
       real*8     dxsize
C   mini redtape of map
       integer    minirt(8)
C   pointers to map data
       integer    ip_map, imap
C   size, centre pixel of output region and loop counters
       integer    nx, ny, icx, icy, iv, i, n
C   scale factor and zero level for integer and ASCII data
       real*4     scale, zero
       integer    iunit, istat

C   work array
       real*4        r4_array(1024)
       real*8        r8_array(512)
       integer       i4_array(1024)
       integer*2     i2_array(2048)
       equivalence  (r4_array(1),i4_array(1))
       equivalence  (r4_array(1),i2_array(1))
       equivalence  (r4_array(1),r8_array(1))

C flags etc.
       logical       fits_map
C   redtape options
       real*8        pi4
       parameter    (pi4 = 3.14159265D+0/4.0)

C   option list
       integer        num_options
       parameter     (num_options = 9)
       character*80   option_list(num_options), option
       data option_list(1)
     * /'ASCII-header ........... ascii (text) file with header'/
       data option_list(2)
     * /'ASCII-array ............ ascii (text) file NO header record'/
       data option_list(3)
     * /'I2-array ............... I2 integer array NO header record'/
       data option_list(4)
     * /'I4-array ............... I4 integer array NO header record'/
       data option_list(5)
     * /'R4-array ............... R4 real array NO header record'/
       data option_list(6)
     * /'R8-array ............... R8 real array NO header record'/
       data option_list(7)
     * /'VIEW ................... spr/sdt format files from VIEW'/
       data option_list(8)
     * /'FITS ................... DISC-FITS format data'/
       data option_list(9)
     * /'I2-frame ............... I2 integer frame NO header record'/


C check status on entry
       if (status.ne.0) return
       fits_map = .false.

C prompt for file and open
       call io_getfil('Input file name : ',' ',filename,status)
       if (status.ne.0) goto 999

C find input style
       dxsize = 1.0D+0
       scale = -1.0
       iframe = 1
       call io_getopt('Input data style : ','FITS',option_list,
     *              num_options,option,status)
       if ( chr_cmatch(option(1:chr_lenb(option)),'ASCII-ARRAY') .or.
     *      chr_cmatch(option(1:chr_lenb(option)),'I2-ARRAY')    .or.
     *      chr_cmatch(option(1:chr_lenb(option)),'I2-FRAME')    .or.
     *      chr_cmatch(option(1:chr_lenb(option)),'I4-ARRAY')    .or.
     *      chr_cmatch(option(1:chr_lenb(option)),'R4-ARRAY')    .or.
     *      chr_cmatch(option(1:chr_lenb(option)),'R8-ARRAY')  ) then
         call io_geti('X/U Size : ','0',nx,status)
         call io_geti('Y/V Size : ','0',ny,status)
         if (nx.le.0 .or. ny.le.0) then
           call io_wrout('***(INPUT-MAP) Invalid image size ')
           goto 999
         end if
         call io_getr('Scale factor : ','1.0',scale,status)
         if (scale.le.0.0) then
           call io_wrout('***(INPUT-MAP) Invalid image scale ')
           goto 999
         end if
         if (chr_cmatch(option(1:chr_lenb(option)),'ASCII-ARRAY'))then
           call io_opefil(iunit,filename(1:chr_lenb(filename)),
     *                    'READ',2,status)
         elseif(chr_cmatch(option(1:chr_lenb(option)),'I2-FRAME'))then
           call io_geti('Frame to read : ','1',iframe,status)
           call io_operan(iunit,filename(1:chr_lenb(filename)),
     *                    'READ',nx*2,0,status)
         elseif(chr_cmatch(option(1:chr_lenb(option)),'I2-ARRAY'))then
           call io_operan(iunit,filename(1:chr_lenb(filename)),
     *                    'READ',nx*2,0,status)
         elseif(chr_cmatch(option(1:chr_lenb(option)),'R8-ARRAY'))then
           call io_operan(iunit,filename(1:chr_lenb(filename)),
     *                    'READ',nx*8,0,status)
         else
           call io_operan(iunit,filename(1:chr_lenb(filename)),
     *                    'READ',nx*4,0,status)
         end if
         call io_getr('Zero-level : ','0.0',zero,status)
       else if (
     *   chr_cmatch(option(1:chr_lenb(option)),'ASCII-HEADER')) then
         call io_getr('Scale factor (<0 read from file) : ','*',
     *             scale,status)
         call io_opefil(iunit,filename(1:chr_lenb(filename)),
     *                  'READ',2,status)
         if (scale.le.0.0) then
           read (iunit,*) nx, ny, icx, icy, scale
         else
           read (iunit,*) nx, ny
         end if
         call io_getr('Zero-level : ','0.0',zero,status)
       else if (
     *   chr_cmatch(option(1:chr_lenb(option)),'VIEW')) then
         call io_brkfil(filename,dir,string,type)
         call io_makfil(dir,string,'spr',filename,jj)
         call io_opefil(iunit,filename(1:chr_lenb(filename)),
     *                  'READ',2,status)
         read(iunit,*)jj
         if (jj.ne.2) then
           call io_wrout('*** VIEW 1D/3D signals not handled')
           close(iunit)
           return
         end if
         read(iunit,*) nx
         read(iunit,*) xoff
         read(iunit,*) xsize
         read(iunit,*) ny
         read(iunit,*) yoff
         read(iunit,*) ysize
         read(iunit,*) jj
         if (jj.ne.3) then
           call io_wrout('*** VIEW signal not R4 -- not handled')
           close(iunit)
           return
         end if
         read(iunit,'(A)') filename
         close (iunit)
         dxsize = xsize
         call io_operan(iunit,filename(1:chr_lenb(filename)),
     *                  'READ',nx*4,0,status)
       else if (
     *   chr_cmatch(option(1:chr_lenb(option)),'FITS')) then

C take special action for FITS maps
         string = ' '
         call io_namfil(filename,string,0,istat)
         jj = chr_lenb(string)
         if (istat.ne.0) then
           call io_makfil(' ',filename,'fits',string,jj)
         endif
         call fits_opmap(iunit,string(1:jj),'READ',1,status)
         call fits_rredt(iunit,1,0,status)
         call enminirt(minirt,status)
         nx = minirt(5)
         ny = minirt(6)
         call ensrcn( filename, status )
         if (chr_lenb(filename).eq.0) then
           call io_getwrd('Source-name : ','MAP',filename,n,status)
         end if
         call redt_setsr(filename(1:chr_lenb(filename)),'FITS',status)
         call map_alloc_out(nx,ny,'DIRECT',imap,ip_map,status)
         call fits_rredt(iunit,1,0,status)
         call fits_rdmap(iunit,1,map_array(ip_map),status)
         fits_map = .true.
       end if
       if (status.ne.0 .or. fits_map) goto 999

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
       call io_getwrd('Source-name : ','MAP',filename,n,status)
       call redt_setsr(filename(1:n),'IN-MAP',status)
       call map_alloc_out(nx,ny,'DIRECT',imap,ip_map,status)
       if (status.ne.0) goto 999

C read data
       if ( chr_cmatch(option(1:chr_lenb(option)),'ASCII-HEADER') .or.
     *      chr_cmatch(option(1:chr_lenb(option)),'ASCII-ARRAY')) then
         i=0
         do iv=minirt(3),minirt(4),-1
           read(iunit,*) (r4_array(n), n=1,nx)
           do n=1,nx
             i = i+1
             map_array(ip_map+i-1) = scale*(r4_array(n)-zero)
           end do
         end do

       else if (
     *  chr_cmatch(option(1:chr_lenb(option)),'I2-HEADER') .or.
     *  chr_cmatch(option(1:chr_lenb(option)),'I2-FRAME') .or.
     *  chr_cmatch(option(1:chr_lenb(option)),'I2-ARRAY')      ) then
         i=0
         jj = (iframe-1)*ny
         do iv=minirt(3),minirt(4),-1
           jj = jj + 1
           iword = nx/2
           call io_rdfile(iunit,jj,i4_array,iword,status)
           do n=1,nx
             i = i+1
             map_array(ip_map+i-1) = scale*(i2_array(n)-zero)
           end do
         end do

       else if (chr_cmatch(option(1:chr_lenb(option)),'I4-ARRAY') .or.
     *          chr_cmatch(option(1:chr_lenb(option)),'I4-ARRAY')) then
         i=0
         jj = 0
         do iv=minirt(3),minirt(4),-1
           jj = jj + 1
           iword = nx
           call io_rdfile(iunit,jj,i4_array,iword,status)
           do n=1,nx
             i = i+1
             map_array(ip_map+i-1) = scale*(i4_array(n)-zero)
           end do
         end do

       else if (chr_cmatch(option(1:chr_lenb(option)),'R8-ARRAY') .or.
     *          chr_cmatch(option(1:chr_lenb(option)),'R8-ARRAY')) then
         i=0
         jj = 0
         do iv=minirt(3),minirt(4),-1
           jj = jj + 1
           iword = 2*nx
           call io_rdfile(iunit,jj,r8_array,iword,status)
           do n=1,nx
             i = i+1
             map_array(ip_map+i-1) = scale*(r8_array(n)-zero)
           end do
         end do

       else if (chr_cmatch(option(1:chr_lenb(option)),'VIEW') .or.
     *          chr_cmatch(option(1:chr_lenb(option)),'R4-ARRAY')) then
         i=0
         jj = 0
         scale = 1.0
         do iv=minirt(3),minirt(4),-1
           jj = jj + 1
           iword = nx
           call io_rdfile(iunit,jj,r4_array,iword,status)
           do n=1,nx
             i = i+1
             map_array(ip_map+i-1) = scale*r4_array(n)
           end do
         end do
       end if

C Tidy up
999    continue
       if (iunit.ne.0 .and. iunit.ne.terminal_out) close (iunit)
       call map_end_alloc(imap,map_array,status)

C check status value
       call cmd_err(status,'INPUT-MAP','Failed ')
       end

