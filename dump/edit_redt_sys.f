*$ Redtape Editor
*  --------------
C
C Last updated Paul Alexander MRAO, 15/5/90
C
*+ redtape_sys

       subroutine redtape_sys(interp,cdata,map_array,status)
C      -----------------------------------------------------
C
C Redtape Editor Sub-System
C
C Updated:
C    command interpreter data structure
       integer       interp(*)
C    command language data structure
       integer       cdata(*)
C   map array data
       real*4        map_array(*)
C   status
       integer       status
C
C Sub-system providing commands to edit and examine redtapes.
*-
       include '../include/anmap_sys_pars.inc'
C
       integer          number_commands
       parameter       (number_commands = 7)
       character*60     liscom(number_commands)
C
       character*80     command_line
       integer          len_cli, iout

       integer          extra_RT(512)

       logical          exit_at_end
       integer          ncomms, icom, i_com_done

       integer          x_pages, x_style, x_recs, n
       character*8      descriptor_list(16)

       data liscom(1)  /
     *  'read-redtape ............. read RT for specified map'
     *                 /
       data liscom(2)  /
     *  'write-redtape ............ write RT back to map'
     *                 /
       data liscom(3)  /
     *  'display-redtape .......... type or print current RT'
     *                 /
       data liscom(4)  /
     *  'dump-redtape ............. dump the current RT'
     *                 /
       data liscom(5)  /
     *  'set-redtape-item ......... change an entry in current RT'
     *                 /
       data liscom(6)  /
     *  'list-extra-redtape ....... list any records of extra RT'
     *                 /
       data liscom(7)  /
     *  'clear-extra-redtape ...... clear all entries in the extra RT'
     *                 /

       ncomms = number_commands

C check for exit on completion of command
       call io_enqcli(command_line,len_cli)
       exit_at_end = len_cli.ne.0
       i_com_done = 0

C find output unit
       call io_enqout(iout)

1000   continue

C .. auto-exit
         if (exit_at_end .and. i_com_done.gt.0) then
           cdata(1) = 100
           return
         end if

C .. command line interpretation
         if (.not.exit_at_end) call io_setcli(' ')

C .. reset error return
         status=0

C .. read command
         call cmd_getcmd('Edit-Redtape> ',liscom,ncomms,
     *                    icom,status)

C check for error
         if (status.ne.0) then
           call cmd_err(status,'EDIT-REDTAPE',' ')
           goto 1000
         else
           i_com_done = i_com_done + 1
         end if

C .. check icom for a basic command
         if (icom.le.0) then
           cdata(1) = icom
           return
         end if


C jump to required command in command list
         goto (1,2,3,4,5,6,7) icom

1        call edit_read(status)
         goto 1000
2        call edit_write(status)
         goto 1000
3        continue
         call exm_print('ALL',status)
         goto 1000
4        call prredt(iout,-1,status)
         goto 1000
5        call edit_set(status)
         goto 1000
6        call io_enqout( iout )
         call enxrdt( x_pages, x_style, status )
         if (x_pages.gt.0) then
           call enxdsc( descriptor_list, x_recs, status )
         else
           x_recs = 0
         end if
         write (iout,'(1X/1X,''Extra Redtape: ''/ 1X,''Pages   = '',I2/
     *                    1X,''Style   = '',I2/ 1X,''Records = '',I2)')
     *                    x_pages, x_style, x_recs
         do n=1,x_recs
           write (iout,'(1X,''Descriptor-'',I2.2,'' = '',A8)')
     *           n, descriptor_list(n)
         end do
         write (iout,*) ' '
         goto 1000
7        call dpxrdt( extra_RT, 1, status )
         do n=1,512
           extra_RT(n) = 0
         end do
         call ldxrdt( extra_RT, 1, status )
         goto 1000

       end

*$ Edit-Redtape Routines
*  ---------------------

*+ edit_read

       subroutine edit_read(status)
C      ----------------------------
C
C Read the redtape for a map on the stack into the standard common blocks
*-

       integer   status, imap

       if (status.ne.0) return
       call map_getmap('Map : ','Default-Map','NONE',imap,
     *                 status)
       call redt_load(imap,status)
       call cmd_err(status,'EDIT-REDTAPE','Redtape NOT read')

       end


*+ edit_write

       subroutine edit_write(status)
C      -----------------------------
C
C Write the redtape to a map on the stack from the standard common blocks
*-

       integer   status, imap, local_buffer(512)

       if (status.ne.0) return
       call dpredt(local_buffer,status)
       call map_getmap('Map : ','Default-Map','NONE',imap,
     *                 status)
       call ldredt(local_buffer,status)
       call redt_dump(imap,status)
       call cmd_err(status,'EDIT-REDTAPE','Redtape NOT written')

       end

*+ edit_set

       subroutine edit_set(status)
C      ---------------------------
C
C Define values for redtape items
C
C Returned:
C   status word
       integer             status
C
C Routine to provide interactive setting of items in the current redtape
C buffer.  This routine does not therefore alter the redtape of a map
C directly -- a WRITE operation is required to apply this redtape to
C a specific map.
*-

       include '/mrao/include/chrlib_functions.inc'

C define commands
       integer         number_options, lt
       parameter      (number_options = 21)
       character*60    option_list(number_options), option
       character*20    sub_option_list(3), sub_option

C local redtape variables
       real*8          local_ramap, local_decmap, local_refdat,
     *                 local_obsdat, local_beam_r8(2),
     *                 local_usamp, local_vsamp,
     *                 local_skew, local_epoch, local_prang
       real*4          local_beam(2), local_beam_pa,
     *                 local_flxnrm, local_freq
       integer         local_poln, local_iproj, minirt(10), 
     *                 local_tscope
       integer         i, uv_range(4), nr, data_type
       character       txt_input*80, txt_name*16, txt_unit*16

       option_list(1) = 'map-centre'
       option_list(2) = 'projection'
       option_list(3) = 'source-name'
       option_list(4) = 'reference-date'
       option_list(5) = 'observation-date'
       option_list(6) = 'flux-normalization'
       option_list(7) = 'clean-beam'
       option_list(8) = 'frequency'
       option_list(9) = 'polarization'
       option_list(10)= 'name-data-type'
       option_list(11)= 'unit-data-type'
       option_list(12)= 'created'
       option_list(13)= 'history'
       option_list(14)= 'title'
       option_list(15)= 'owner'
       option_list(16)= 'user'
       option_list(17)= 'UV-range'
       option_list(18)= 'sampling'
       option_list(19)= 'pb-pointing'
       option_list(20)= 'pb-to-map-centre'
       option_list(21)= 'telescope-identifier'

C check the error flag on entry
       if (status.ne.0) return

       call io_getopt('Set-option (?=list) : ','?',option_list,
     *             number_options,option,status)
       call chr_chlcas(option)
       if (status.ne.0) then
         call cmd_err(status,'EDIT-REDTAPE','SET failed')
         return
       end if

C decode option
       if (chr_cmatch(option,'map-centre')) then
         call enmapc(local_ramap,local_decmap,local_refdat,
     *               local_obsdat,txt_input,status)
         call io_getra('1950.0 Map-centre RA  : ','*',local_ramap,
     *               status)
         call io_getdec('1950.0 Map-centre DEC : ','*',local_decmap,
     *               status)
         call stmapc(local_ramap,local_decmap,local_refdat,
     *               local_obsdat,txt_input,status)

       elseif (chr_cmatch(option,'pb-pointing')) then
         call entpnt(local_tscope,local_freq,
     *               local_ramap,local_decmap,status)
         call io_getra('1950.0 Map-centre RA  : ','*',local_ramap,
     *               status)
         call io_getdec('1950.0 Map-centre DEC : ','*',local_decmap,
     *               status)
         call sttpnt(local_tscope,local_freq,
     *               local_ramap,local_decmap,status)

       elseif (chr_cmatch(option,'pb-to-map-centre')) then
         call edit_pb2mc

       elseif (chr_cmatch(option,'telescope-identifier')) then
         call entpnt(local_tscope,local_freq,
     *               local_ramap,local_decmap,status)
         call io_geti('Telescope Identifier : ','*',local_tscope,
     *                status)
         call sttpnt(local_tscope,local_freq,
     *               local_ramap,local_decmap,status)

       elseif (chr_cmatch(option,'projection')) then
         call enmapj(local_iproj,local_usamp,local_skew,local_epoch,
     *               local_prang,status)
         sub_option_list(1) = 'equatorial'
         sub_option_list(2) = 'sky'
         sub_option_list(3) = 'tangent-plane'
         call io_getopt('Projection (?=list) : ',
     *               sub_option_list(local_iproj),
     *               sub_option_list(1),3,
     *               sub_option,status)
         call chr_chlcas(sub_option)
         do i=1,3
           if (chr_cmatch(sub_option,sub_option_list(i))) then
             local_iproj = i
           end if
         end do
         call io_getd('Sampling-in-U : ','*',local_usamp,status)
         call io_getd('Skew-angle : ','*',local_skew,status)
         call io_getd('Epoch : ','*',local_epoch,status)
         call stmapj(local_iproj,local_usamp,local_skew,local_epoch,
     *               local_prang,status)

       else if (chr_cmatch(option,'source-name')) then
         call enmapc(local_ramap,local_decmap,local_refdat,
     *               local_obsdat,txt_input,status)
         call io_getstr('Source name : ','*',txt_input,status)
         call stmapc(local_ramap,local_decmap,local_refdat,
     *               local_obsdat,txt_input,status)

       else if (chr_cmatch(option,'reference-date')) then
         call enmapc(local_ramap,local_decmap,local_refdat,
     *               local_obsdat,txt_input,status)
         call io_getd('Reference date (decimal date) : ',
     *             '*',local_refdat,status)
         call stmapc(local_ramap,local_decmap,local_refdat,
     *               local_obsdat,txt_input,status)

       else if (chr_cmatch(option,'observation-date')) then
         call enmapc(local_ramap,local_decmap,local_refdat,
     *               local_obsdat,txt_input,status)
         call io_getd('Observation date (decimal date) : ',
     *             '*',local_obsdat,status)
         call stmapc(local_ramap,local_decmap,local_refdat,
     *               local_obsdat,txt_input,status)

       else if (chr_cmatch(option,'flux-normalization')) then
         call enbeam(local_flxnrm,local_beam,local_beam_pa,
     *               status)
         call io_getr('Flux-normalization : ','*',
     *            local_flxnrm,status)
         call stbeam(local_flxnrm,local_beam,local_beam_pa,
     *               status)

       else if (chr_cmatch(option,'clean-beam')) then
         call enbeam(local_flxnrm,local_beam,local_beam_pa,
     *               status)
         do i=1,2
           local_beam_r8(i) = local_beam(i)
         end do
         call getbw('Beam-width : ','*',local_beam_r8,status)
         do i=1,2
           local_beam(i) = local_beam_r8(i)
         end do
         call io_getr('Beam position-angle (decimal degrees) : ',
     *             '*',local_beam_pa,status)
         call ensamp( local_usamp, local_vsamp, status )
         call calc_flxnrm(local_beam(1),local_beam(2),local_beam_pa,
     *                    local_usamp,local_vsamp,local_flxnrm,status)
         call stbeam(local_flxnrm,local_beam,local_beam_pa,
     *               status)

       else if (chr_cmatch(option,'frequency')) then
         call entype(local_freq,local_poln,txt_name,txt_unit,status)
         call io_getr('Frequency (MHz) : ','*',local_freq,status)
         call sttype(local_freq,local_poln,txt_name,txt_unit,status)

       else if (chr_cmatch(option,'polarization')) then
         call entype(local_freq,local_poln,txt_name,txt_unit,status)
         call io_wrout('.. valid polarization codes are:')
         call io_wrout('.. -1 = undefined; 0 = beam')
         call io_wrout(
     *           '.. 1 = I; 2 = Q; 3 = U; 4 = V; 5 = I-Q; 6 = I+Q')
         call io_geti('Polarization-code : ','*',local_poln,status)
         call sttype(local_freq,local_poln,txt_name,txt_unit,status)

       else if (chr_cmatch(option,'name-data-type')) then
         call entype(local_freq,local_poln,txt_name,txt_unit,status)
         call io_getstr('Name of quantity: ','*',txt_name,status)
         call sttype(local_freq,local_poln,txt_name,txt_unit,status)

       else if (chr_cmatch(option,'unit-data-type')) then
         call entype(local_freq,local_poln,txt_name,txt_unit,status)
         call io_getstr('Unit of quantity: ','*',txt_unit,status)
         call sttype(local_freq,local_poln,txt_name,txt_unit,status)

       else if (chr_cmatch(option,'created')) then
         call io_getstr('Created-text : ',' ',txt_input,status)
         lt = chr_lenb(txt_input)
         call adredt('created',txt_input(1:lt),status)

       else if (chr_cmatch(option,'history')) then
         call io_getstr('History-text : ',' ',txt_input,status)
         lt = chr_lenb(txt_input)
         call adredt('history',txt_input(1:lt),status)

       else if(chr_cmatch(option,'title')) then
         call io_getstr('Title-text : ',' ',txt_input,status)
         lt = chr_lenb(txt_input)
         call adredt('title',txt_input(1:lt),status)

       else if (chr_cmatch(option,'owner')) then
         call io_getstr('Owner-text : ',' ',txt_input,status)
         lt = chr_lenb(txt_input)
         call adredt('owner',txt_input(1:lt),status)


       else if (chr_cmatch(option,'user')) then
         call io_getstr('User-text : ',' ',txt_input,status)
         lt = chr_lenb(txt_input)
         call adredt('user',txt_input(1:lt),status)

       else if (chr_cmatch(option,'UV-range')) then
         call enredt( uv_range, data_type, status )
         call enminirt( minirt, status )
         nr = 4
         call io_getni('UV-range : ','*',uv_range,nr,status)
         if ((uv_range(2)-uv_range(1)+1).ne.minirt(5) .or.
     *       (uv_range(3)-uv_range(4)+1).ne.minirt(6)) then
           call cmd_wrerr('edit-redtape','Illegal UV-range')
         else
           call stredt( uv_range, data_type, status )
         endif

       else if (chr_cmatch(option,'sampling')) then
         call ensamp(local_usamp,local_vsamp,status)
         call io_getd('U-sampling : ','*',local_usamp,status)
         call io_getd('V-sampling : ','*',local_vsamp,status)
         call stsamp(local_usamp,local_vsamp,status)

       else
         call cmd_wrerr('EDIT-REDTAPE','Unknown Option')

       end if

       call cmd_err(status,'EDIT-REDTAPE','Parameter not reSET')

       end
C
C
*+ calc_flxnrm

       subroutine calc_flxnrm(beam_size_u,beam_size_v,beam_pa,
     *                        usamp,vsamp,flxnrm,status)
C      -------------------------------------------------------
C
C Calculate the flux normalization parameter for a clean beam
C
C Given:
C    beam sizes (arcsec) in U' and V'
       real*4     beam_size_u, beam_size_v
C    beam position angle (degrees) (pa of U' from map u'
       real*4     beam_pa
C    sampling in u and v
       real*8     usamp, vsamp
C Returned:
C    normalization parameter
       real*4     flxnrm
C    status word
       integer    status
C
C The factor used for converting from sum of pixel values on a CLEAN map
C to flux density is returned.  All parameters required for the calculation
C are supplied via the arguments to the routine.
*-

C local gemetrical variables
       real*4      angle, cb1, cb2, cbsig1, cbsig2
       real*4      x1, x2, bsum
C range of calculation and counters
       integer     na1, na2
       integer     i1, i2, j1, j2, i, j

C check status on entry
       if (status.ne.0) return
C convert to suitable values for pixel coordinates
      cbsig1 = beam_size_u*0.4246609/usamp
      cbsig2 = beam_size_v*0.4246609/vsamp
      angle  = beam_pa*3.14159265/180.0

C calculate geometrical quantities for the gaussian
      cb1=1.0/(cbsig1*cbsig1*2.0)
      cb2=1.0/(cbsig2*cbsig2*2.0)
      na1=6.324*cbsig1+2.5
      na2=6.324*cbsig2+2.5
      i2 = max(na1,na2)
      j2 = i2
      i1 = -i2
      j1 = -j2

C Calculate beam "volume"
      bsum=0.0
      do j=j1,j2
        do i=i1,i2
          x1 = float(i)*cos(angle) + float(j)*sin(angle)
          x2 = float(j)*cos(angle) - float(i)*sin(angle)
          bsum=bsum+exp(-x1*x1*cb1 -x2*x2*cb2)
        end do
      end do

C determine normalization factor
      flxnrm=1.0/bsum

      end


      subroutine edit_pb2mc 
      include '/mrao/include/maplib_redtape.inc'
      rapnt = raobs
      decpnt = decobs
      end

