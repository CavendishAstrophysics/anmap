C
C
*+ do_reproject

      subroutine do_reproject(map_array,s)
C     ------------------------------------
C
C Reproject a map to a new coordinate system and epoch
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       s
C
C This is a user interface to the map_reproj routine in the maplib
C RA library. The reprojection is performed from one astrometric system
C to another.  The astometric part of the image redtape MUST be correct.
C In general this routine should not be used to correct ERRORS in the
C map projection, but rather to change it.
C
C
C Original version     NPR
C ANMAP version 1.0    PA      4/4/88
C ANMAP version 1.1    PA      4/4/91
C
*-
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/include/iolib_functions.inc'
      include '/mrao/include/maplib_errors.inc'

C local variables
      integer             iuv(4), iproj2, iprec, i
      integer             imap, imapo, ip_map, ip_mapo
      integer             iepoch(3), degrid_type
      real*8              usamp2, skew2, ramap2, decmap2, refdat2,epoch2
      real*8              position_angle
      character           string*60, source_name*20, reply*20
      integer             iproj, data_type
      real*8              usamp, skew, epoch, prang, obsdat

C     Valid coordinate types
      character*13        coord_types(3)
      data  coord_types / 'equatorial', 'sky', 'tangent-plane' /
C     Valid precession types
      character*4         prec_types(2)
      data   prec_types / 'fast', 'slow' /
C     Valid degridding types
      character*13        degrid_types(2)
      data degrid_types / 'linear', 'gaussian-sinc' /


C get input map
      if (s.ne.0) return
      call map_getmap('Map : ','Default-Map','READ',imap,s)
      call map_alloc_in(imap,'DIRECT',map_array,ip_map,s)
      call enmapj(iproj,usamp,skew,epoch,prang,s)
      if ( s .ne. 0 ) goto 9999

C     Read in parameters for o/p map
      call io_wrout( ' ' )
      call io_wrout( '.. Enter output map parameters:' )
      call io_getopt( 'Projection type : ', coord_types(iproj),
     *              coord_types, 3,
     *              reply, s      )
      if (s.ne.0) goto 9999
      do 100, i = 1, 3
          if (reply .eq. coord_types(i)) iproj2 = i
  100 continue

C .. the following has been inserted to overcome rounding problems
      if (abs(epoch-1950.0D+0) .lt. 1.0D+0/366.0D+0) then
        epoch = 1950.0D+0
        iepoch(1) = 1
        iepoch(2) = 1
        iepoch(3) = 1950
      else
        call frddat( epoch, iepoch )
      end if
      call io_getdat( 'Epoch of projection : ', '*', iepoch, s )
      if ( s .ne. 0 ) goto 9999
      if (iepoch(1).eq.1.and.iepoch(2).eq.1.and.iepoch(3).eq.1950) then
          epoch2 = 1950.0D+0
      else
          call toddat( iepoch, epoch2 )
          if (abs(epoch-epoch2) .lt. 1.0D+0/366.0D+0) epoch2 = epoch
      end if

      usamp2 = usamp
      call io_getd( 'Sampling in U (arcsec/gp) : ', '*', usamp2, s )
      if (s.ne.0) goto 9999
      skew2 = skew
      if (iproj.eq.2) then

C .. read in position angle on sky
         call io_getd('Position angle (degrees) : ','0',position_angle,
     *             s)
         skew2 = skew + position_angle*3.14159265D+0/180.0D+0
      else

C .. read in skew angle in HMS
        call io_getra( 'Skew angle (HMS) : ', '*', skew2, s )
      end if
      if (s.ne.0) goto 9999
      call enredt( iuv, data_type, s )
      call plot_getuv ('UV-range : ', '*', iuv, s )

      call enmapc( ramap2, decmap2, refdat2, obsdat, source_name, s )
      call io_wrout( ' ' )
      if (io_yesno('Do you want to change the map centre ? ', 'No', s))
     *  then
        call io_getra('Right-Ascension of new map position : ','*',
     *             ramap2,s )
        call io_getdec('DEClination of new map position : ','*',
     *             decmap2,s )
      end if
      if ( s .ne. 0 ) goto 9999

C     Get reprojection parameters:
      call io_wrout( ' ' )
      call io_wrout( '.. Enter re-projection parameters:' )
      call io_getopt( 'Convolution type : ', degrid_types(2),
     *              degrid_types, 2,
     *              reply, s      )
      if (s.ne.0) goto 9999
      do i = 1, 2
          if (reply .eq. degrid_types(i)) degrid_type = i
      end do
      iprec = 0
      if (epoch.eq.1950.0D+0 .or. epoch2.eq.1950.0D+0) iprec = iprec + 1
      call io_getopt( 'Precession type to use : ', prec_types(iprec+1),
     *              prec_types, 2,
     *              reply, s      )
      if (s.ne.0) goto 9999
      do i = 1, 2
          if (reply .eq. prec_types(i)) iprec = i-1
      end do

C     Reproject
      call map_alloc_out(0,0,'DIRECT',imapo,ip_mapo,s)
      call io_wrout('.. working')
      call reproj(  map_array(ip_map), map_array(ip_mapo), iuv,
     *              iproj2, iprec, usamp2, skew2, ramap2, decmap2,
     *              refdat2, epoch2, degrid_type, s )
      if (s.ne.0) goto 9999

C     Set up redtape for output map
      call stredt( iuv, 3, s )
      call stmapc( ramap2, decmap2, refdat2, obsdat, source_name, s )
      call stmapj( iproj2, usamp2, skew2, epoch2, prang, s )
      call adredt ('CREATED', 'PROJECT', s )
      if (s.ne.0) goto 9999

C projection information in redtape
      string=' '
      if (iproj.eq.1)
     *string='Reprojected to aerial coords.'
      if (iproj.eq.2)
     *string='Reprojected to sky coords.   '
      if (iproj.eq.3)
     *string='Reprojected to tangent plane '
      call adredt ('title',string,s)
      string=' '
      if (iproj.eq.1) write (string,510) usamp
510   format ('Aerial coords  ',f4.1,'"/gp')
      if (iproj.eq.2) write (string,520) usamp
520   format ('Sky coords     ',f4.1,'"/gp')
      if (iproj.eq.3) write (string,530) usamp
530   format ('Tangent plane  ',f4.1,'"/gp')
      call adredt ('Projection',string,s)

9999  call map_end_alloc(imapo,map_array, s)
      call map_end_alloc(imap,map_array, s)

      call cmd_err(s,'REPROJECT','Failed')
      end
