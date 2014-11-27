C
C
*+ do_catpbc

       subroutine do_catpbc(map_array,status)
C      --------------------------------------
C
C Apply a primary beam correction for the CAT
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C The values for the primary beam correction are obtained from the
C local routine CAT_PBCORR.
*-

C Local variables
       integer    tscope, minirt(8), imap, imapo, ip_mapo, iu, iv, ip
       integer    iuvmap
       real*8     u, v, ra, dec, rapnt, decpnt
       real*4     pb_value, freq

C check status on entry
       if (status.ne.0) return

C read input map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call map_alloc_toout(imap,'DIRECT',map_array,
     *                      imapo,ip_mapo,status)
       call entpnt( tscope, freq, rapnt, decpnt, status )
       if (status.ne.0) goto 999

C read the map size etc.
       call enminirt(minirt,status)
       if (status.ne.0) goto 999

C scale the map
       do iv = minirt(3),minirt(4),-1
         do iu = minirt(1),minirt(2),1
           ip = iuvmap(iu,iv) - 1
           u = iu
           v = iv
           call uvtord( u, v, ra, dec, status )
           call cat_pbcorr(ra,dec,rapnt,decpnt,freq,pb_value,status)

           if (pb_value.gt.0.0) then
             map_array(ip_mapo+ip) = map_array(ip_mapo+ip) /
     *                               pb_value
           else
             map_array(ip_mapo+ip) = 0.0
           end if
         end do
       end do

C Update the TEXT of the redtape
       call adredt('CREATED','CATPBC',status)

C Tidy up
999    call map_end_alloc(imap,map_array,status)
       call map_end_alloc(imapo,map_array,status)

C check status value
       call cmd_err(status,'CATPBC','Failed ')
       end
C
C
C
C
C+cat_pbcorr
C
      subroutine cat_pbcorr ( ra, dec,
     *                        ra_aerial, dec_aerial, freq,
     *                        pb_value, status                 )
C
C     Calculates the primary beam correction for a telescope.
C
C     Given:
C         RA (or hour angle) and dec of the direction that the
C         correction is to be calculated for (angles in radians)
              real*8      ra, dec
C         RA (or hour angle) and dec of the direction that the
C         aerial was pointing at the time
              real*8      ra_aerial, dec_aerial
C         CAT operating frequency
              real*4      freq
C
C     Return values:
C         Primary beam correction factor - a value between 0 and 1
              real        pb_value
C         Status value
              integer     status
C
C     Returns the value of the primary beam for the CAT.
C
C-
             include  '/mrao/include/constants.inc'

C     Variables for calculating tabulated beams
          real*8          s

      if (status.ne.0) return


C Symmetrical beams tabulated in include file.
       s = dsin(dec)*dsin(dec_aerial)+
     *     dcos(dec)*dcos(dec_aerial)*dcos(ra_aerial-ra)
       if ( s .gt. 1.0 ) then
              s = 0.0D+0
       else if ( s .lt. -1.0 ) then
              s = const_pi
       else
              s = abs(acos( s ))
       end if

C Convert separation to a scale factor
       s = freq*s/(1000.0*const_d2r)

C Return PB value
       pb_value = 0.9987 + s*1.2E-3 - (s**2)*2.543E-3 +
     *            (s**3)*8.49E-6 + (s**4)*3.262E-6 -
     *            (s**5)*7.954E-8 + (s**6)*5.573E-10

 9999 if (status.ne.0) call maperr(status,'in subroutine cat_pbcorr.')
      return
      end
