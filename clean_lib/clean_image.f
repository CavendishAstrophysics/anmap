*+ clean_image

       subroutine clean_image(residual_map, map, beam,
     *                        cc_values, cc_posns, cc_list, status )
C      -------------------------------------------------------------
C
C Open the existing residual map file and CLEAN / RESTORE
C
C Given:
C   File name of the residual file
       character*(*)      residual_map
C   work space for the map
       real*4             map(*)
C   work space for the beam
       real*4             beam(*)
C   work space for the clean components
       real*4             cc_values(*)
       integer*2          cc_posns(2,*)
       integer*2          cc_list(*)
C Returned:
C   error status
       integer            status

C
C The CLEAN routine requires:
C  A) File name of an existing RESDIUAL map
C  B) Work space for the map, beam and clean components
C
C If the beam is in fact a BEAM-SET, then the routine panels the beam
C into a number of beam windows - one for each sub-beam in the beam-set.
C CLEANing proceeds using the beam window appropriate to the local
C maxima found on the map.
C
C Search-Windows, Clean-Windows and an overall Beam-Window may be
C specified by the user.
*-

       include      '../include/clean_record.inc'
       include      '/mrao/include/chrlib_functions.inc'
       include      '/mrao/include/maplib_redtape.inc'

C additional file names
       character*60   def_dir, clean_components_map, cleaned_map
       integer        len_file
C output unit
       integer        iout
C buffer for clean-components redtape and extra redtape
       integer        redt_cc(1024), extra_RT(1024)
C descriptors for extra redtape
       integer        ndesc
       character*8    desc_list(12)
C unit numbers for residual map, beam and clean map
       integer        iresi, ibeam, icc, iclean
C logical flag indicating a beam set clean
       logical        bset_clean
C number of beams in a beam set clean
       integer        n_beam
C map/beam areas
       integer        clean_area(4),
     *                search_area(4,10),
     *                beam_area(4,500),
     *                box_area(4), bset_window(4)
C centres of beams on beam set
       integer        bset_u, bset_v
C UV-ranges on beam and map and size of map, beam
       integer        uv_beam(4), uv_map(4),
     *                ixmap, iymap, ixbeam, iybeam
C loop counter
       integer        n, nn_ext, nn_style
C array used in SORT clean
       integer        index(1)
C variables used in defining redtape of output maps
       real*4         zmnx(10)
       integer        iuv(4),izmnx(10)
       integer        count

C check status
       if (status.ne.0) return

C determine output file
       call io_enqout(iout)

C open the residual map file and read the control record
       write(iout,'(1X/1X,''CLEAN: Starting clean of '',A/1X)')
     *       residual_map(1:chr_lenb(residual_map))
       call opemap(iresi, residual_map(1:chr_lenb(residual_map)),
     *               'WRITE', 1, status)
       call rdredt(iresi, 1, status )
       call enxrdt(nn_ext,nn_style,status)
       call enxrec(clean_text_header, clean_record, status)
       call enmdir( def_dir, status )
       call io_makfil( def_dir(1:chr_lenb(def_dir)),
     *              generic_name(1:chr_lenb(generic_name)),
     *              'ccmp', clean_components_map, len_file )
       call io_makfil( def_dir(1:chr_lenb(def_dir)),
     *              generic_name(1:chr_lenb(generic_name)),
     *              'cln', cleaned_map, len_file )
       print *,'CLEAN:'
       call dpredt( redt_map, status )
       call iuv_load( uv_map, status )
       call iuv_load( iuv, status )
       ixmap = ixmax
       iymap = iymax
       call rdmap(iresi,map,status)
       print *,'CLEAN:'
       if (status.ne.0) then
         call cmd_err(STATUS,'CLEAN','Error on residual: CLEAN failed')
         goto 999
       end if

C .. create the clean map file
       print *,'CLEAN:'
       call io_wrout('CLEAN: creating CLEANed map')
       call io_wrout(cleaned_map(1:chr_lenb(cleaned_map)))
       call opemap( iclean, cleaned_map(1:chr_lenb(cleaned_map)),
     *             'WRITE', 0, status )
       call wrredt( iclean, 0, status )
       close (iclean)
       print *,'CLEAN:'

C open the clean components map and read
       call io_wrout(
     *      clean_components_map(1:chr_lenb(clean_components_map)))
       call opemap(icc,
     *      clean_components_map(1:chr_lenb(clean_components_map)),
     *      'UPDATE', 0, status)
       call rdredt(icc, 0, status )
       call enxrec(clean_text_header, clean_record, status)
       call dpredt(redt_cc, status)
       call dpxrdt(extra_RT, 1, status)
       call rdmapc(icc,cc_posns,cc_values,max_cc_iter,number_cc,status)
       if (status.ne.0) then
         call cmd_err(STATUS,'CLEAN',
     *                'Error on clean components: CLEAN failed')
         goto 999
       end if

C Open the beam and read
       call opemap( ibeam, beam_name(1:chr_lenb(beam_name)), 'READ',
     *              0, status )
       call rdredt( ibeam, 0, status)
       call dpredt( redt_beam, status)
       call iuv_load( uv_beam, status )
       ixbeam = ixmax
       iybeam = iymax
       call rdmap(ibeam,beam,status)
       close (ibeam)
       if (status.ne.0) then
         call cmd_err(status,'CLEAN','Error on beam: CLEAN failed')
         goto 999
       end if
       print *,'CLEAN:'

C take special action if a beam set clean is required
C check for a beam set clean
       bset_clean = maptyp.eq.3
       if (bset_clean .and. lclean) then
         call ldredt( redt_beam, status )
         n_beam = 0
         do bset_v = v0set, v0set+(nvset-1)*dvset, dvset
           do bset_u = u0set, u0set+(nuset-1)*duset, duset
C .. Set up a window on the current beam in the beamset
             bset_window(1) = bset_u-duset/2
             bset_window(2) = bset_u+duset/2
             bset_window(3) = bset_v+dvset/2
             bset_window(4) = bset_v-dvset/2

C .. Only include whole beams
             if ((bset_window(1) .ge. beam_window(1)) .and.
     *           (bset_window(2) .le. beam_window(2)) .and.
     *           (bset_window(3) .le. beam_window(3)) .and.
     *           (bset_window(4) .ge. beam_window(4))       ) then
                 n_beam = n_beam + 1
                 call uv_conv(uv_beam, bset_window, beam_area(1,n_beam))
              end if
           end do
         end do
       print *,'CLEAN:'

       else

         n_beam = 1
         call uv_conv(uv_beam, beam_window, beam_area(1,1))

       end if

       call uv_conv(uv_map, clean_window, clean_area)
       call uv_conv(uv_map, box_window, box_area)
       do n = 1,number_search
         call uv_conv(uv_map, search_window(1,n), search_area(1,n))
       end do
       print *,'CLEAN:'

C do CLEAN
       if (lclean) then
         itlim = itlim + number_it_done
         call ldredt( redt_map, status )
       print *,'CLEAN: -- CLEANING'
         call clean_map ( map, ixmap, iymap,
     *                    clean_area, search_area, box_area,
     *                    beam, ixbeam, iybeam, n_beam, beam_area,
     *                    cc_values, cc_posns, cc_list, number_cc,
     *                    number_it_done, index, status           )
       print *,'CLEAN: -- DONE'

C .. write the clean components
         itlim = 0
         call ldredt( redt_cc, status )
         call ldxrdt( extra_RT, 1, status )
         times_cleaned = times_cleaned + 1
         call io_wrout('CLEAN: writing CLEAN COMPONENTS map')
         zmnx(1) = -1.0E+30
         zmnx(2) = 1.0E+30
         do n=1,number_cc
           if (cc_values(n).gt.zmnx(1)) then
             zmnx(1) = cc_values(n)
             izmnx(1) = cc_posns(1,n)
             izmnx(2) = cc_posns(2,n)
           end if
           if (cc_values(n).lt.zmnx(2)) then
             zmnx(2) = cc_values(n)
             izmnx(3) = cc_posns(1,n)
             izmnx(4) = cc_posns(2,n)
           end if
         end do
         call adredt('history','CLEAN',status)
         hpfbwu = usamp
         hpfbwv = vsamp
         beampa = 0.0
         call stxrec( clean_text_header, clean_record, status )
         if (status.ne.0) then
           status = 0
           call enxdsc( desc_list, ndesc, status )
           print *,'***(CLEAN-IMAGE) Number of descriptors = ',ndesc
           do n=1,ndesc
             print *,'***(CLEAN-IMAGE) Descriptor = ',desc_list(n)
           end do
           status = 0
         end if
         call stscal( zmnx, izmnx, status )
         call wrredt( icc, 0, status )
         call wrmapc( icc, cc_posns, cc_values,
     *                number_cc, status )

C .. save residual map
         call ldredt( redt_map, status )
         call io_wrout('CLEAN: writing RESIDUAL map')
         call adredt('created','CLEAN',status)
         call iuv_load(iuv,status)
         call stxrec( clean_text_header, clean_record, status )
         call scnmap( map, iuv, zmnx, izmnx, status )
         call stredt( iuv, 3, status )
         call stscal( zmnx, izmnx, status )
         call wrredt( iresi, 0, status )
         call wrmap( iresi, map, status )
       end if

C restore sources if required
       if (lrest) then

         write(iout,
     *       '(''+CLEAN: RESTORing with '',i5,
     *         '' CLEAN components'',30X)')
     *         number_cc
         call restore_map(map, ixmap, iymap, clean_area,
     *                    beam, ixbeam, iybeam, beam_area,
     *                    cc_values, cc_posns, number_cc,
     *                    status                  )

C .. open the clean map file
         call ldredt( redt_map, status )
         call io_wrout('CLEAN: writing CLEANed map')
         call adredt('history','CLEAN',status)
C .. set beam sizes on CLEANed map
         hpfbwu = beam_size_u
         hpfbwv = beam_size_v
         beampa = beam_pa
         call opemap( iclean, cleaned_map(1:chr_lenb(cleaned_map)),
     *               'WRITE', 0, status )
         call stxrec( clean_text_header, clean_record, status )
         call scnmap( map, iuv, zmnx, izmnx, status )
         call stredt( iuv, 3, status )
         call stscal( zmnx, izmnx, status )
         call wrredt( iclean, 0, status )
         call wrmap( iclean, map, status )
         close ( iclean )
         call mapcat_addtocat(cleaned_map(1:chr_lenb(cleaned_map)),
     *                        .false.,count,status)
       end if

999    close (iresi)
       close (icc)
       end
