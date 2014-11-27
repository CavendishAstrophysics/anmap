*+ clean_image

       subroutine clean_image(residual_map, user_name, map, beam,
     *                        cc_values, cc_posns, cc_list, status )
C      -------------------------------------------------------------
C
C Open the existing residual map file and CLEAN / RESTORE
C
C Given:
C   File name of the residual file
       character*(*)      residual_map
C   User name
       character*(*)      user_name
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

       include      '/mrao/anmap/include/clean_record.inc'
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
       call enmdir( ' ', def_dir, status )
       def_dir = '/home/pa/images'
       call io_makfil( def_dir(1:chr_lenb(def_dir)),
     *              generic_name(1:chr_lenb(generic_name)),
     *              'ccmp', clean_components_map, len_file )
       call io_makfil( def_dir(1:chr_lenb(def_dir)),
     *              generic_name(1:chr_lenb(generic_name)),
     *              'cln', cleaned_map, len_file )
       call dpredt( redt_map, status )
       call iuv_load( uv_map, status )
       call iuv_load( iuv, status )
       ixmap = ixmax
       iymap = iymax
       call rdmap(iresi,map,status)
       if (status.ne.0) then
         call cmd_err(STATUS,'CLEAN','Error on residual: CLEAN failed')
         goto 999
       end if

C .. create the clean map file
       call io_wrout('CLEAN: creating CLEANed map')
       call opemap( iclean, cleaned_map(1:chr_lenb(cleaned_map)),
     *             'WRITE', 0, status )
       call wrredt( iclean, 0, status )
       close (iclean)

C open the clean components map and read
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

       else

         n_beam = 1
         call uv_conv(uv_beam, beam_window, beam_area(1,1))

       end if

       call uv_conv(uv_map, clean_window, clean_area)
       call uv_conv(uv_map, box_window, box_area)
       do n = 1,number_search
         call uv_conv(uv_map, search_window(1,n), search_area(1,n))
       end do

C do CLEAN
       if (lclean) then
         itlim = itlim + number_it_done
         call ldredt( redt_map, status )
         call clean_map ( map, ixmap, iymap,
     *                    clean_area, search_area, box_area,
     *                    beam, ixbeam, iybeam, n_beam, beam_area,
     *                    cc_values, cc_posns, cc_list, number_cc,
     *                    number_it_done, index, status           )

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
         call adredt('owner',user_name,status)
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
         call adredt('owner',user_name,status)
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
         call adredt('owner',user_name,status)
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


c+uv_conv

      subroutine uv_conv( map_uv, window, area )
C     ------------------------------------------
C
C Converts a uv window into a set of x-y coordinates
C
C Given:
C    Map UV limits
              integer     map_uv(4)
C    UV limits of window
              integer     window(4)

C Returned:
C    X-Y coordinates of u-v window
              integer     area(4)

C     NPR     5 July 1988
C-

      area(1) =  window(1) - map_uv(1) + 1
      area(2) =  window(2) - map_uv(1) + 1
      area(3) = -window(3) + map_uv(3) + 1
      area(4) = -window(4) + map_uv(3) + 1
      end
C
C
*+ clean_map

       subroutine clean_map ( map,  m1, m2,
     *                        clean_area, search_area, not_box_area,
     *                        bset, n1, n2, n_beam, beam_area,
     *                        cc_values, cc_posns, cc_list, num_cc,
     *                        num_iter, index, status              )

C     Cleans a specified region of the map with the given beam-set.
C
C     Given:
C         Dimensions of map array and map array
              integer     m1, m2
C         Area of map to clean
              integer     clean_area(4)
C         Search areas
              integer     search_area(4,*)
C         Not-Box area
              integer     not_box_area(4)
C         Beam-set and it's array dimensions
              integer     n1, n2
              real        bset(n1,n2)
C         Number of beams in the beamset and windows on each beam
              integer     n_beam, beam_area(4, n_beam)
C         Index array used in SORT option
              integer     index(1)

C     Updated:
C         Map data
              real        map(m1,m2)
C         Current number of clean components
              integer     num_cc
C         List of clean components (CCs)
              real        cc_values(*)
C         Positions of each CC
              integer*2   cc_posns(2,*)
C         Work space arrays used for compacting CC's
              integer*2   cc_list(*)
C         Current number of iterations
              integer     num_iter

C     Returned
C         Status - must be zero on entry
              integer     status

C
C Perform a number of CLEAN iterations on the map data passed as an
C argument to the routine. Multiple windows are allowed on:
C    search-windows      --      to enable individual sources to be
C                                singled out.
C    beam-window         --      to allow for beam-set CLEANing
C
C PA MRAO, Cambridge
C NPR, MRAO, Cambridge. Modified to allow multiple beam windows.
C PA, MRAO. Modified to use the CLEAN-RECORD and simplify the call
C
*-
       include    '/mrao/anmap/include/clean_record.inc'

C local variables
      logical         inter
      character       io_attn_char*1, job_action*4
      character*80    string
      integer         i,j,x,y, out, not_box(4), beam_posn(2,100), npts,
     *                beam_centre(2,100), iprint
      integer         ic, ijob, chr_lenb
      real            beam_max(100), max, min, mean, sigma
      common /notbox/ not_box

C test status on entry
      if (status .ne. 0) return

C find print frequency -- chech and reset status if required
      call cmd_enqparam( 'report-frequency', string, status )
      if (chr_lenb(string).gt.0) then
        call chr_chctoi( string, iprint, status )
      else
        iprint = 100
      end if
      if (iprint.le.10 .or. status.ne.0) iprint = 100
      status = 0

C set up interaction variables
      call io_enqout(out)
      call io_enqbch( inter )
      inter = .not. inter
      io_attn_char = 'X'

C write out windows used
      write(out,'(/,A,4I6)') ' CLEAN: Cleaning the window:',
     *                         clean_window
      write(out,'(1('' CLEAN: Search window '',I1,''    :'',4I6))')
     *    ( j, ((search_window(i,j)), i=1,4), j = 1, number_search )
      if (lnotbx) then
          do i = 1, 4
              not_box(i) = not_box_area(i)
          end do
      end if

C determine noise on input map
      call noise_map( map, m1, m2,
     *                clean_area(1), clean_area(2),
     *                clean_area(3), clean_area(4),
     *                mean, sigma, npts, status             )
      write(out,601)
     *    ' Dirty  ', mean, sigma
601    format(' CLEAN: Statistics [',A,'] Mean = ',1PE10.2,
     >        ' SD = ',1PE10.2/1X)

C find maximum value and position of each beam
      do i = 1, n_beam
        call maxmin( bset, n1, n2,
     *               beam_area(1,i), beam_area(2,i),
     *               beam_area(3,i), beam_area(4,i),
     *               beam_max(i), beam_posn(1,i), beam_posn(2,i),
     *               min, x, y, status                            )

C define beam centre for beam (where beam "should" be
        if (n_beam.gt.1) then
C .. beam set case
          beam_centre(1,i) = (beam_area(1,i)+beam_area(2,i))/2
          beam_centre(2,i) = (beam_area(3,i)+beam_area(4,i))/2
        else
C .. single beam case
          beam_centre(1,i) = beam_posn(1,i)
          beam_centre(2,i) = beam_posn(2,i)
        end if

C apply Prussian-Hot modification if required
        if (lprhat) then
          call make_phbm(bset,n1,n2,beam_posn(1,i),
     *                   beam_posn(2,i),status)
        end if
      end do


C find position to subtract next clean component from.
      ic = 0
      call find_cc( map, m1, m2,
     *              search_area, number_search, lnotbx,
     *              lsort, sort_depth, sort_inner, sort_gate, index,
     *              max, x, y, ic, status                   )

      do while ( (abs(max)  .gt. fllim ) .and.
     *           (num_iter  .lt. itlim ) .and.
     *           .not.(max.le.0.and.lstop0)     )

*     *           (io_attn_char .ne. 'Q'        ) .and.

C .. if output working information
          if (inter .and. (mod(num_iter,iprint).eq.0)) then
              write(out,'(A,I6,A,1PE9.2)')
     *            '+CLEAN: component: ',num_iter,' maximum: ',max
          end if

C .. find nearest beam
          call nearest_pt( x, y, beam_posn, n_beam, i, status)

C .. subtract from clean position.
          call subtrc( map, m1, m2,
     *                 clean_area(1), clean_area(2),
     *                 clean_area(3), clean_area(4),
     *                 bset, n1, n2,
     *                 beam_area(1,i), beam_area(2,i),
     *                 beam_area(3,i), beam_area(4,i),
     *                 max, x, y,
     *                 beam_max(i), beam_posn(1,i), beam_posn(2,i),
     *                 fract, status                               )
          num_cc = num_cc + 1
          cc_values(num_cc)  = max*fract/beam_max(i)
          cc_posns(1,num_cc) = x + beam_centre(1,i) - beam_posn(1,i)
          cc_posns(2,num_cc) = y + beam_centre(2,i) - beam_posn(2,i)
          num_iter = num_iter + 1

C .. find position of next clean component.
          call find_cc( map, m1, m2,
     *                  search_area, number_search, lnotbx,
     *                  lsort, sort_depth, sort_inner, sort_gate, index,
     *                  max, x, y, ic, status                       )

C .. look for attention if interactive and suspend CLEANing
*          if (inter) then
*              io_attn_char = 'X'
*              call io_getatt(
*     *         '.. CLEAN halted - type c(ontinue) or q(uit) : ',
*     *                     'C', 'QC', io_attn_char, status             )
*          end if
      end do

C . tidy up
      i = num_cc
      call make_ccsl( cc_values, cc_posns, cc_list, num_cc, status )

      if (status .ne. 0) then
          how_ended = status
      else if (num_iter .ge. itlim) then
          how_ended = 1
      else if (max .le. fllim) then
          how_ended = 2
      else if (io_attn_char.eq.'Q') then
          how_ended = 4
      else
          how_ended = 3
      end if
      max_on_resid = max

      call cmd_err(status, 'CLEAN', 'Cleaning Failed; Fatal Error' )

      end


*+find_cc

      subroutine find_cc( map, m, n,
     *                    search_area, n_search, not_box,
     *                    lsort, sort_depth, sort_inner, sort_gate,
     *                    index,
     *                    map_val, x, y, ic, status                )

C     Returns the position to subtract the next clean component.
C
C     Input
C         Map array and bounds
              integer         m, n
              real            map(m,n)
C         Number of search areas and their bounds
              integer         n_search, search_area(4,n_search)
C         Flag set if not-box is being used.
              logical         not_box
C         Sort options
              logical         lsort
              integer         sort_depth, sort_inner
              real*4          sort_gate
C         Work space for sort
              integer         index(1)

C     Updated
C         Inner counter
              integer         ic

C     Returned:
C         Map value and x-y coordinates of next clean-component
              real            map_val
              integer         x, y
C         Error flag
              integer         status
C         Maximum length of index
              integer         max_index

C     NPR  6 July 1988
C     PA  15 September 1988
*-
      real        max, min
      integer     i, i2, k, l, len_index,
     *            x_max, x_min, y_max, y_min

      common /cln_hold_1/ len_index

      if (status.ne.0) return

      if (lsort) then
C .. use sorted list of image maxima for sort_inner iterations

        max_index = 2000
        if (ic.eq.0) then
          len_index = 0
          do i = 1,n_search
            do l = search_area(3,i),search_area(4,i)
              do k = search_area(1,i),search_area(2,i)
                if (abs(map(k,l)).gt.sort_gate) then
                  len_index = len_index + 1
                  index(len_index) = (l-1)*m + k
                end if
                if (len_index.eq.max_index) then
                  call sort_index(map,m*n,sort_depth,index,len_index)
                end if
              end do
            end do
          end do
          call sort_index(map,m*n,sort_depth,index,len_index)
          ic = sort_inner
        end if

        max = -1.0E+30
        do i = 1,len_index
          l  = index(i)/m + 1
          k  = index(i) - (l-1)*m
          if (abs(map(k,l)) .gt. max) then
            max = abs(map(k,l))
            i2 = index(i)
          end if
        end do
        y = i2/m + 1
        x = i2 - (y-1)*m
        map_val = map(x,y)
        ic = ic - 1

      else
C .. normal clean seach for next CC
        max = -1.0E+30
        min =  1.0E+30

        do i = 1, n_search
          if (not_box) then
            call boxmin_map( map, m, n,
     *                       search_area(1,i), search_area(2,i),
     *                       search_area(3,i), search_area(4,i),
     *                       max, x_max, y_max,
     *                       min, x_min, y_min, status           )
          else
            call maxmin_map( map, m, n,
     *                       search_area(1,i), search_area(2,i),
     *                       search_area(3,i), search_area(4,i),
     *                       max, x_max, y_max,
     *                       min, x_min, y_min, status           )
          end if
        end do

        if (abs(max) .ge. abs(min)) then
          map_val = max
          x       = x_max
          y       = y_max
        else
          map_val = min
          x       = x_min
          y       = y_min
        end if

      end if

      end
C
C
*+nearest_pt

      subroutine nearest_pt( x, y, a, n_pts, nearest, status )

C     Finds the nearest point in a given array.

C     Given:
C         Coordinates of given point
              integer     x, y
C         An array of cartesian positions and its size
              integer     n_pts, a(2,n_pts)

C     Returned:
C         Index in array of point nearest to the given one.
              integer     nearest
C     Error flag
              integer     status

C     If n_pts is less than 1 then nearest is returned as zero.
C-

      integer     i, distance, temp

      if (n_pts .le. 0) then
          nearest = 0
      else
          nearest  = 1
          distance = (x-a(1,1))*(x-a(1,1)) + (y-a(2,1))*(y-a(2,1))

          do i = 2, n_pts
              temp = (x-a(1,i))*(x-a(1,i))+(y-a(2,i))*(y-a(2,i))
              if (temp .lt. distance) then
                  distance = temp
                  nearest  = i
              end if
          end do
      end if

      return
      end
C
C
*+ sort_index

       subroutine sort_index(map,m,depth,index,len_index)
C      --------------------------------------------------
C
C Sort and reset the index
C
       integer    m, depth, index(1), len_index, ii, i
       real*4     map(1)
       integer    i1, i2, i3

C sort indexed array
       call qindxr1(map,m,index,len_index)

C find depth entries
       if (len_index.le.depth) then
         return
       end if

       i1 = 1
       i2 = len_index
       i3 = 0

       do while (i3.le.depth)

         if (map(index(i1)).gt.abs(map(index(i2))) ) then
           i1 = i1 + 1
         else
           i2 = i2 - 1
         end if
         i3 = i3 + 1

       end do

C re-write array
       ii = len_index + 1
       do i = i1,i3
         ii = ii - 1
         index(i) = index(ii)
       end do

C reset the length of the index array
       len_index = i3

       end
C
C
*+ restore_map

       subroutine restore_map(map, m1, m2, clean_area,
     *                        beam, n1, n2, beam_area,
     *                        cc_values, cc_posns, ncc,
     *                        status                   )
C      -------------------------------------------------
C
C restore sources to map with specified opions
C
C Given:
C    size of map
       integer      m1, m2
C    map area to restore
       integer      clean_area(4)
C    beam size and array
       integer      n1 ,n2
       real*4       beam(n1,n2)
C    area on beam
       integer      beam_area(4)
C    CC values
       real*4       cc_values(*)
C    CC positions
       integer*2    cc_posns(2,*)
C    number of CC's
       integer      ncc
C Updated:
C    map data
       real*4       map(m1,m2)
C Returned:
C    error status
       integer      status
C
C
C local variables
       integer    im1, im2, jm1, jm2, ib1, ib2, jb1, jb2,
     *            ib3, ib4, jb3, jb4, ibc, jbc, ius1, jus1
       integer    iout, npts
       real*4     xbar, xsd, bmin, bmax, angle, cbsig1, cbsig2

       include '/mrao/anmap/include/clean_record.inc'
       include '/mrao/include/maplib_redtape.inc'

C check status on entry
       if (status.ne.0) return
       im1 = clean_area(1)
       im2 = clean_area(2)
       jm1 = clean_area(3)
       jm2 = clean_area(4)
       ib1 = beam_area(1)
       ib2 = beam_area(2)
       jb1 = beam_area(3)
       jb2 = beam_area(4)

C output unit
       call io_enqout(iout)

C information on residual map
       call noise_map(map,m1,m2,im1,im2,jm1,jm2,xbar,xsd,npts,status)
       write(iout,601) 'Residual-Map',xbar,xsd
C
C find maximum value of beam and position corresponding (IBC,JBC)
       call maxmin(beam,n1,n2,ib1,ib2,jb1,jb2,bmax,ibc,jbc,
     *             bmin,ius1,jus1,status)
C
C Make clean beam.
C Find the noise on the residual map.
C Restore the sources to the residual map.
       ib3=ib1
       ib4=ib2
       jb3=jb1
       jb4=jb2
C
C Make clean beam
       if (ltrbm) then
         write(iout,'(''+'',a,20x)')'CLEAN: truncating dirty beam'
         call make_trbm(beam,n1,n2,bmax,ibc,jbc,ib3,ib4,jb3,jb4,
     *                  flxnrm,status)

       else if (.not.loldbm) then
         write(iout,'(''+'',a,20x)')'CLEAN: making clean beam'
         bmax = 1.0
         ibc = n1/2
         jbc = n2/2
         cbsig1 = beam_size_u*0.4246609/usamp
         cbsig2 = beam_size_v*0.4246609/vsamp
         angle  = beam_pa*3.14159265/180.0
         call make_clbm(beam,n1,n2,cbsig1,cbsig2,angle,
     *                  bmax,ibc,jbc,ib3,ib4,
     *                  jb3,jb4,flxnrm,status)
       end if
C
C Restore removed sources
       write(iout,'(''+'',a,20x)')'CLEAN: restoring sources'
       call restor(cc_values,cc_posns,ncc,map,m1,m2,im1,im2,jm1,jm2,
     *             beam,n1,n2,ib3,ib4,jb3,jb4,bmax,ibc,jbc,status)
C
       call noise_map(map,m1,m2,im1,im2,jm1,jm2,xbar,xsd,npts,status)
       write(iout,601)'Restored-Map',xbar,xsd
       write(iout,602)flxnrm
602    format(1h0,
     >    'CLEAN: Factor for converting sum of numbers to flux :',
     >     2x,f8.5)
       return
C
601    format(' CLEAN: Statistics [',A,'] Mean = ',1PE10.2,
     >        ' SD = ',1PE10.2/1X)
       end
C
C
*+ clean_batch_report

       subroutine clean_batch_report( string )
C      ---------------------------------------
C
C Report progress of clean to user
C
C Given:
C   character string to send
       character*(*)    string
C
C The character string STRING is sent as a message to the user who owns
C the current CLEAN job.
C-
       include '/mrao/include/chrlib_functions.inc'
C
       integer         status
       character*80    comment
       character*80    job_name, job_task, user_name
       common /clean_batch_rep_incl/ job_name, job_task, user_name,
     *                               comment

       status = 0
       call io_wrout(string(1:chr_lenb(string)))

       end



