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
       include    '../include/clean_record.inc'

C local variables
      logical         inter
      character       io_attn_char*1
      character*80    string
      integer         i,j,x,y, out, not_box(4), beam_posn(2,100), npts,
     *                beam_centre(2,100), iprint
      integer         ic, chr_lenb
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
