C
C
*+ plot_do_plot

       subroutine plot_doplot(option,map_array,status)
C      -----------------------------------------------
C
C Plot/display using one of various options, passed via the call to the routine
C
C Given:
C   option
       character*(*)  option
C   image data array
       real*4         map_array(*)
C Updated:
C   error status
       integer        status
*-
       include '../include/anmap_sys_pars.inc'
       include '../include/plt_basic_defn.inc'
       include '../include/plt_image_defn.inc'
       include '/mrao/include/chrlib_functions.inc'


C scratch map and pointers to maps
       integer       iscr, ip_scr, ip_chi, ip_map, i, ii
C logical flags
       logical       all

C check status on entry
       if (status.ne.0) return

C ensure a map is defined
       if (.not.map_defined) then
         call cmd_wrerr('PLOT','No map set')
         return
       end if

C sort out special options
       all = chr_cmatch(option,'all')
       plot_refresh = chr_cmatch(option,'refresh')

C setup display and frame
       call graphic_open( image_defn, status )
       call plot_frinit( .true., status )

       if ( (all.and. .not.image_done) .or.
     *       chr_cmatch(option,'GREY') .or.
     *      (plot_refresh.and.image_done)) then
         call redt_load( imap, status )
         call map_alloc_in( imap, 'DIRECT', map_array, ip_map, status )
         call plot_dogrey(map_array(ip_map),status)
         call map_end_alloc( imap, map_array, status )
         image_done = .true.
       endif


       if ( (all.and. .not.pframe_done) .or.
     *       chr_cmatch(option,'FRAME') .or.
     *      (plot_refresh.and.pframe_done)) then
         call redt_load( imap, status )
         call plot_doframe(status)
         pframe_done = .true.
       endif

       if ( all .or. chr_cmatch(option,'CONTOURS') .or.
     *      (plot_refresh)) then
         ii = imap_current
         do i=1,2
           if ( (i.eq.1 .and. map_defined) .or.
     *          (i.eq.2 .and. overlay_defined .and. overlay_map)) then
             imap_current = i
             call redt_load( imaps(i), status )
             call map_alloc_scr(map_side_size*2, map_side_size*2,
     *                          'DIRECT', iscr, ip_scr, status )
             call map_alloc_in(imaps(i),'DIRECT',map_array,
     *                         ip_map,status)
             call plot_docont(map_array,ip_map,ip_scr,status)
             call map_end_alloc(imaps(i),map_array,status )
             call map_end_alloc(iscr,map_array,status )
           endif
         enddo
         imap_current = ii
       endif


       if ( (all.and. .not.symbol_done) .or.
     *       chr_cmatch(option,'SYMBOLS') .or.
     *      (plot_refresh.and.symbol_done)) then
         call redt_load( imap, status )
         call map_alloc_in( imap, 'DIRECT', map_array, ip_map, status )
         call plot_dosymb(map_array(ip_map),status)
         call map_end_alloc( imap, map_array, status )
         symbol_done = .true.
       endif

       if ( (all.and. .not.vectors_done) .or.
     *       chr_cmatch(option,'VECTORS') .or.
     *      (plot_refresh.and.vectors_done)) then
         call redt_load( imap, status )
         if (vec_chi_map.ne.0 .and. vectors_opt) then
           if (vec_int_map.ne.0 .and. vec_type.ne.2) then
             call map_alloc_in( vec_int_map, 'DIRECT',
     *                          map_array, ip_map, status )
           end if
           call map_alloc_in( vec_chi_map, 'DIRECT',
     *                        map_array, ip_chi, status)
           call plot_dovecs(map_array(ip_chi),map_array(ip_map),status)
           if (vec_int_map.ne.0 .and. vec_type.ne.2) then
             call map_end_alloc( vec_int_map, map_array, status )
           end if
           call map_end_alloc( vec_chi_map, map_array, status )
         end if
         vectors_done = .true.
       endif

       if ( (all.and. .not.text_done) .or.
     *       chr_cmatch(option,'TEXT') .or.
     *      (plot_refresh.and.text_done)) then
         call redt_load( imap, status )
         call plot_dotext(status)
         text_done = .true.
       endif

       if ( (all.and. .not.crosses_done) .or.
     *       chr_cmatch(option,'CROSSES') .or.
     *      (plot_refresh.and.crosses_done)) then
         call redt_load( imap, status )
         call plot_crosses(status)
         crosses_done = .true.
       endif

       if ( (all) .or.
     *       chr_cmatch(option,'ANNOTATIONS') .or.
     *      (plot_refresh)) then
         call annotate_plot( annot_data, option, status )
       endif

C tidyup coordinates on exit to allow for routines changing them
       plot_refresh = .false.
       call plot_frset( status )
       call cmd_err(status,'plot_doplot', ' ')

       end
