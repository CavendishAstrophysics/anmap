C
C
*$ Clean-System Support Routines
*  -----------------------------

*+ cln_setmap

       subroutine cln_setmap(large_map,status)
C      ---------------------------------------
C
C Read in a map from stack or disc.
C
C Returned:
C    flag set if CLEAN is defined as a LARGE clean job
       logical     large_map
C    error return
       integer     status
C
C Prompt for map and beam and initialise the clean components map if
C required.
*-
       include '../include/clean_record.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/maplib_redtape.inc'

C local variables
C   general string
       character  string*60, temp*10
C   lengths of character strings
       integer    len_g, len_m
C   UV-range
       integer    iuv(4)
C   unit number for CC file
       integer    icc
C   print control indicator
       integer    iprint
C   dummy values for the clean components
       real*4     cc_values(1), itemp(10)
       integer*2  cc_posns(2,1)
C   logical variable to test for existence of CC-map
       logical    exist, cmd_dblev

C test status on entry
       if (status.ne.0) return

C prompt for map
       large_map = .false.
       call mapcat_getmap('Map-to-CLEAN : ','Default-Map','READ',
     *                    map_cat,status)
       call redt_load(map_cat,status)
       large_map = large_map .or. ixmax*iymax.gt.256*256
       call dpredt(redt_map,status)
       call mapcat_enqsr(map_cat,string,temp,status)
       len_g = chr_ilstc(string,'-') - 1
       generic_name = string(1:len_g)
       if (len_g.eq.0) then
          if (chr_lenb(string).gt.0) then
            generic_name = string
          else
            generic_name = ' '
            write(generic_name,'(A,I4.4)') 'map-entry-',map_cat
          endif
          len_g = chr_lenb(generic_name)
       endif
       if (status.ne.0) goto 999

C prompt for beam
       call mapcat_getmap('Beam : ',
     *                    generic_name(1:len_g)//'-BEAM','READ',
     *                    beam_cat,status)
       call redt_load(beam_cat,status)
       call dpredt(redt_beam,status)
       large_map = large_map .or. ixmax*iymax.gt.256*256
       call mapcat_enqrt(beam_cat,beam_name,itemp,status)
       call ldredt(redt_map,status)
       if (status.ne.0) goto 999

C check for clean components map
       call mapcat_enqdefdir( string, status )
       call io_makfil( string(1:chr_lenb(string)),
     *                 generic_name(1:len_g),
     *                 'ccmp', map_name, len_m )
       len_m = chr_lenb(map_name)
       inquire (file = map_name(1:len_m), exist=exist)
       iprint = 0
       if (exist) then
C .. read the source-list file redtape
         status = 0
         call opemap( icc, map_name(1:len_m), 'READ', 0, status )
         call rdredt( icc, 0, status )
         call enxrec( clean_text_header, clean_record, status )
         close (icc)

       else
C .. create a new source list map
         call ldredt( redt_map, status )
         call iuv_load( iuv, status )
         map_defined = .true.
         call cln_setdef( .true., status )
         call stredt( iuv, -3, status )
         call stxrdt( 1, 1, status )
         call stxrec( clean_text_header, clean_record, status )
         call opemap( icc, map_name(1:len_m), 'WRITE', iprint, status )
         call wrredt( icc, 0, status )
         call wrmapc( icc, cc_posns, cc_values, number_cc, status )
         close (icc)

       end if

C specify file as defined -- makes certain commands accessible
       map_defined = status.eq.0
999    call cmd_err(status,'SET-MAP','Failed')
       end
