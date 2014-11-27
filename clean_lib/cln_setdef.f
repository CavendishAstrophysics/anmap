C
C
*+ cln_setdef

       subroutine cln_setdef(full,status)
C      ----------------------------------
C
C reset the CLEAN defaults
C Given:
C   logical flag indicating whether a full initialisation is required
       logical       full
C Returned:
C    error status
       integer       status
C
C Initialise the control information for CLEAN.  If full is set then
C the results information is initialised as well.
*-
       include '../include/clean_record.inc'
       include '/mrao/include/maplib_redtape.inc'

       integer  n

       if (status.ne.0) return

C reset values for parameters
       lnotbx = .false.
       do n=1,4
         box_window(n) = 0
       end do
       if (full) then
         ltrbm = .false.
         lprhat = .false.
         prhat = 0.0
         lclean = .true.
         lrest = .true.
         fllim = 0.001
         fract = 0.2
         itlim = 100
         lsort = .false.
         sort_gate  = 0.0
         sort_depth = 100
         sort_inner = 100
         number_cc  = 0
         times_cleaned = 0
         number_it_done = 0
         max_on_resid = 0.0
         how_ended = 0
       end if

       do n=1,4
         box_window(n) = 0
       end do

       if (map_defined) then

         call ldredt(redt_map,status)
         call iuv_load(clean_window,status)
         number_search = 1
         do n=1,9
           call iuv_load(search_window(1,n),status)
         end do

         call ldredt(redt_beam,status)
         beam_pa = 0.0
         if (hpfbwu.gt.0.0) then
             beam_size_u = hpfbwu
             beam_size_v = hpfbwv
         else
           if (freq.gt.100.0 .and. freq.lt.40000.0) then
             beam_size_u = 2.0*5000.0/freq
           else
             beam_size_u = 1.0
           end if
           beam_size_v = beam_size_u
           beam_size_v = beam_size_v/sin(decmap)
         end if
         call iuv_load(beam_window,status)

       end if

       end
