C
C
*+ do_fit_maps

       subroutine do_fit_maps(map_array,s)
C      -----------------------------------
C
C Fit spectral map data with one of a number of models
C
C Given:
C   map data
       real*4    map_array(*)
C Returned:
C   status
       integer   s
C
C
C Maps at n-frequencies are fitted using one of the models supported by
C synch_fit().
C
C
C PA, 1/04/93
*-
C
C define name of this task
       character  task_name*8
       parameter (task_name = 'FIT-MAPS')
C define maximum number of maps we can handle
       integer    max_maps
       parameter (max_maps = 10)
C UV-range and map range and counters
       integer    minirt(10), iu, iv
C number of input maps
       integer    input_maps
C input maps and pointers
       integer    in_map(max_maps), in_pointer(max_maps)
C prompt
       character  prompt*8
C map frequencies
       real*4     map_freq(max_maps)
C map gates
       real*4     map_gate(max_maps)
C map errors
       real*4     map_errors(max_maps)
C map blank values
       real*4     blank_value(max_maps)
C polarization code, data type and unit from redtape
       integer    en_poln
       character  en_name*16, en_unit*16
C output maps and pointers
       integer    map1, ip1
       integer    map2, ip2
       integer    map3, ip3
       integer    map4, ip4
C data passed to fitting routine and returned
       real*4     freq_data(max_maps), flux_data(max_maps),
     *            err_data(max_maps), res_data(4)
C pointers and counters
       integer    i, ii, i1
C warning flag used for redtapes and poor fits etc
       integer    iwarn
C count of points rejected:
       integer    ic_OK, ic_not_OK, ic_not3, ic_fail
       integer    iopt
C fixed data values
       real*4     f1
C modelled spectrum
       real*4     synch_fitfun

C check status on entry
       if (s.ne.0) return

C set option for method of fitting
       iopt = 1
       call io_geti('Model-for-fit : ','20',iopt,s)
       if (iopt.eq.13 .or. iopt.eq.130) then
         call io_getr('Non-thermal-spectral-index : ','0.75',
     *                f1,s)
       else if (iopt.eq.14 .or. iopt.eq.140) then
         call io_getr('Minimum-spectral-index : ','0.4',
     *                f1,s)
       end if

C read number of input maps
       call io_geti('Number of input maps : ','3',input_maps,s)
       if (s.ne.0) goto 999

C read input maps and data
       do i=1,input_maps
         write (prompt,10) i
 10      format('Map-',i1,' : ')
         call map_getmap(prompt,'Default-Map','READ',
     *                   in_map(i),s)
         call entype(map_freq(i),en_poln,en_name,en_unit,s)
         call ennull(blank_value(i),s)
         call io_getr('Error (map-units) : ','0.0',map_errors(i),s)
         map_gate(i) = 3.0*map_errors(i)
         call io_getr('Gate-value (map-units) : ','*',
     *             map_gate(i),s)
C .. check redtapes
         iwarn = 2
         if (i.gt.1) then
           call redt_comp(in_map(1),in_map(i),iwarn,s)
           if (s.ne.0. .or. iwarn.gt.5) goto 999
         end if
       end do

C find output maps
       call redt_load(in_map(1),s)
       call enminirt(minirt,s)
       call map_alloc_out(0,0,'DIRECT',map1,ip1,s)
       call map_alloc_out(0,0,'DIRECT',map2,ip2,s)
       if (iopt.ne.11 .and. iopt.ne.110 .and.
     *     iopt.ne.20 .and. iopt.ne.200) then
         call map_alloc_out(0,0,'DIRECT',map3,ip3,s)
       else
         call map_alloc_scr(minirt(5),minirt(6),'DIRECT',map3,ip3,s)
       endif
       call map_alloc_out(0,0,'DIRECT',map4,ip4,s)

C check status at this stage
       if (s.ne.0) goto 999

C read the input maps into core
       do i=1,input_maps
         call  map_alloc_in(in_map(i),'DIRECT',
     *                      map_array,in_pointer(i),s)
       end do

C do the calculation
       do ii = 1,minirt(5)*minirt(6)
         map_array(ii+ip1-1) = blank_value(1)
         map_array(ii+ip2-1) = blank_value(1)
         map_array(ii+ip3-1) = blank_value(1)
         map_array(ii+ip4-1) = 0.0
       end do
       ii = 0
       ic_OK = 0
       ic_not_OK = 0
       ic_not3 = 0
       ic_fail = 0
       do iv = minirt(3),minirt(4),-1
         do iu = minirt(1),minirt(2)
            s = 0
            ii = ii + 1
            i1 = 0
            do i = 1,input_maps
              if (map_array(in_pointer(i)+ii-1).ne.blank_value(i).and.
     *            map_array(in_pointer(i)+ii-1).ge.map_gate(i)) then
                i1 = i1 + 1
                freq_data(i1) = map_freq(i)/1.0E3
                flux_data(i1) = map_array(in_pointer(i)+ii-1)
                err_data(i1)  = map_errors(i)
              end if
            end do
            if (i1.ge.3 .or. 
     *         (i1.ge.2 .and. (iopt.eq.11 .or. iopt.eq.110)) .or.
     *         (i1.ge.2 .and. (iopt.eq.13 .or. iopt.eq.130)) ) then
              res_data(3) = f1
              call synch_fit(i1,freq_data,flux_data,err_data,
     *                              iopt,res_data,s)
              map_array(ip1+ii-1) = res_data(1)
              map_array(ip2+ii-1) = res_data(2)
              map_array(ip3+ii-1) = res_data(3)
              map_array(ip4+ii-1) = res_data(4)
              call synch_enqwn( iwarn )
              if (iwarn.eq.0) then
                if (iopt.eq.20 .or. iopt.eq.200) then
                  map_array(ip1+ii-1) = synch_fitfun(1.0,20,res_data,s)
                elseif (iopt.gt.20) then
                  res_data(2) = res_data(3)
                  map_array(ip1+ii-1) = synch_fitfun(1.0,20,res_data,s)
                endif
                if (s.ne.0) then
                  ic_not_OK = ic_not_OK + 1
                  s = 0
                else
                  ic_OK = ic_OK + 1
                endif
              else
                if (iwarn.lt.0 .or. s.ne.0) then
                  map_array(ip1+ii-1) = blank_value(1)
                  map_array(ip2+ii-1) = blank_value(1)
                  map_array(ip3+ii-1) = blank_value(1)
                  ic_fail = ic_fail + 1
                else
                  if (iopt.gt.20) then
                    res_data(2) = res_data(3)
                    map_array(ip1+ii-1) = 
     *                  synch_fitfun(1.0,20,res_data,s)
                  else if (iopt.eq.20) then
                    map_array(ip1+ii-1) = 
     *                  synch_fitfun(1.0,20,res_data,s)
                  endif
                  map_array(ip4+ii-1) = iwarn
                  if (s.ne.0) then
                    ic_not_OK = ic_not_OK + 1
                    s = 0
                  else
                    ic_OK = ic_OK + 1
                  endif
                endif
              end if
            else
              ic_not3 = ic_not3 + 1
            end if
         end do
       end do


       print *,'No. pixels fitted               = ',ic_OK
       print *,'No. pixels failed to fit        = ',ic_fail
       print *,'No. pixels fit detected as poor = ',ic_not_OK
       print *,'No. pixels rejected from fit    = ',ic_not3

C tidy up, make output map new current map etc.
999    call adredt('created','SYNC-S',s)
       call stnull(blank_value(1),s)
       call map_end_alloc(map1,map_array,s)
       call adredt('created','SYNC-FT2',s)
       call map_end_alloc(map2,map_array,s)
       call adredt('created','SYNC-FT2',s)
       call map_end_alloc(map3,map_array,s)
       call adredt('created','SYNC-ERR',s)
       call map_end_alloc(map4,map_array,s)
       do i = 1,input_maps
         call map_end_alloc(in_map(i),map_array,s)
       end do
C check s value
       call iocmd_err(s,task_name,' ')

       end


