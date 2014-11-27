C
C
*+ do_noise_map

       subroutine do_noise_map(map_array,status)
C      -----------------------------------------
C
C Perform a noise analysis of the input map
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C A map is analysed a series of user defined uv-windows. In each
C window the statistics for that region are first found, then in
C an iterative analysis points more than a specified number of
C sigma from the mean are diregarded in future analysis. In this way
C reliable noise and zero levels may be determined even for regions
C of a map dominated by sources. Thought must be given to the meaning
C of the results for those regions which are dominated by side-lobes
C from interfering sources.
*-
       include '../include/anmap_sys_pars.inc'

C Local variables:
C   full uv_range of the map, uv_range to analyse (full), list of
C   UV-regions (sub-division of uv_full)
       integer    full_uv_range(8), uv_full(8), uv_region(4,16)
C   variables used to define regions to analyse
       integer    width_u, width_v, isubu, isubv, isu, isv, iu, iv, i
C   output unit for results, output unit for log, map entry, map pointer
       integer    iout, imap, ip_map
C   results file unit number
       integer    ires
C   results of "scan", integer and reals
       integer    iscan(10)
       real*4     rscan(10)
C   gate above which to reject pixels
       real*4     gate
C   results arrays
       real*4     mean(16), sigma(16), mean_out, sigma_out
       real*4     bar_mean, bar_sigma
       integer    n_region, i_times, nr
C   mini redtape of map
       integer    minirt(8)

C check status
       if (status.ne.0) return

C read map
       call map_getmap('Map : ','default_map','read',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)

C get map region
       call enminirt(minirt,status)
       call enminirt(full_uv_range,status)
       call enminirt(uv_full,status)
       call plot_getuv('Full-UV-range : ','*',uv_full,status)

C read the number of sub-divisions
       call io_geti('Sub-division of range in U (<4) : ','4',isubu,
     *           status)
       call io_geti('Sub-division of range in V (<4) : ','4',isubv,
     *           status)

C check for errors
       if (status.ne.0) then
         call cmd_err(status,'NOISE-MAP',' ')
         goto 999
       end if

C check the number of sub divisions
       if (isubu.gt.4 .or. isubv.gt.4) then
         call cmd_wrerr('NOISE-MAP','Number of sub-divisions > 4')
         goto 999
       end if

C get statistics
       call scnmap(map_array(ip_map),uv_full,rscan,iscan,status)

C read the gateing level
       call io_getr('Gate (pixels > gate*sigma rejected) : ','5.0',
     *           gate,status)

C sub-divide uv-range
       width_u  = (uv_full(2) - uv_full(1))/isubu
       width_v  = (uv_full(3) - uv_full(4))/isubv
       n_region = 0
       do isv = 1,isubv
         iv = uv_full(3) - (isv-1)*width_v
         do isu = 1,isubu
           n_region = n_region + 1
           iu = uv_full(1) + (isu-1)*width_u
           uv_region(1,n_region) = iu
           uv_region(2,n_region) = iu + width_u
           uv_region(4,n_region) = iv - width_v
           uv_region(3,n_region) = iv
         end do
       end do

C prime results arrays
       do nr = 1,n_region
         mean(nr)   = rscan(3)
         sigma(nr)  = rscan(4)
       end do

C loop twice and determine gated statistics
       do i_times = 1,2
         do nr = 1,n_region
           call image_gscan(minirt,map_array(ip_map),
     *                      uv_region(1,nr),gate,mean(nr),sigma(nr),
     *                      mean_out,sigma_out,status)
           mean(nr)  = mean_out
           sigma(nr) = sigma_out
         end do
       end do
       if (status.ne.0) goto 999

C output results
       call io_enqout(iout)
       write(iout,100) (uv_full(i), i=1,4), rscan(3), rscan(4)
100    format(1x/1x,'Noise Analysis per region:'/
     *           1x,'=========================='/1x/
     *           1x,' Full UV-range : ',4i6/
     *           1x,' Mean = ',1PE12.3,'      Sigma = ',1PE12.3/1x/
     *           1x,' Region                      Mean        Sigma'/
     *           1x,'----------------------------------------------'/1x)

C open results file
       call io_opefil(ires,general_results_file,'WRITE',0,status)
       if (status.ne.0) then
         call cmd_err(status,'NOISE-MAP',
     *                 'Error accessing results file')
         goto 999
       end if
       write (ires,*)'%ndata ',n_region
       write (ires,*)'%ncols 6'
       write (ires,*)'%title Noise Map'
       write (ires,*)'%uv-range ',uv_full
       write (ires,*)'%mean ',rscan(3)
       write (ires,*)'%sigma ',rscan(4)
       write (ires,*)'%title-1 U1'
       write (ires,*)'%title-2 U2'
       write (ires,*)'%title-3 V1'
       write (ires,*)'%title-4 V2'
       write (ires,*)'%title-5 Mean'
       write (ires,*)'%title-6 Standard-Deviation'
       do nr=1,n_region
         write(iout,110) (uv_region(i,nr), i=1,4),mean(nr),sigma(nr)
         write(ires,111) (uv_region(i,nr), i=1,4),mean(nr),sigma(nr)
       end do
110    format(1x,4i6,1P2E12.3)
111    format(1x,4i6,1P2E12.3)
       bar_mean  = 0.0
       bar_sigma = 0.0
       do nr=1,n_region
         bar_mean  = bar_mean + mean(nr)/float(n_region)
         bar_sigma = bar_sigma + sigma(nr)/float(n_region)
       end do

       write (iout,120) bar_mean, bar_sigma
120    format (1x/1x,'.. Average MEAN  over the map = ',1PE12.3/
     *            1x,'.. Average SIGMA over the map = ',1PE12.3/)

999    call map_end_alloc(imap,map_array,status)
       call cmd_err(status,'NOISE-MAP','Failed')
       close (ires)

       end
