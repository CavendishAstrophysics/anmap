C
C
*+ do_fit_data

       subroutine do_fit_data(results,status)
C      --------------------------------------
C
C Fit spectral data with one of a number of models
C
C Returned:
C   results string
       character results*(*)
C   status
       integer   status
C
C N-frequency data is fitted using one of the models supported by
C synch_fit().
C
C PA, 31/03/93
*-
       include '../include/anmap_sys_pars.inc'
       include '/mrao/include/chrlib_functions.inc'
C
C define name of this task
       character  task_name*21
       parameter (task_name = 'FIT-SPECTRUM')
C define maximum number of data points we can handle
       integer    max_data, ndata
       parameter (max_data = 19)
C prompt
       character  prompt*30
C map frequencies
       real*4     freq_data(max_data), flux_data(max_data),
     *            error_data(max_data)
C data returned from fit and a copy for output
       real*4     res_data(4), res(4), synch
C local varaibles used in outputting results
       real*4     x, y
C pointers and counters
       integer    i, iunit
C warning flag used for poor fits
       integer    iwarn
C debug statements
       integer    iopt
C functions
       real*4     synch_fitfun

C check status on entry
       if (status.ne.0) return

C set option for method of fitting
       iopt = 20
       call io_geti('Model-for-fit : ','20',iopt,status)
       if (iopt.eq.13 .or. iopt.eq.130) then
         call io_getr('Non-thermal-spectral-index : ','0.75',
     *                res_data(3),status)
       else if (iopt.eq.14 .or. iopt.eq.140) then
         call io_getr('Minimum-spectral-index : ','0.4',
     *                res_data(3),status)
       end if

C read number of input maps
       call io_geti('Number of input data points : ','3',ndata,status)
C read input data
       do i=1,ndata
         prompt = ' '
         write (prompt,10) i
 10      format('Frequency-',i2.2,' (GHz) : ')
         call io_getr(prompt(1:chr_lenb(prompt)+1),
     *                '0.0',freq_data(i),status)
         prompt = ' '
         write (prompt,11) i
 11      format('Flux-',i2.2,' (mJy) : ')
         call io_getr(prompt(1:chr_lenb(prompt)+1),
     *                '0.0',flux_data(i),status)
         if (iopt.gt.100) then
           prompt = ' '
           write (prompt,12) i
 12        format('Error-',i2.2,' (mJy) : ')
           call io_getr(prompt(1:chr_lenb(prompt)+1),
     *                  '0.0',error_data(i),status)
         endif
         if (status.ne.0) goto 999
       end do

C check status at this stage
       if (status.ne.0) goto 999

C output data to the standard data file
       call io_opefil(iunit,general_data_file,'write',0,status)
       if (status.ne.0) goto 999
       write(iunit,*)'%ndata ',ndata
       write(iunit,*)'%ncols  3 '
       do i=1,ndata
         write(iunit,*) freq_data(i), flux_data(i), error_data(i)
       enddo
       close (iunit)

       call synch_fit(ndata,freq_data,flux_data,error_data,iopt,
     *                      res_data,status)
       call synch_enqwn( iwarn )
       if (iwarn.lt.0 .or. status.ne.0) then
         call io_wrout('***Failed to fit data')
       else
         do i=1,4
           res(i) = res_data(i)
         enddo
         if (iopt.eq.20 .or. iopt.eq.200) then
            synch = synch_fitfun(1.0,20,res,status)
         elseif (iopt.gt.20) then
            res(2) = res(3)
            synch = synch_fitfun(1.0,20,res,status)
         endif
         results = ' '
         write(results,20) synch, (res_data(i), i=2,4)
  20     format(1PE12.5,' ',1PE12.5,' '1PE12.5,' '1PE12.5)
       endif

C output results to the standard results file
       call io_opefil(iunit,general_results_file,'write',0,status)
       write(iunit,*)'%ndata 200'
       write(iunit,*)'%ncols  2 '
       do i=1,200
         x = float(i-1)*freq_data(ndata)/199.0
     *       + 0.5*freq_data(1)
         y = synch_fitfun(x,iopt,res_data,status)
         write(iunit,*) x, y
       enddo
       close (iunit)

999    call iocmd_err(status,task_name,' ')

       end




