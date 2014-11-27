C
C
*+ synch_fit.f

       subroutine synch_fit(ndata,freq_data,flux_data,err_data,
     *                      iopt,res_data,s                   )
C      --------------------------------------------------------
C
C Fit a theoretical synchrotron spectrum with one of a number of models
C
C Input:
C   Number of data points
       integer     ndata
C   frequency data points
       real*4      freq_data(ndata)
C   flux data points
       real*4      flux_data(ndata)
C   error data points
       real*4      err_data(ndata)
C   option code
       integer     iopt
C Returned:
C   results array
       real*4      res_data(*)
C   status
       integer     s
C
C 
C The supplied data are fitted with a theoretical synchrotron spectrum
C which can take one of a number of forms depending on the value of
C iopt.
C
C iopt      spectral form and minimized function
C --------------------------------------------------------------------
C
C 11        power-law of the form:  S * x**(-alpha)    minimizing SoS
C 110                   "                "             minimizing Chi2
C 12        power-law with thermal component 
C                      S x**(-alpha) + T x**(-0.1)     minimizing SoS
C 120                   "                              minimizing Chi2 
C 13        power-law with thermal component; alpha0 given 
C                      S x**(-alpha0) + T x**(-0.1)    minimizing SoS
C 130                   "                              minimizing Chi2 
C 14        power-law with thermal component; alpha constrained
C                      S x**(-alpha0+a) + T x**(-0.1)  minimizing SoS
C 140                   "                              minimizing Chi2 
C
C 20        current synchrotron spectrum               minimizing SoS
C 200                   "                              minimizing Chi2
C 21        current synchrotron spectrum + T x**(-0.1) minimizing SoS
C 210                   "                              minimizing Chi2
C 22        current synchrotron spectrum + T x**(-0.1) 
C                      with fixed break frequency      minimizing SoS
C 220                   "                              minimizing Chi2
C
C 31        as 21 except break >= max_frequency for
C                      flat spectra                    minimizing SoS
C 310                   "                              minimizing Chi2
C 32        as 21 except use power-laws for flat spectra
C                                                      minimizing SoS
C 320                   "                              minimizing Chi2
C 33        as 21 except use power-laws for flat spectra
C                      and break >= max_frequency for
C                      flat, but steepening spectrum   minimizing SoS
C 330                   "                              minimizing Chi2
*-

C define local data variables passed in common to the internal routine
       real*8                    x_data(20), y_data(20), e_data(20)
       common /local_synch_fit/  x_data,     y_data,     e_data
       real*8                    con(20)
       common /local_synch_con/  con

C variables used by NAG -- X holds results
       integer       lrwork, liwork
       parameter    (lrwork=300, liwork=100)
       real*8        x(4), r_work(lrwork), fsumsq
       integer       ifail, i_work(liwork)
       real*8        synch_bfun8, aa, xs

C parameters describing spectral type
       integer       spectral_type
       real*4        injection_index_E, alpha

C loop counter, output unit number and number of model parameters
       integer       n, nn, iout, nx, nm

C flags to indiacte a local change of mode
       integer       ioptl

C flag used in ordering of the data
       integer       io(20)
       real*4        fmin

C logical variable used to test for input data which could be fitted by
C the spectrum as defined
       logical       test_OK, test_steep

C local variable used as a flag when fitwas not perfect or data do not
C obey tests before fitting -- this is set only when an error state
C does NOT occur
       integer                    local_warn
       common  /local_synch_warn/ local_warn


       include '/mrao/anmap/include/synch_errors.inc'

C check status on entry and initialise local warning flag
       if (s.ne.0) return
       if (ndata.le.1) then
         local_warn = -1
         return
       else
         local_warn = 0
       endif
       ioptl = iopt

C prepare data; scale so that values are within a sensible range
       test_OK = .true.
       do n=1,ndata
         if (freq_data(n).le.0.0 .or. flux_data(n).le.0.0) then
           test_OK = .false.
         endif
         if (iopt.gt.100) then
           if (err_data(n).le.0.0) then
             test_OK = .false.
           endif
         endif
         io(n) = n
       enddo
       if (.not.test_OK) then
         local_warn = -1
         return
       end if

C order and scale data
       do n=1,ndata
         fmin = 1.0E+30
         do nn=1,ndata
           if (io(nn).ne.0) then
             if (freq_data(nn).lt.fmin) then
               fmin = freq_data(nn)
               nx = nn
             endif
           endif
         enddo
         io(nx) = 0
         x_data(n) = freq_data(nx)
         y_data(n) = flux_data(nx)
         e_data(n) = err_data(nx)
         if (n.eq.1) then
           nm = nx
         endif
       enddo
       if (iopt.gt.100) then
         do n=1,ndata
           e_data(n) = e_data(n)/y_data(1)
         enddo
       endif
       do n=ndata,1,-1
         x_data(n) = x_data(n)/x_data(1)
         y_data(n) = y_data(n)/y_data(1)
       end do
       if (.not.test_ok) then
         local_warn = -1
         return
       end if

C find spectral range
       call synch_enqsp(injection_index_E,spectral_type)

C look for standard steep spectrum -- useful for all modes
       test_OK = .false.
       do n=1,ndata-1
         do nn=n+1,ndata
           alpha = - log(y_data(nn)/y_data(n))/
     *               log(x_data(nn)/x_data(n))
           test_OK = test_OK .or. (2.0*alpha+1.0).gt.injection_index_E
         end do
       end do
C look for steepening spectrum
       test_steep = .true.
       aa = 0.0
       do n=1,ndata-1
         nn = n + 1
         alpha = - log(y_data(nn)/y_data(n))/
     *             log(x_data(nn)/x_data(n))
         test_steep = test_steep .and. alpha.gt.aa
         aa = alpha
       enddo

C setup trial spectral index
       aa = -log(flux_data(2))/log(freq_data(2))

C initialise for call to NAG fitting routine
       if (iopt.eq.11 .or. iopt.eq.110) then
         nx = 2
         x(1) = y_data(1)
         x(2) = aa
       elseif (iopt.eq.12 .or. iopt.eq.120) then
         nx = 3
         if (ndata.lt.3) then
           local_warn = -1 
           return
         endif
         if (test_OK) then
           x(1) = y_data(1)
           x(3) = aa
           x(2) = 0.0
         else
           x(1) = y_data(1)
           x(3) = aa
           x(2) = y_data(ndata)
         endif
       elseif (iopt.eq.13 .or. iopt.eq.130) then
         nx = 2
         if (test_OK) then
           x(1) = y_data(1)
           x(2) = 0.0
         else
           x(1) = y_data(1)
           x(2) = y_data(ndata)
         endif
         con(1) = res_data(3)
       elseif (iopt.eq.14 .or. iopt.eq.140) then
         nx = 3
         if (ndata.lt.3) then
           local_warn = -1 
           return
         endif
         if (test_OK) then
           x(1) = y_data(1)
           x(3) = aa
           x(2) = 0.0
         else
           x(1) = y_data(1)
           x(3) = aa
           x(2) = y_data(ndata)
         endif
         con(1) = res_data(3)
       elseif (iopt.eq.20 .or. iopt.eq.200) then
         call synch_enqlim( con(1) )
         con(1) = con(1)*0.8D+0
         nx = 2
         if (test_OK) then
           x(2) = x_data(ndata)
           xs = x_data(1)/x(2)
           x(1) = y_data(1)*synch_bfun8(1.0D+0,s)/
     *                      synch_bfun8(xs,s)
         else
           local_warn = 100
           return
         endif
       elseif (iopt.eq.21 .or. iopt.eq.210) then
         nx = 3 
         call synch_enqlim( con(1) )
         con(1) = con(1) * 0.8D+0
         if (test_OK) then
           x(3) = x_data(ndata)
           xs = x_data(1)/x(3)
           x(1) = y_data(1)*synch_bfun8(1.0D+0,s) /
     *                      synch_bfun8(xs,s)
           x(2) = 0.0D+0
         else
           x(3) = 2.0D+0*x_data(ndata)
           xs = x_data(1)/x(3)
           x(1) = y_data(1)*synch_bfun8(1.0D+0,s) /
     *                          synch_bfun8(xs,s)
           x(2) = y_data(ndata)
         endif
       elseif (iopt.eq.22 .or. iopt.eq.220) then
         nx = 2 
         call synch_enqlim( con(1) )
         con(1) = res_data(3)/freq_data(nm)
         if (test_OK) then
           xs = x_data(1)/con(1)
           x(1) = y_data(1)*synch_bfun8(1.0D+0,s) /
     *                      synch_bfun8(xs,s)
           x(2) = 0.0D+0
         else
           xs = x_data(1)/con(1)
           x(1) = y_data(1)*synch_bfun8(1.0D+0,s) /
     *                          synch_bfun8(xs,s)
           x(2) = y_data(ndata)
         endif

       elseif (iopt.eq.31 .or. iopt.eq.310) then
         nx = 3 
         ioptl = 21
         call synch_enqlim( con(1) )
         con(1) = con(1) * 0.8D+0
         if (test_OK) then
           x(3) = x_data(ndata)
           xs = x_data(1)/x(3)
           x(1) = y_data(1)*synch_bfun8(1.0D+0,s) /
     *                      synch_bfun8(xs,s)
           x(2) = 0.0D+0
         else
           con(1) = 1.0D+0
           x(3) = 2.0D+0*x_data(ndata)
           xs = x_data(1)/x(3)
           x(1) = y_data(1)*synch_bfun8(1.0D+0,s) /
     *                          synch_bfun8(xs,s)
           x(2) = y_data(ndata)
         endif

       elseif (iopt.eq.32 .or. iopt.eq.320) then
         nx = 3 
         call synch_enqlim( con(1) )
         con(1) = con(1) * 0.8D+0
         if (test_OK) then
           ioptl = 21
           x(3) = x_data(ndata)
           xs = x_data(1)/x(3)
           x(1) = y_data(1)*synch_bfun8(1.0D+0,s) /
     *                      synch_bfun8(xs,s)
           x(2) = 0.0D+0
         else
           ioptl = 22
           con(1) = 100.0*x_data(ndata)
           xs = x_data(1)/con(1)
           x(1) = y_data(1)*synch_bfun8(1.0D+0,s) /
     *                          synch_bfun8(xs,s)
           x(2) = y_data(ndata)
         endif

       elseif (iopt.eq.33 .or. iopt.eq.330) then
         nx = 3 
         call synch_enqlim( con(1) )
         con(1) = con(1) * 0.8D+0
         if (test_OK) then
           ioptl = 21
           x(3) = x_data(ndata)
           xs = x_data(1)/x(3)
           x(1) = y_data(1)*synch_bfun8(1.0D+0,s) /
     *                      synch_bfun8(xs,s)
           x(2) = 0.0D+0
         else
           if (test_steep) then
             ioptl = 21
             con(1) = 1.0D+0
             x(3) = 2.0D+0*x_data(ndata)
             xs = x_data(1)/x(3)
             x(1) = y_data(1)*synch_bfun8(1.0D+0,s) /
     *                            synch_bfun8(xs,s)
             x(2) = y_data(ndata)
           else
             ioptl = 22
             con(1) = 100.0*x_data(ndata)
             xs = x_data(1)/con(1)
             x(1) = y_data(1)*synch_bfun8(1.0D+0,s) /
     *                            synch_bfun8(xs,s)
             x(2) = y_data(ndata)
           endif
         endif

       end if

C initialise fitting mode and do the fit
       call synch_fitmode( ioptl )
       ifail = 1
       call e04fdf(ndata,nx,x,fsumsq,i_work,
     *             liwork,r_work,lrwork,ifail)

C check IFAIL on exit
       local_warn = 0
       if (ifail.eq.1) then
         call io_enqout(iout)
         call io_wrout('***(SYNCH_FIT) Error calling e04fdf')
         write(iout,*) '***(SYNCH_FIT) non-zero IFAIL = ',ifail
         s = ill_synNAG
         return
       else if (ifail.eq.2) then
         local_warn = 3000
       else if (ifail.eq.3) then
         local_warn = 2000
       else if (ifail.ge.4) then
         local_warn = 1000
       end if
       do n=1,3
         res_data(n) = abs(x(n))
       enddo
       res_data(4) = sqrt(fsumsq)
       res_data(1) = res_data(1) * flux_data(nm)

       if (ioptl.eq.11 .or. ioptl.eq.110) then
         res_data(1) = res_data(1)*(freq_data(nm)**res_data(2))
         res_data(2) = x(2)
         res_data(3) = 0.0
       endif
       if (ioptl.eq.13 .or. ioptl.eq.130 ) then
         res_data(3) = con(1)
       endif
       if (ioptl.eq.14 .or. ioptl.eq.140 ) then
         res_data(3) = con(1) + res_data(3)
       endif
       if (ioptl.eq.12 .or. ioptl.eq.120 .or.
     *     ioptl.eq.13 .or. ioptl.eq.130  .or.
     *     ioptl.eq.14 .or. ioptl.eq.140 ) then
         res_data(1) = res_data(1)*(freq_data(nm)**res_data(3))
         res_data(2) = res_data(2)*flux_data(nm)*(freq_data(nm)**0.1)
       endif
       if (ioptl.eq.22 .or. ioptl.eq.220 ) then
         res_data(3) = con(1)
       endif
       if (ioptl.eq.21 .or. ioptl.eq.210 .or.
     *     ioptl.eq.22 .or. ioptl.eq.220 ) then
         res_data(2) = res_data(2)*flux_data(nm)*(freq_data(nm)**0.1)
         res_data(3) = res_data(3)*freq_data(nm)
       endif

       if (ioptl.eq.20 .or. ioptl.eq.200) then
         res_data(2) = res_data(2)*freq_data(nm)
       endif
C check error on exit
       call iocmd_err(s,'synch_fit','Failed')
       end
C
C
*+ lsfun1
C
       subroutine lsfun1(ndata,nres,x,fvecc)
C      -------------------------------------
C
C Local routine used by NAG for fitting purposes
C
       integer     ndata, nres, n, status
       real*8      fvecc(ndata), x(nres)
       real*8      x_data(20), y_data(20), e_data(20)
       real*8      x_point, y_point
       real*8      synch_bfun8
       integer     mode, icount
       common /local_synch_fit/   x_data, y_data, e_data
       common /local_lsfun1_mode/ mode, icount
       real*8                     con(20)
       common /local_synch_con/   con

       status = 0
       if (mode.eq.11 .or. mode.eq.110) then
         do n=1,ndata
           fvecc(n) =
     *        abs(x(1)) * (x_data(n)**(-x(2))) -
     *        y_data(n)
           if (mode.eq.110) then
             fvecc(n) = fvecc(n) / e_data(n)
           endif
         end do
         icount = icount + 1

       else if (mode.eq.12 .or. mode.eq.120) then
         do n=1,ndata
           fvecc(n) =
     *        abs(x(1)) * (x_data(n)**(-abs(x(3)))) +
     *        abs(x(2))*(x_data(n)**(-0.1D+0)) -
     *        y_data(n)
           if (mode.eq.120) then
             fvecc(n) = fvecc(n) / e_data(n)
           endif
         end do
         icount = icount + 1

       else if (mode.eq.13 .or. mode.eq.130) then
         do n=1,ndata
           fvecc(n) =
     *        abs(x(1)) * (x_data(n)**(-con(1))) +
     *        abs(x(2))*(x_data(n)**(-0.1D+0)) -
     *        y_data(n)
           if (mode.eq.130) then
             fvecc(n) = fvecc(n) / e_data(n)
           endif
         end do
         icount = icount + 1

       else if (mode.eq.14 .or. mode.eq.140) then
         do n=1,ndata
           fvecc(n) =
     *        abs(x(1)) * (x_data(n)**(-(con(1)+abs(x(3))))) +
     *        abs(x(2))*(x_data(n)**(-0.1D+0)) -
     *        y_data(n)
           if (mode.eq.140) then
             fvecc(n) = fvecc(n) / e_data(n)
           endif
         end do
         icount = icount + 1

       else if (mode.eq.20 .or. mode.eq.200) then
         do n=1,ndata
           x_point = x_data(n) / max(abs(x(2)),x_data(ndata)/con(1))
           y_point = abs(x(1)) * synch_bfun8(x_point,status) / 
     *                           synch_bfun8(1.0D+0,status) 
           fvecc(n)= log(y_point) - log(y_data(n))
           if (mode.eq.200) then
             fvecc(n) = fvecc(n) * y_data(n) / e_data(n)
           endif
         end do
         icount = icount + 1

       else if (mode.eq.21 .or. mode.eq.210) then
         do n=1,ndata
           x_point = x_data(n) / max(abs(x(3)),x_data(ndata)/con(1))
           y_point = abs(x(1)) * synch_bfun8(x_point,status) / 
     *                           synch_bfun8(1.0D+0,status) +
     *               abs(x(2))*(x_data(n)**(-0.1D+0))
           fvecc(n)= log(y_point) - log(y_data(n))
           if (mode.eq.210) then
             fvecc(n) = fvecc(n) * y_data(n) / e_data(n)
           endif
         end do
         icount = icount + 1

       else if (mode.eq.22 .or. mode.eq.220) then
         do n=1,ndata
           x_point = x_data(n) / con(1)
           y_point = abs(x(1)) * synch_bfun8(x_point,status) / 
     *                           synch_bfun8(1.0D+0,status) +
     *               abs(x(2))*(x_data(n)**(-0.1D+0))
           fvecc(n)= log(y_point) - log(y_data(n))
           if (mode.eq.220) then
             fvecc(n) = fvecc(n) * y_data(n) / e_data(n)
           endif
         end do
         icount = icount + 1
       end if
       call iocmd_err(status,'LSFUN1 -- synch_fit',' ')
       end
C
C
*+
       real function synch_fitfun( x, iopt, res, s )
C      ---------------------------------------------
C
C Return the value of the fitted synchrotron spectrum at x
C
C Given:
C   frequency
       real*4        x
C   spectral type
       integer       iopt
C   results data
       real*4        res(*)
C Updated:
C   error status
       integer       s
C
C-
       real*4      synch_bfun

       if (s.ne.0) return

       if (iopt.eq.11 .or. iopt.eq.110) then
         synch_fitfun = res(1)*(x**(-res(2)))

       elseif (iopt.eq.12 .or. iopt.eq.120 .or.
     *         iopt.eq.13 .or. iopt.eq.130 .or. 
     *         iopt.eq.14 .or. iopt.eq.140 ) then 
         synch_fitfun = res(1)*(x**(-res(3))) + 
     *                  res(2)*(x**(-0.1))

       elseif (iopt.eq.20 .or. iopt.eq.200) then
         s = 1
         synch_fitfun = res(1)*synch_bfun(x/res(2),s)/
     *                         synch_bfun(1.0,s)

       elseif (iopt.eq.21 .or. iopt.eq.210 .or. 
     *         iopt.eq.22 .or. iopt.eq.220 .or.
     *         iopt.eq.31 .or. iopt.eq.310 .or.
     *         iopt.eq.32 .or. iopt.eq.320 .or.
     *         iopt.eq.33 .or. iopt.eq.330 .or.
     *         iopt.eq.34 .or. iopt.eq.340 ) then
         s = 1
         synch_fitfun = res(1)*synch_bfun(x/res(3),s)/
     *                         synch_bfun(1.0,s) + 
     *                  res(2)*(x**(-0.1))

       endif
       end



