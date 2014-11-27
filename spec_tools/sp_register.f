C REGISTER: perform a minimization of scaling and shit parameters for
C           one spectrum to register it as closely as possible with
C           a second spectrum.
C
       implicit    NONE
C
C This routine will attempt to register two spectra as closely as possible.
C The axes with which the registration will be attempted may be specified 
C and the parameters (shift and scale) to do the registration may also
C be controlled; optionally the registration can be done in log-space. 
C
C Parameters:
C    outfile   ==  output file name
C    infile    ==  input file name
C    model     ==  model file against which registration is done
C    xc        ==  X-column in the infile 
C    xcm       ==  X-column in the model file
C    yc        ==  column to register
C    ycm       ==  Y-column in the model file to register against
C    title     ==  new title for output file
C    comment   ==  user defined comment text
C    logx      ==  specify a registration using log( XC ) values
C    logy      ==  specify a registration using log( YC ) values
C    scale     ==  request registration uses scaling
C    shift     ==  request registration uses shifting
C    mode      ==  specify minimization mode
C                  minimize (y(i) - f(x(i)))**2/(f(x(i))**mode)
C                  
C
C (shift and scale apply AFTER taking of logarithms if log/logy is
C  specified).
C
C Include information on standard array sizes etc.
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'
C Include standard functions
       include '/mrao/include/chrlib_functions.inc'

C
C Local variables
C   file identifiers
       integer    id_to, id_from, id_model
C   status return
       integer    status
C   number of data points
       integer    nd_from, nd_model
C   array to hold data values
       real*4     x(max_ndata), x_model(max_ndata),
     *            y(max_ndata), y_model(max_ndata)
C   local variables used to hold information and do the interpolation
       real*4     xmin, xmax
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   x-column / y-column
       integer    xc, xcm, yc, ycm
C   logical to test for setting of input parameters
       logical    test
C   parameters indicating type of registration to perform
       integer    mode
       logical    logx, logy, scale, shift
       common /fit_control/ logx, logy, scale, shift, mode
C   counters
       integer    n, n1, n2, nn
C Parameters used by NAG minimization
       integer    ifail, len_iwork, len_work, npts, nvals
       parameter (len_iwork = 100, len_work = 10000)
       real*8     fsumsq, work(len_work), fit(2)
       real*4     sum0, sum1, sum2
       integer    iwork(len_iwork)
       real*8     x_fit(max_ndata), y_fit(max_ndata)
       common /fit_data/ x_fit, y_fit


C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'register',
     *      dir(1:chr_lenb(dir))//tool_definitions, status)
C find input file names and allocate files
       call cmd_items( 'infile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_from, status)
       call cmd_items( 'model', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'read', id_model, status)
C find output file name and allocate
       call cmd_items( 'outfile', file, status)
       call spec_allocate( file(1:chr_lenb(file)),
     *                     'write', id_to, status)
C copy output descriptors; do this from the model to the output
       call spec_hd_copy( id_model, id_to, status )

C read control data
C .. get the x-column
       call cmd_itemi( 'xc', 1, xc, status )
C .. find model x-column if necessary
       call cmd_itemset( 'xcm', test, status )
       if (test) then
         call cmd_itemi( 'xcm', 1, xcm, status )
       else
         xcm = xc
       endif
C .. get the y-column
       call cmd_itemi( 'yc', 1, yc, status )
C .. find model y-column if necessary
       call cmd_itemset( 'ycm', test, status )
       if (test) then
         call cmd_itemi( 'ycm', 1, xcm, status )
       else
         ycm = yc
       endif
C .. setup options for the registration
       logx = .false.
       logy = .false.
       shift = .false.
       scale = .false.
       call cmd_itemset( 'logx', logx, status)
       call cmd_itemset( 'logy', logy, status)
       call cmd_itemset( 'scale', scale, status)
       call cmd_itemset( 'shift', shift, status)
       call cmd_itemi( 'mode', 1, mode, status)

C .. read the x-column from the model and input files
       call spec_get_data( id_from, xc, x, nd_from, status )
       call spec_get_data( id_from, yc, y, nd_from, status )
       call spec_get_data( id_model, xcm, x_model, nd_model, status )
       call spec_get_data( id_model, ycm, y_model, nd_model, status )
C .. setup for "log" options
       if (logx) then
         test = .true.
         n = 1
         do while (test .and. n.le.nd_from)
           test = x(n).gt.0.0
           if (test) then
             x(n) = log(x(n))
           endif
           n = n + 1
         enddo
         n = 1
         do while (test .and. n.le.nd_model)
           test = x_model(n).gt.0.0
           if (test) then
             x_model(n) = log(x_model(n))
           endif
           n = n + 1
         enddo
         if (.not. test) then
           print *,'*** Illegal data in "x-axis" for log options'
           goto 999
         endif
       endif
       if (logy) then
         test = .true.
         n = 1
         do while (test .and. n.le.nd_from)
           test = y(n).gt.0.0
           if (test) then
             y(n) = log(y(n))
           endif
           n = n + 1
         enddo
         n = 1
         do while (test .and. n.le.nd_model)
           test = y_model(n).gt.0.0
           if (test) then
             y_model(n) = log(y_model(n))
           endif
           n = n + 1
         enddo
         if (.not. test) then
           print *,'*** Illegal data in "y-axis" for log options'
           goto 999
         endif
       endif
      
C .. determine and store range of input data; check also that the input
C    and model data are in ascending order -- needed for the interpolation
       xmin = 1.0E+30
       xmax = -1.0E+30
       test = .true.
       n = 1
       xmin = x(1) - 1.0
       do while (test .and. n.le.nd_from)
         test = x(n).gt.xmin
         xmin = x(n)
         n = n + 1
       enddo
       n = 1
       xmin = x_model(1) - 1.0
       do while (test .and. n.le.nd_model)
         test = x_model(n).ge.xmin
         xmin = x_model(n)
         n = n + 1
       enddo
       if (.not.test) then
         print *,'*** Input data and/or model not in ascending order'
         print *,'*** X-columns specified as : ',xc,xcm
         goto 999
       endif
       xmin = x(1)
       xmax = x(nd_from)
C .. add user text to header       
       call cmd_items( 'title', string, status )
       if (chr_lenb(string).gt.0) then
         call spec_hd_set( id_to, 'title',
     *                     string(1:chr_lenb(string)), status )
       end if
       call cmd_items( 'comment', string, status )
       if (chr_lenb(string).gt.0) then
         call spec_hd_set( id_to, 'comment',
     *                     string(1:chr_lenb(string)), status )
       end if
       if (status.ne.0) goto 999

C interpolate onto a common axis
       n1 = 1
       nn = 0
       do n=1,nd_model
         if (x_model(n).ge.xmin .and. x_model(n).le.xmax) then
            nn = nn + 1
            do while (x(n1).lt.x_model(n))
              n1 = n1 + 1
            enddo
            n1 = n1 - 1
            n2 = n1
            do while (x(n2).lt.x_model(n))
              n2 = n2 + 1
            enddo
            y_fit(nn) = y(n1) +
     *              (x_model(n)-x(n1))*(y(n2)-y(n1))/(x(n2)-x(n1))
            x_fit(nn) = y_model(n)
         endif
       enddo
       if (status.ne.0) goto 999

C register the data according to the chosen scheme
       fit(1) = 0.0
       fit(2) = 1.0
       if (shift.and.scale) then
         fit(1) = 0.0
         fit(2) = 1.0  
         nvals = 2
       elseif (shift) then  
         fit(1) = 0.0
         nvals = 1       
       elseif (scale) then  
         fit(1) = 1.0
         nvals = 1
       else 
         nvals = 0
       endif
       npts = nn
       if (nvals.gt.0 .and. npts.gt.nvals) then
          ifail = 1      
          call e04fdf(npts, nvals, fit, fsumsq,
     *                iwork, 100, work, len_work, ifail)
          if (ifail.ne.0 .and. ifail.lt.5) then
             print *,'***(REGISTER) Ifail non-zero in NAG = ',ifail
             if (ifail.eq.1) goto 999
          end if
C .. modify output
          if (shift.and.scale) then
            do n=1,nd_from
              y(n) = (y(n) + fit(1))*fit(2)
            enddo  
            do n=1,npts
              y_fit(n) = (y_fit(n) + fit(1))*fit(2)
            enddo  
          elseif (shift) then
            do n=1,nd_from
              y(n) = (y(n) + fit(1))
            enddo   
            do n=1,npts
              y_fit(n) = (y_fit(n) + fit(1))
            enddo  
          elseif (scale) then
            do n=1,nd_from
              y(n) = y(n)*fit(1)
            enddo 
            do n=1,npts
              y_fit(n) = y_fit(n)*fit(1)
            enddo     
          endif
          if (logy) then
            do n=1,npts
              x_fit(n) = exp(x_fit(n))
              y_fit(n) = exp(y_fit(n))
            enddo   
          endif
C .. calculate "goodness" of fit
          sum0 = 0.0
          sum1 = 0.0
          sum2 = 0.0
          do n=1,npts
            sum0 = sum0 + (x_fit(n)-y_fit(n))**2
            sum1 = sum1 + ((x_fit(n)-y_fit(n))**2)/x_fit(n)
            sum2 = sum2 + ((x_fit(n)-y_fit(n))**2)/(x_fit(n)**2)
          enddo
       endif

C set data to standard output form
       if (logx) then
         do n=1,nd_from
           x(n) = exp(x(n))
         enddo
       endif
       if (logy) then
         do n=1,nd_from
           y(n) = exp(y(n))
         enddo
       endif

C write output data
       call spec_put_data( id_to, xc, x, nd_from, status)
       call spec_put_data( id_to, yc, y, nd_from, status)

C report results to the standard output
       if (shift.and.scale) then
         print 10,fit(1),fit(2)
  10     format(' Fitted with SHIFT and SCALE : ',1PE12.3,1PE12.3)
       elseif (shift) then  
         print 11,fit(1)
  11     format(' Fitted with SHIFT : ',1PE12.3)   
       elseif (scale) then  
         print 12,fit(1)
  12     format(' Fitted with SCALE : ',1PE12.3) 
       endif
       if (logx) then
         print 13,'Used Logarithm of x-axis in fit'
  13     format(' ',A)
       endif
       if (logy) then
         print 14,'Used Logarithm of y-axis in fit'
  14     format(' ',A)
         if (shift) then
           print 15,exp(fit(1))
         endif
  15     format(' SHIFT corresponds to a scale of : ',1PE12.3)       
       endif
       print 16,sum0,sum1,sum2
  16   format(' Goodness of fit: '/
     *        ' Sum[ (x(i)-y(i))**2 )           ] = ',1PE12.3/
     *        ' Sum[ (x(i)-y(i))**2 ) / x(i)    ] = ',1PE12.3/
     *        ' Sum[ (x(i)-y(i))**2 ) / x(i)**2 ] = ',1PE12.3 )


C add history items
       string = ' '
       write(string,'(A,2I2)')
     *       'REGISTER: xc/xcm = ',xc,xcm
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )
       if (logy) then
         if (shift) then
           string = ' '
           write(string,'(1PE12.3)') exp(fit(1))
           call spec_hd_set( id_to, 'scale',
     *                       string(1:chr_lenb(string)),
     *                       status )
           string = ' '
           write(string,'(1PE12.3)') exp(fit(2))
           call spec_hd_set( id_to, 'power',
     *                       string(1:chr_lenb(string)),
     *                       status )
         else
           string = '1.0'
           call spec_hd_set( id_to, 'scale',
     *                       string(1:chr_lenb(string)),
     *                       status )
           string = ' '
           write(string,'(1PE12.3)') exp(fit(1))
           call spec_hd_set( id_to, 'power',
     *                       string(1:chr_lenb(string)),
     *                       status )
         endif
       else
         if (shift) then
           string = ' '
           write(string,'(1PE12.3)') fit(1)
           call spec_hd_set( id_to, 'shift',
     *                       string(1:chr_lenb(string)),
     *                       status )
           string = ' '
           write(string,'(1PE12.3)') fit(2)
           call spec_hd_set( id_to, 'scale',
     *                       string(1:chr_lenb(string)),
     *                       status )
         else
           string = '0.0'
           call spec_hd_set( id_to, 'shift',
     *                       string(1:chr_lenb(string)),
     *                       status )
           string = ' '
           write(string,'(1PE12.3)') fit(1)
           call spec_hd_set( id_to, 'scale',
     *                       string(1:chr_lenb(string)),
     *                       status )
         endif
       endif

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_model, status )
       call spec_deallocate( id_to, status )

       end
C
C routine used by nag : LSFUN1

       subroutine LSFUN1( npts, nvals, fit, f )
C
C Routine required by NAG -- see definition in NAG manual for details
C
*-
C Include information on standard array sizes etc.
       include '../include/spec_global.inc'
       include '../include/spec_pars.inc'

       integer    nvals, npts
       real*8     fit(nvals), f(npts)

C control information and data passed in common 
       integer    mode
       logical    logx, logy, scale, shift
       common /fit_control/ logx, logy, scale, shift, mode
       real*8     x_fit(max_ndata), y_fit(max_ndata)
       common /fit_data/ x_fit, y_fit
       real*8     xmode

C counters
       integer    n

       if (mode.eq.0) then
         if (shift.and.scale) then
           do n=1,npts
             f(n) = x_fit(n) - (y_fit(n)+fit(1))*fit(2)
           enddo
         elseif (shift) then
           do n=1,npts
             f(n) = x_fit(n) - (y_fit(n)+fit(1))
           enddo
         elseif (scale) then
           do n=1,npts
             f(n) = x_fit(n) - y_fit(n)*fit(1)
           enddo
         endif
       else
         xmode = float(mode)/2.0D+0
         if (shift.and.scale) then
           do n=1,npts
             f(n) = (x_fit(n) - (y_fit(n)+fit(1))*fit(2)) /
     *              (abs(x_fit(n)))**xmode   
           enddo
         elseif (shift) then
           do n=1,npts
             f(n) = (x_fit(n) - (y_fit(n)+fit(1))) /
     *              (abs(x_fit(n)))**xmode   
           enddo
         elseif (scale) then
           do n=1,npts
             f(n) = (x_fit(n) - y_fit(n)*fit(1)) /
     *              (abs(x_fit(n)))**xmode  
           enddo 
         endif
       endif
       end
