C REINTERPOLATE: Re-interpolate a spectrum onto a sample defined by another
C
       implicit    NONE
C
C The input spectrum is interpolated onto the sampling of a second spectrum, the
C model. The output has exactly the same shape as the model, when data are not
C present in the input then zeros are placed in the output. 
C
C Multi column data are handled.  The x-column must be specifed for the 
C re-interpolation to proceed, the default is to assume column 1.  Where
C the output data lies in the range of the input data linear interpolation
C between adjacent data values is used.
C
C Parameters:
C    outfile   ==  output file name
C    infile    ==  input file name
C    model     ==  model file against which re-interpolation is done
C    xc        ==  X-column in the infile for use in reinterpolation
C    xcm       ==  X-column in the model file
C    title     ==  new title for output file
C    comment   ==  user defined comment text
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
     *            y(max_ndata), y_out(max_ndata)
C   local variables used to hold information and do the interpolation
       real*4     xmin, xmax
C   file and directory name(s) and strings
       character  file*100, dir*100, string*80
C   x-column
       integer    xc, xcm
C   logical to test for setting of input parameters
       logical    test
C   number of columns
       integer    ncols, ncols_model
C   counters
       integer    n, nc, n1, n2


C setup for input
       call io_initio
       call cmd_extpars( 'SPECTRUM', dir, status )
       call cmd_getiline( 'reinterpolate',
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
C .. read the x-column from the model and input files
       call spec_get_data( id_from, xc, x, nd_from, status )
       call spec_get_data( id_model, xcm, x_model, nd_model, status )
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
C .. find number of columns
       call spec_hd_enqi( id_from, 'ncols', ncols, status )
       call spec_hd_enqi( id_model, 'ncols', ncols_model, status )
C loop for each column in the model
       if (status.ne.0) goto 999
       do nc=1,ncols_model
         if (nc.eq.xc) then
           call spec_get_data( id_model, xcm, y_out, nd_model, status )
         elseif (nc.gt.ncols) then
           call spec_get_data( id_model, xcm, y_out, nd_model, status )
           do n=1,nd_model
             y_out(n) = 0.0
           enddo
         elseif (nc.le.ncols) then
           call spec_get_data( id_from, nc, y, nd_from, status)
           n1 = 1
           do n=1,nd_model
             if (x_model(n).lt.xmin .or. x_model(n).gt.xmax) then
               y_out(n) = 0.0
             else
               do while (x(n1).lt.x_model(n))
                 n1 = n1 + 1
               enddo
               n1 = n1 - 1
               n2 = n1
               do while (x(n2).lt.x_model(n))
                 n2 = n2 + 1
               enddo
               y_out(n) = y(n1) +
     *              (x_model(n)-x(n1))*(y(n2)-y(n1))/(x(n2)-x(n1))
             endif
           enddo
         endif
         if (status.ne.0) goto 999

C .. copy this column to the output
         call spec_put_data( id_to, nc, y_out, nd_model, status)

       end do

C add history item
       string = ' '
       write(string,'(A,2I2)')
     *       'REINTERPOLATE: xc/xcm = ',xc,xcm
       call spec_hd_set( id_to, 'history',
     *                   string(1:chr_lenb(string)),
     *                   status )

C deallocate files
999    continue
       call spec_deallocate( id_from, status )
       call spec_deallocate( id_model, status )
       call spec_deallocate( id_to, status )

       end
