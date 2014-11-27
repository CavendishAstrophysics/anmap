C Multi-slice-fit:  Fit a function to each column of an image and
C                   output a spectral file.
C
C
C Document origin and update of this routine
C   P. Alexander, MRAO, Cambridge, 09/04/92
C
*-
       include '/mrao/include/chrlib_functions.inc'
       include '../include/anmap_sys_pars.inc'

C define work array
       integer       nm, nb
       integer       msize, bsize
       parameter    (msize = 256*256)
       parameter    (bsize=1024)
       parameter    (nm=4)
       parameter    (nb=1)
       real*4        map_array( nm*msize + nb*bsize )
C error status
       integer       status
C information for image
       integer       imap, ip_map, minirt(8)
C X and Y data to fit, control data and results
       real*4        x(bsize), y(bsize), cd(10), res(10)
       integer       ndata, ncols, nc, istat
C model type
       integer       model
C counters etc
       integer       n, i, j, iunit
       character     string*80

C perform standard initialization
       status = 0
       call anm_start( 0,nm,msize,nb,bsize,status )

C read map
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call enminirt(minirt,status)
       if (status.ne.0) goto 999

C get model type
       print *,'Available model types to fit are:'
       print *,' 1: y = r(1) exp( -x/r(2) )'
       print *,' 2: y = r(1) [ 1 - exp( -x/r(2) ) ]'
       print *,' 3: y = r(1) exp( -r(2) [cd(1) x**2 - cd(2) x**3] )'
       print *,' 4: y = r(1) + r(2) x'
       print *,' 5: y = r(1) + r(2) [ cd(1)x + cd(2)x**2 + cd(3)x**3 ]'
       call io_geti('Model type (1-5) : ','1',model,status)
       if (model.lt.1) model = 1
       if (model.gt.5) model = 5
       ncols = 4
       ndata = minirt(5)

C get user data for giving "X" value for each column of data
       do n=1,minirt(6)
         x(n) = n
       enddo
       string = ' '
       write(string,10) minirt(6)
10     format('Input X-axis values (',I3,' values) : ')
       call io_getnr(string(1:chr_lenb(string)+1),' ',
     *               x,minirt(6),status)
C read control data if required
       if (model.eq.3) then
         nc = 2
         call io_getnr('Supply control data (cd(1),cd(2)) : ',' ',
     *                 cd,nc,status)
       elseif (model.eq.5) then
         nc = 3
         call io_getnr('Supply control data (cd(1),cd(2),cd(3)) : ',' ',
     *                 cd,nc,status)
       endif

C open output file -- use anmap results file
       call io_opefil(iunit,general_results_file,'WRITE',0,status)
       if (status.ne.0) goto 999
       write(iunit,*)'%ndata ',ndata
       write(iunit,*)'%ncols ',ncols
       write(iunit,*)'%title Results produced by slicefit'
       write(iunit,*)'%ce ',imap
       write(iunit,*)'%model ',model

C loop through map and do fit
       do i=1,minirt(5)
         do j=1,minirt(6)
           n= ip_map + (j-1)*minirt(5) + i - 1
           y(j) = map_array(n)
         enddo
         istat = 0
         call find_mfit( minirt(6), x, y, nc, cd, model, res, istat )
         write(iunit,*) i, (res(j), j=1,ncols-1)
       enddo
       close(iunit)

C check status value and close
999    call cmd_err(status,'multi-slicefit','Failed ')
       call map_end_alloc(imap,map_array,status)
       call anm_end( status )

       end
C
C
*+ find_mfit

       subroutine find_mfit( nn, xd, yd, nc, cd, model, res, status )
C      --------------------------------------------------------------
C
C Perform the fitting
C
C Given:
C  number of data points
       integer    nn
C  input "x" and "y" data
       real*4     xd(*), yd(*)
C  number of user-specified constants
       integer    nc
C  user-supplied constants
       real*4     cd(*)
C  type of fitting model to use
       integer    model
C Returned:
C  results of the fit
       real*4     res(*)
C Updated:
C  return code
       integer    status
C
C Perform one of number of different fitting models:
C
C model   Fit-decription
C -------------------------------------------------------------------
C   1     y = res(1) exp( -x/res(2) )
C   2     y = res(1) [ 1 - exp( -x/res(2) ) ]
C   3     y = res(1) exp( -res(2) [cd(1) x**2 - cd(2) x**3] )
C   4     y = res(1) + res(2) x
C   5     y = res(1) + res(2) [ cd(1)x + cd(2)x**2 + cd(3)x**3 ]
C -------------------------------------------------------------------
C
C The fit is performed using a least-squares minimization using the
C NAG routine E04FDF.
C
C P.Alexander MRAO, 06/04/92
*-

C local variables
       integer     len_work, len_iwork
       parameter  (len_work=1000, len_iwork=100)
       real*8      xx(5), fsumsq, work(len_work)
       integer     iwork(len_iwork), nvar, istat

C counters etc.
       integer     n

C data passed in common to LSFUN1
       real*8      x(50), y(50), c(10)
       integer     itype, ndata, nconst
       common /local_fitr8/ x, y, c
       common /local_fiti4/ itype, ndata, nconst

       if (status.ne.0) return

C set itype
       if (model.le.0) then
         itype = 1
       else if (model.gt.5) then
         itype = 3
       else
         itype = model
       end if
C copy data
       ndata = nn
       nconst = nc
       do n=1,nn
         x(n) = xd(n)
         y(n) = yd(n)
       end do
       do n=1,nc
         c(n) = cd(n)
       end do

C call minimization routine
       nvar = 2
       istat = 1
       xx(1) = yd(1)
       xx(2) = xd(2) - xd(1)
       call e04fdf( ndata, nvar, xx, fsumsq,
     *              iwork, len_iwork, work, len_work, istat )
       do n=1,nvar
         res(n) = xx(n)
       end do
       res(nvar+1) = fsumsq
       end
C
C
       subroutine lsfun1( m, n, xx, f )
       integer    n, m, i
       real*8     xx(n), f(m)
C data passed in common to LSFUN1
       real*8      x(50), y(50), c(10)
       integer     itype, ndata, nconst
       common /local_fitr8/ x, y, c
       common /local_fiti4/ itype, ndata, nconst

       if (itype.eq.1) then
         do i = 1,ndata
           f(i) = xx(1)*exp(-x(i)/xx(2)) - y(i)
         end do
       elseif (itype.eq.2) then
         do i = 1,ndata
           f(i) = xx(1)*(1.0D+0 - exp(-x(i)/xx(2))) - y(i)
         end do
       elseif (itype.eq.3) then
         do i = 1,ndata
           f(i) = xx(1)*exp(-xx(2)*(c(1)*(x(i)**2)-c(2)*(x(i)**3) ))
     *            - y(i)
         end do
       elseif (itype.eq.4) then
         do i = 1,ndata
           f(i) = xx(1) + xx(2)*x(i) - y(i)
         end do
       elseif (itype.eq.5) then
         do i = 1,ndata
           f(i) = xx(1) +
     *            xx(2)*(c(1)*x(i) + c(2)*(x(i)**2) + c(3)*(x(i)**3))
     *            - y(i)
         end do
       endif
       end




