C MULTI-FIT: Program to fit a function to a series of images
C ---------
C
C Multi add takes as input a file name and reads from the redtape a
C list of maps in the standard map catalogue to add together.
C Output is to the map specified in the call to the routine.
C-
C Document origin and update of this routine
C   P. Alexander, MRAO, Cambridge, 06/07/95
C
*-

C define work array
       integer       nm, nb
       integer        msize, bsize
       parameter    (msize = 256*256)
       parameter    (bsize=1024)
       parameter    (nm=20)
       parameter    (nb=3)
       real*4        map_array( nm*msize + nb*bsize )
C error status
       integer       status

C number of maps to fit/return
       integer       max_maps
       parameter    (max_maps = 10)
C pointers and map identifiers
       integer       nin, nout, imap(max_maps), ip_map(max_maps),
     *               imapo(max_maps), ip_mapo(max_maps)
C control data
       integer       model, nc
       real*4        blank(max_maps), gates(max_maps),
     *               xvals(max_maps), cdata(20)
C mini redtape for map size etc.
       integer       minirt(8), ix, iy
C counters etc.
       integer       n, nn, im
       real*4        x_data(max_maps), y_data(max_maps),
     *               r_data(max_maps), xx

C perform standard initialization
       call anm_start( 0,nm,msize,nb,bsize,status )

C read input maps:
       call io_geti('Number of input-maps : ','0',nin,status)
       do n=1,nin
         call map_getmap('Map : ','Default_Map','READ',imap(n),status)
         call map_alloc_in(imap(n),'DIRECT',map_array,ip_map(n),status)
         call enminirt(minirt,status)
         if (n.eq.1) then
            ix = minirt(5)
            iy = minirt(6)
         else
            if (minirt(5).ne.ix .or. minirt(6).ne.iy) then
               status = 100
            endif
         endif
         call ennull(blank(n),status)
         call io_getr('Xvalue: ','0.0',xvals(n),status)
         call io_getr('Gate: ','0.0',gates(n),status)
       enddo
       call io_geti('Number of output-maps : ','0',nout,status)
       do n=1,nout
         call map_alloc_out(0,0,'DIRECT',imapo(n),ip_mapo(n),status)
       enddo
       call io_geti('Model type: ','1',model,status)
       call io_geti('Number control data : ','0',nc,status)
       do n=1,nc
          call io_getr('Control: ','0.0',cdata(n),status)
       enddo
       if (status.ne.0) goto 999

C do all the hard work of modelling the data
       do n=0,ix*iy-1
         nn = 0
         do im=1,nin
           xx = map_array(ip_map(im)+n)
           if (xx.ne.blank(im) .and. abs(xx).gt.gates(im) ) then
               nn = nn + 1
               x_data(nn) = xvals(im)
               y_data(nn) = xx
           end if
         end do
         if (nn.ge.nout) then
             call find_mfit( nn, x_data, y_data, nc, cdata, model,
     *                       r_data, status )
             do im=1,nout
                 map_array(ip_mapo(im)+n) = r_data(im)
             end do
         else
             do im=1,nout
                 map_array(ip_mapo(n)+n) = blank(1)
             end do
         end if
         if (status.ne.0) goto 999
       end do

999    continue
       call adredt('CREATED','MODEL',status)
       do n=1,nin
         if (imap(n).ne.0) then
            call map_end_alloc(imap(n),map_array,status)
         endif
       enddo
       do n=1,nout
         if (imapo(n).ne.0) then
            call map_end_alloc(imapo(n),map_array,status)
         endif
       enddo

C check status value and report an error to the user
       call cmd_err(status,'MODEL','Failed ')

C finally perform standard shut-down
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
       res(1) = res(1)

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
