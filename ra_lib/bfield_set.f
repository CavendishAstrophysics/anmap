*+ bfield_spiral

      subroutine bfield_calc   ( flux, s, th1, th2, Bfield, status)
c     -------------------------------------------------------------
c
c calculate the minimum energy magnetic field
c
C Given:
C    flux 
       real*4     flux
C    line-of-sight depth
       real*4     s
C    angular isze of region
       real*4     th1, th2
C Returned:
C    euipartition magnetic field strength
       real*4     Bfield
C Updated:
C    error status
       integer    status
C
C-
C Local variables
      real*4         B1,B2,B3,B4

      include 'synch_bfield.inc'

      if (status .ne. 0) return
      if (flux.gt.0.0) then
        B1 = ((nu_lo**(0.5-alpha)) - (nu_hi**(0.5-alpha)))/(alpha-0.5)
        B2 = B1*flux*(nu_zero**alpha)*(1.0+k)/f
        B3 = (1.0+z)**(3.0+alpha)
        B4 = B2*B3/(s*th1*th2)
        Bfield = c_zero*(B4**(2.0/7.0))
      else
        Bfield = 0.0
      endif
      call iocmd_err( status, 'bfield_calc', ' ')
      end
C
C
C
C
*+ bfield_set

       subroutine bfield_set(status)
C      -----------------------------
C
C Set parameters for the B-field analysis
C
C Updated:
C  error status
       integer     status
C
C-

       include 'synch_bfield.inc'

C check status on entry
       if (status.ne.0) return
       call io_getr('Filling-factor : ','1.0',f,status)
       call io_getr('Energy-ratio : ','0.0',k,status)
       call io_getr('Observation-frequency (/GHz) : ','1',
     *              nu_zero,status)
       call io_getr('Low-frequency integration limit (/GHz) : ',
     *             '0.01',nu_lo,status)
       call io_getr('High-frequency integration limit (/GHz) : ',
     *             '100',nu_hi,status)
       call io_getr('Spectral-index : ','0.5',alpha,status)
       call io_getr('Redshift : ','0.0',z,status)
       call iocmd_err(status,'EQUIPARTITION-PARAMETERS',' ')

       end
C
C
*+ bfield_spiral

      subroutine bfield_spiral (minirt, map, iuv, 
     *                          flx_lvl, flx_nrm, theta_r, s,
     *                          flux, Bfield, status)
c     ----------------------------------------------------
c
c calculate the minimum energy magnetic field for a spiral galaxy model
c
C Given:
C    mini redtape
       integer  minirt(10)
C    map data
       real*4   map(*)
C    data range
       integer  iuv(4)
C    flux level below which pixels are ignored
       real*4   flx_lvl
C    flux normalization constant
       real*4   flx_nrm
C    disc semi-major axis
       real*4   theta_r
C    line-of-sight depth
       real*4   s
C Returned:
C    flux within window, above flx_lvl (Jansky)
       real*4   flux
C    euipartition magnetic field strength
       real*4   Bfield
C Updated:
C    error status
       integer    status
C
C-
C Local variables
      integer*4      i, j, uv(2)
      equivalence   (uv(1), i)
      equivalence   (uv(2), j)
      real*4         map_data
      real*4         sum,B1,B2,B3,B4

       include '/mrao/include/constants.inc'
       include 'synch_bfield.inc'

      if (status .ne. 0) return

      sum = 0.0
      do j = iuv(3),iuv(4),-1
        do i = iuv(1),iuv(2)
          call iuvval2(minirt,map,uv,map_data,status)
          if (map_data .ge. flx_lvl) sum = sum + map_data
        end do
      end do
      flux = flx_nrm*sum

c B3 is model dependent - thin disc model for spiral
      if (flux.gt.0.0) then
        B1 = (nu_lo**(0.5-alpha)) - (nu_hi**(0.5-alpha))/(alpha-0.5)
        B2 = B1*flux*(nu_zero**alpha)*(1.0+k)/f
        B3 = (1.0+z)**(3.0+alpha)
        B4 = B2*B3/(s*const_pi*(theta_r**2))
        Bfield = c_zero*(B4**(2.0/7.0))
      else
        Bfield = 0.0
      endif
      call iocmd_err( status, 'bfield_spiral', ' ')
      end
C
C
C
C
*+ do_B_spiral

       subroutine do_B_spiral(map_array, results, status)
C      --------------------------------------------------
C
C Determine B-field for a spiral galaxy model
C
C Given:
C   Map data array
       real*4     map_array(*)
C
C Updated:
C   Results string
       character*(*) results
C   Status
       integer    status
C
*-
C
       integer    iuv(4), i, imap, ip_map, lr
       integer    uv_pos_1(2), uv_pos_2(2)
       real*4     flux, theta_r, bfield, s
       real*4     flux_level, x, y
       integer    iopt, minirt(8)
       real*8     usamp,vsamp
       real*4     flxnrm, beam(2), posa
       character  option*1

 

C set status and read in a map
       if (status.ne.0) return
       call map_getmap('Map : ','Default-Map','READ',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call enminirt(minirt,status)
       call ensamp(usamp, vsamp, status)
       call enbeam(flxnrm, beam, posa, status)

C get map region
       do i =1,4
         iuv(i) = minirt(i)
       end do
       call plot_getuv('UV-range : ','*',iuv,status)

C mark the required contour level
       call io_getr('Scale-height (kpc) : ','1.0',s,status)
       call plot_getpos2(minirt,'Mark-Cursor : ',' ',uv_pos_1,status)
       call iuvval2(minirt,map_array(ip_map),uv_pos_1,
     *              flux_level,status)
       call io_wrout('Option       C=mark-Core')
       call io_wrout('             D=mark-Diameter')
       call io_wrout('             R=Repeat-first-point')
       call io_wrout('             Q=quit')
1      call io_getkey('Option [C] : ','C','CDRQ',option,status)
       if (option.eq.'R') then
         call plot_getpos2(minirt,'UV-position : ',' ',uv_pos_1,status)
         iopt = 1
         goto 1
       else if (option.eq.'C') then
         call plot_getpos2(minirt,'UV-position : ',' ',uv_pos_2,status)
         iopt = 2
         x = (uv_pos_1(1)-uv_pos_2(1))*usamp
         y = (uv_pos_1(2)-uv_pos_2(2))*vsamp
       else if (option.eq.'D') then
         call plot_getpos2(minirt,'UV-position : ',' ',uv_pos_1,status)
         iopt = 3
         x = (uv_pos_1(1)-uv_pos_2(1))*usamp/2.0
         y = (uv_pos_1(2)-uv_pos_2(2))*vsamp/2.0
       else
         iopt = 4
         goto 100
       end if
       theta_r = sqrt(x*x+y*y)

C determine field for this region
       call bfield_spiral(map_array(ip_map),iuv,flux_level,
     *                    flxnrm,theta_r,s,flux,bfield,status)

       if (status.ne.0) goto 100
       call chr_chrtoc(Bfield,results,lr)

100    call map_end_alloc(imap,map_array,status)
       call iocmd_err(status,'BFIELD-SPIRAL','Failed')

       end
C
C
*+ do_B_calc

       subroutine do_bfield_calc ( results, status )
C      ---------------------------------------------
C
C Do an equipartition calculation
C
C Updated:
C   Results string
       character*(*) results
C   error status
       integer       status
C-
       real*4  s, th1, th2, flux, Bfield
       integer lr
       call io_getr('Line-of-sight-depth (kpc) : ','1',s,status)
       call io_getr('Angular-size 1 (arcsec) : ','1',th1,status)
       call io_getr('Angular-size 2 (arcsec) : ','1',th2,status)
       call io_getr('Flux (mJy) : ','0.0',flux, status )
       flux = flux/1000.0
       call bfield_calc(flux, s, th1, th2, Bfield, status)
       call chr_chrtoc(Bfield,results,lr)
       call iocmd_err(status,'equipartition_calculation',' ')
       end




