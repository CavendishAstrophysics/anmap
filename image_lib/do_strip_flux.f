C
C
*+ do_strip_flux

       subroutine do_strip_flux(map_array,status)
C      ------------------------------------------
C
C Add flux in strips and plot result
C
C Given:
C   map data
       real*4        map_array(*)
C Returned:
C   error status word
       integer       status
C
C Flux is added in strips of uniform width across the map: the result
C is displayed as a table and also as a plot of flux in the strip
C against arcseconds from a specified central position.
*-

       include '../include/anmap_sys_pars.inc'
       include '../include/error_file_full.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_constants.inc'

C Local variables:
C   maximum number of strips to analyse
       integer    max_strips
       parameter (max_strips= 100)

C   mini redtape
       integer    minirt(8)
C   used to hold the date (day, month, year)
       integer    idate(3)
C   input map and data pointer, output unit
       integer    imap, ip_map, iout
C   counters
       integer    n, i, nu, nv
C   results file unit
       integer    ires
C   position in arcsec of first region centre, and
C   position in arcsec of region centre
       real*4     u0, v0, u, v
C   full uv_range of map, list of regions (uv coordinates) to analyse
       integer    uv_full(4), uv_list(4,max_strips)
C   number of strips (entries) in uv_list array, central pixel of
C   analysed data, width of strips in u/v, number of strips in u/v,
C   start pixel in u/v when making a regular grid
       integer    number_strips,
     *            u_strip, v_strip, n_u_strip, n_v_strip,
     *            start_u, start_v
C  size of beam in pixels
       integer    beam_pixels
C  map sampling in U and V
       real*8     usamp, vsamp
C  number of points foudn in each region
       integer    npts(max_strips)
C  R*4 version of beam_pixels,  noise on map (etimate by user)
       real*4     beam_size, noise, pnoise
C  flux in each region, error in each region
       real*4     flux(max_strips), error(max_strips)
C  gate to use in calculating the flux
       real*4     gate
C  error_estimate is set true if user requires error calculation
       logical    error_estimate
C  flag to indicate polarization correction is wanted
       logical    poln_correction
C  local function
       logical    uv_within
C  data type (needed as argument to enredt)
       integer    data_type
C  beam and flux-normalization from redtape
       real*4     local_beam(2), local_beam_pa, flxnrm
C  text variable for results
       character  text*80
C  enquired parameters
       integer    en_poln
       real*4     en_freq
       character  en_unit*16, en_name*16

C local common block to retain uv_list between calls
       integer                      old_number_strips
       common /local_strip_flux/    uv_list, old_number_strips

C check status
       if (status.ne.0) return

C find output unit number
       call io_enqout(iout)

C read in the map
       poln_correction = .false.
       call map_getmap('Map : ','Default-Map','READ',imap,status)
       call enminirt(minirt,status)
       call ensamp(usamp,vsamp,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call io_getr('Gate (map units) : ','0.0',gate,status)
       if (io_yesno('Perform noise estimate : ','no',status)) then
         error_estimate = .true.
         call io_geti('Size of beam in pixels : ','1',beam_pixels,
     *             status)
         beam_size = beam_pixels
         call io_getr('Noise (map units) : ','0.0',noise,status)
         poln_correction =
     *       io_yesno('Perform polarization correction : ','no',status)
         if (poln_correction) then
           pnoise = noise/sqrt(2.0)
           call io_getr('Noise on Q/U maps : ','*',pnoise,status)
         endif
       else
         error_estimate = .false.
       end if
       if (status.ne.0) goto 999

C check flux-normalization value from the redtape
       call enbeam(flxnrm,local_beam,local_beam_pa,status)
       if (flxnrm .lt. 1.0E-30) then
         call io_wrout(' ')
         call io_wrout('.. Flux Normalization constant <= 0 in redtape')
         call io_getr('Flux-Normalization (e.g. CLEAN-BEAM volume) : ',
     *             '1.0',flxnrm,status)
C .. store this corrected value in the redtape
         call stbeam(flxnrm,local_beam,local_beam_pa,status)
         call redt_dump(imap,status)
       end if
       if (status.ne.0) goto 999
C read the number of strips
C                    Options :    = 0    --   regular grid
C                                 = n    --   input "n" regions
C                                 = -1   --   use current regions
C
       number_strips = 0
       call io_wrout(
     * '.. Give No. of regions: or 0=regular-grid; -1=existing-regions')

       call io_geti('Number of strips (0 or C/R = regular grid) : ',
     *           ' ',number_strips,status)
       if (status.ne.0) goto 999

C decode number_strips
       if (number_strips.ge.0) then

         if (number_strips.ge.max_strips) then
           call cmd_wrerr('STRIP-FLUX',
     *                     'Maximum number of strips exceeded')
           status = ill_input
           goto 999
         end if

C .. initialise strips array
         do n=1,max_strips
           do i=1,4
             uv_list(i,n) = 0
           end do
         end do
       end if

       if (number_strips.eq.-1) then

C .. use existing uv-regions
         call io_wrout(' ')
         call io_wrout('.. Using existing regions')
         number_strips = old_number_strips

       else if (number_strips.eq.0) then

C .. regular grid option
         call io_wrout(' ')
         call io_wrout('.. Using a regular grid')
         call enredt(uv_full,data_type,status)
         call io_geti('Width of strips in u : ','1',u_strip,status)
         call io_geti('...                v : ','1',v_strip,status)
         call io_geti('Number of strips in u : ','1',n_u_strip,status)
         call io_geti('...                 v : ','1',n_v_strip,status)
         call io_geti('Start pixel in u : ','0',start_u,status)
         call io_geti('...            v : ','0',start_v,status)

C .. define regions using this information
         call enredt(uv_full,data_type,status)
         if (status.ne.0) goto 999

         if (n_u_strip*n_v_strip .gt. max_strips) then
           call cmd_wrerr('STRIP-FLUX',
     *                     'Maximum number of strips exceeded')
           status = ill_input
           goto 999
         end if

         do nu=1,n_u_strip
           do nv=1,n_v_strip
             number_strips = number_strips + 1
             uv_list(1,number_strips) = start_u+(nu-1)*u_strip
             uv_list(2,number_strips) = start_u+(nu-1)*u_strip
     *                                  + u_strip-1
             uv_list(4,number_strips) = start_v+(nv-1)*v_strip
             uv_list(3,number_strips) = start_v+(nv-1)*v_strip
     *                                  + v_strip-1
             if (.not.uv_within(uv_list(1,number_strips),uv_full)) then
               write(iout,'(1x,''***(STRIP-FLUX) region '',i2,
     *                  '' outside map -- ignore this region''/
     *               1x,''*** Map range = '',4i7,''  requested = '',
     *                  4i7)') number_strips, uv_full,
     *                         (uv_list(i,number_strips), i=1,4)
               number_strips = number_strips - 1
             end if
           end do
         end do

       else

C input "number_strips" uv-strips
         call io_wrout(' ')
         call io_wrout('.. Input uv-coordinates for each strip')
         do n=1,number_strips
           write (text,10) n
  10       format('Strip ',I2,' : ')
           call enredt(uv_list(1,n),data_type,status)
           call plot_getuv(text(1:12),'*',uv_list(1,n),status)
         end do

       end if

C save the number of strips selected
       if (status.eq.0) old_number_strips = number_strips

C now loop through this list and determine the flux in each region
       do n=1,number_strips
         if (poln_correction) then
           call image_addpoln(minirt,map_array(ip_map),pnoise,
     *                        uv_list(1,n),gate,flux(n),npts(n),status)
         else
           call image_addflux(minirt,map_array(ip_map),
     *                        uv_list(1,n),gate,flux(n),npts(n),status)
         endif
         flux(n) = flux(n)*flxnrm
         if (error_estimate) then
           error(n) = noise*sqrt(float(npts(n))/beam_size)
         end if
       end do

C open the results file
       call io_opefil(ires,general_results_file,'WRITE',0,status)
       if (status.ne.0) then
         call cmd_err(status,'STRIP-FLUX',
     *                 'Error accessing results file')
         goto 999
       end if

C title to the output device
       text = ' '
       call io_getstr('Title: ',' ',text,status)
       call util_enqdat(idate)
       write (iout,'(1x/1x,''STRIP FLUX output on '',
     *               i2,'':'',i2,'':'',i4/
     *               1x/1x,a/1x)')idate,text

C titles to the results file
       write (ires,*)'%ndata ',number_strips
       if (error_estimate) then
         write (ires,*)'%ncols 8'
       else
         write (ires,*)'%ncols 7'
       endif
       write (ires,*)'%file-title STRIP FLUX output on ',idate
       write (ires,*)'%title ',text(1:chr_lenb(text))
       write (ires,*)'%title-1 U1'
       write (ires,*)'%title-2 U2'
       write (ires,*)'%title-3 V1'
       write (ires,*)'%title-4 V2'
       write (ires,*)'%title-5 U-coordinte (arcsec)'
       write (ires,*)'%title-6 V-coordinte (arcsec)'
       call entype(en_freq,en_poln,en_name,en_unit,status)
       write (ires,*)'%title-7 Integrated ',
     *               en_name(1:chr_lenb(en_name)),' (',
     *               en_unit(1:chr_lenb(en_unit)),')'
       write (ires,*)'%title-8 Error ',
     *               en_name(1:chr_lenb(en_name)),' (',
     *               en_unit(1:chr_lenb(en_unit)),')'

C output comment line to the output device
       write (iout,20)number_strips
20     format(1x/1x,'Results for ',i2,' regions:'/1x)

C write out the results of the integration
       do n=1,number_strips
         if (n.eq.1) then
           u0 = 0.5*usamp*(uv_list(1,n)+uv_list(2,n))
           v0 = 0.5*vsamp*(uv_list(3,n)+uv_list(4,n))
           u = 0.0
           v = 0.0
         else
           u = 0.5*usamp*(uv_list(1,n)+uv_list(2,n)) - u0
           v = 0.5*vsamp*(uv_list(3,n)+uv_list(4,n)) - v0
         end if
         if (error_estimate) then
           write (iout,30) (uv_list(i,n), i=1,4), flux(n), error(n)
           write (ires,31) (uv_list(i,n), i=1,4), u, v,
     *                     flux(n), error(n)
         else
           write (iout,40) (uv_list(i,n), i=1,4), flux(n)
           write (ires,31) (uv_list(i,n), i=1,4), u, v,
     *                     flux(n)
         end if
       end do
  30   format(4i6,'  :  ',g14.4,' +/- ',g14.4)
  31   format(4i6,4g14.4)
  40   format(1x,4i6,'  :  ',g14.4)
       write (iout,*) ' '

999    call map_end_alloc(imap,map_array,status)
       call cmd_err(status,'STRIP-FLUX','Failed')
       close (ires)
       call io_setout(terminal_out)

       end




